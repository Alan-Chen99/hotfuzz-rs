use bumpalo::{
    collections::{string::String, vec::Vec},
    vec, Bump,
};
use emacs::{defun, Env, Result, Value};
use itertools::{chain, izip};
use rayon::prelude::*;
use scopeguard::guard;
use second_stack::buffer;
use std::{cell::RefCell, cmp::min, iter::repeat};
use std::{cmp, os, ptr};

emacs::plugin_is_GPL_compatible!();

mod e {
    use emacs::use_symbols;
    use_symbols! {
        nil
        t
        encode_coding_string
        no_conversion
    }
}

type Cost = i32;

#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

thread_local! {
    pub static BUMP: RefCell<Bump> = RefCell::new(Bump::with_capacity(10000));
}

fn is_match<const DOWNCASE: bool>(needle: &[u8], haystack: &[u8]) -> bool {
    let mut haystack_iter = haystack.iter();

    for &needle_char in needle {
        match haystack_iter.position(|&hay| {
            needle_char
                == if DOWNCASE {
                    hay.to_ascii_lowercase()
                } else {
                    hay
                }
        }) {
            Some(_) => (),
            None => return false,
        };
    }
    true
}

fn downcase_byte_str(s: &mut [u8]) {
    for x in s.iter_mut() {
        *x = x.to_ascii_lowercase()
    }
}

fn char_bonus(prev: u8, ch: u8) -> i32 {
    let word_bonus = 80;

    match ch {
        b'A'..=b'Z' if prev.is_ascii_lowercase() => word_bonus,
        b'A'..=b'Z' | b'a'..=b'z' => match prev {
            b'/' => 90,
            b'.' => 60,
            b'-' | b'_' | b' ' => word_bonus,
            _ => 0,
        },
        _ => 0,
    }
}

fn get_cost<const DOWNCASE: bool>(needle: &[u8], haystack: &mut [u8]) -> Cost {
    if DOWNCASE {
        downcase_byte_str(haystack);
    }
    let nl = needle.len();
    if nl == 0 {
        return 0;
    }
    buffer(repeat(10000 as Cost).take(nl), |c| {
        buffer(repeat(10000 as Cost).take(nl), |d| {
            // d: cost if ch does not match hay
            // c: cost either way
            // chunk penalty, incured at end of matched chunk
            let g: Cost = 100;
            let s_init = chain!([0], repeat(g));
            let mut prev_hay = b'/';
            for (&hay, s_init) in izip!(haystack.iter(), s_init) {
                // s = c[i-1][j-1]
                let mut s: i32 = s_init;
                let bonus = char_bonus(prev_hay, hay);

                for (&ch, c, d) in izip!(needle, c.iter_mut(), d.iter_mut()) {
                    let oldc = *c;
                    *d = min(*d, *c + g);
                    *c = if hay == ch { min(*d, s - bonus) } else { *d };
                    s = oldc;
                }
                prev_hay = hay;
            }
            min(*d.last().unwrap(), *c.last().unwrap() + g) + 5 * (haystack.len() as Cost)
        })
    })
}

fn calc_all_score<'b, const DOWNCASE: bool>(
    b: &'b Bump,
    str: String,
    cands: Vec<String>,
) -> Vec<'b, Option<Cost>> {
    let mut ans: Vec<Option<Cost>> = vec![in b; None; cands.len()];
    let mut str = str.into_bytes();
    if DOWNCASE {
        downcase_byte_str(&mut str);
    }
    let needle = str.as_slice();
    let mut cands = Vec::from_iter_in(
        cands.into_iter().map(|x| x.into_bytes().into_boxed_slice()),
        b,
    );
    let chunk_size = 100;
    cands
        .par_chunks_mut(chunk_size)
        .zip(ans.par_chunks_mut(chunk_size))
        .for_each(|(cand, ans)| {
            for (haystack, ans) in izip!(cand, ans) {
                if is_match::<DOWNCASE>(needle, haystack) {
                    *ans = Some(get_cost::<DOWNCASE>(needle, haystack));
                }
            }
        });
    ans
}

fn filter_cands_impl<'a>(b: &Bump, str: Value<'a>, mut cands: Value<'a>) -> Result<Value<'a>> {
    let env = str.env;
    let str = try_into_string(str, b)?;

    let mut cands_val = vec![in b];
    let mut cands_str = vec![in b];
    while cands.is_not_nil() {
        let v = cands.car()?;
        cands_val.push(v);
        cands_str.push(try_into_string(v, b)?);
        cands = cands.cdr()?;
    }
    let costs = calc_all_score::<true>(b, str, cands_str);
    let mut costs_filtered = Vec::from_iter_in(
        costs
            .into_iter()
            .enumerate()
            .filter_map(|(i, x)| x.map(|x| (i, x))),
        b,
    );
    costs_filtered.sort_unstable_by_key(|(_, x)| cmp::Reverse(*x));

    let mut ans = e::nil.bind(env);
    for (idx, _) in costs_filtered {
        ans = env.cons(cands_val[idx], ans)?;
    }
    Ok(ans)
}

#[defun(name = "filter")]
fn filter_cands<'a>(str: Value<'a>, cands: Value<'a>) -> Result<Value<'a>> {
    BUMP.with(|b| {
        let b = guard(b.borrow_mut(), |mut b| b.reset());
        filter_cands_impl(&b, str, cands)
    })
}

fn try_into_string<'b>(v: Value, b: &'b Bump) -> Result<String<'b>> {
    match try_into_string_utf8(v, b) {
        Ok(s) => Ok(s),
        Err(_) => {
            let env = v.env;
            let v = e::encode_coding_string
                .call(env, [v, e::no_conversion.bind(env), e::t.bind(env)])?;
            try_into_string_utf8(v, b)
        }
    }
}

// slightly modified emacs::Env::string_bytes
fn try_into_string_utf8<'b>(v: Value, b: &'b Bump) -> Result<String<'b>> {
    let env = v.env;
    let env_raw = env.raw();
    let mut len: isize = 0;
    unsafe {
        let copy_string_contents = (*env_raw).copy_string_contents.unwrap();
        let ok: bool = env.handle_exit(copy_string_contents(
            env_raw,
            v.raw(),
            ptr::null_mut(),
            &mut len,
        ))?;

        // Technically this shouldn't happen, and the return type of copy_string_contents
        // should be void, not bool.
        if !ok {
            panic!("Emacs failed to give string's length but did not raise a signal");
        }
        let mut bytes: Vec<u8> = Vec::with_capacity_in(len as usize, b);

        let ok: bool = env.handle_exit(copy_string_contents(
            env_raw,
            v.raw(),
            bytes.as_mut_ptr() as *mut os::raw::c_char,
            &mut len,
        ))?;
        // Technically this shouldn't happen, and the return type of copy_string_contents
        // should be void, not bool.
        if !ok {
            panic!("Emacs failed to copy string but did not raise a signal");
        }
        bytes.set_len((len - 1) as usize);
        Ok(String::from_utf8_unchecked(bytes))
    }
}
