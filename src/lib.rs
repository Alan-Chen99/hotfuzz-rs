use bumpalo::{
    // boxed::Box,
    collections::{string::String, vec::Vec},
    vec,
    Bump,
};
use emacs::{defun, Env, Result, Value};
use itertools::izip;
use rayon::prelude::*;
use scopeguard::guard;
use std::cell::RefCell;
use std::{cmp, os, ptr};

emacs::plugin_is_GPL_compatible!();

mod e {
    use emacs::use_symbols;
    use_symbols! {
        nil
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

fn calc_all_score<'b, const DOWNCASE: bool>(
    b: &'b Bump,
    str: String,
    cands: Vec<String>,
) -> Vec<'b, Option<Cost>> {
    let mut ans: Vec<Option<Cost>> = vec![in b; None; cands.len()];
    let mut str = str.into_bytes();
    if DOWNCASE {
        str.iter_mut().for_each(|x| *x = x.to_ascii_lowercase());
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
                    *ans = Some(haystack.len() as i32)
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

// slightly modified emacs::Env::string_bytes
fn try_into_string<'b>(v: Value, b: &'b Bump) -> Result<String<'b>> {
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
