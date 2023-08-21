mod bump_utils;
use bump_utils::{parse_to_string, BoxIntoIter};

use bumpalo::{boxed::Box, collections::vec::Vec, vec, Bump};
use emacs::{defun, Env, Result, Value};
use itertools::{chain, izip};
use scopeguard::guard;
use std::cmp;
use std::{cell::Cell, os, ptr};
use std::{cmp::min, iter::repeat};

emacs::plugin_is_GPL_compatible!();

mod e {
    use emacs::use_symbols;
    use_symbols! {
        nil
        t
        encode_coding_string
        no_conversion
        symbol_name
    }
}

type Cost = i32;

#[emacs::module]
#[allow(clippy::unnecessary_wraps)]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

thread_local! {
    static BUMP: Cell<Option<Bump>> = Cell::new(None);
}

fn with_bump<T>(callback: impl FnOnce(&mut Bump) -> T) -> T {
    BUMP.with(|cell| {
        let b = cell.take().unwrap_or_else(Bump::new);
        let mut b = guard(b, |mut b| {
            b.reset();
            cell.set(Some(b));
        });
        callback(&mut b)
    })
}

#[defun]
fn free_mem() -> Result<usize> {
    BUMP.with(|cell| Ok(cell.take().map_or(0, |b| b.allocated_bytes())))
}

struct Config {
    downcase_needle: bool,
    downcase_haystack: bool,
    chunk_size: usize,
    num_threads: usize,
}

#[inline]
fn is_match<const DOWNCASE_HAY: bool>(needle: &[char], haystack: &str) -> bool {
    let mut haystack_iter = haystack.chars();

    for &needle_char in needle {
        match haystack_iter.position(|hay| {
            needle_char
                == if DOWNCASE_HAY {
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

fn char_bonus(prev: char, ch: char) -> Cost {
    let word_bonus = 80;

    match ch {
        'A'..='Z' if prev.is_ascii_lowercase() => word_bonus,
        'A'..='Z' | 'a'..='z' => match prev {
            '/' => 90,
            '.' => 60,
            '-' | '_' | ' ' => word_bonus,
            _ => 0,
        },
        _ => 0,
    }
}

fn get_cost(bump: &Bump, needle: &[char], mut haystack: Box<str>, cf: &Config) -> Cost {
    let nl = needle.len();
    if nl == 0 {
        return 0;
    }
    if cf.downcase_haystack {
        haystack.make_ascii_lowercase();
    }
    // cost if ch does not match hay
    let mut d = Vec::from_iter_in(repeat(10000 as Cost).take(nl), bump);
    // cost either way
    let mut c = Vec::from_iter_in(repeat(10000 as Cost).take(nl), bump);
    // chunk penalty, incured at end of matched chunk
    let g: Cost = 100;
    let s_init = chain!([0], repeat(g));
    let mut prev_hay = '/';
    for (hay, s_init) in izip!(haystack.chars(), s_init) {
        // s = c[i-1][j-1]
        let mut s: Cost = s_init;
        let bonus = char_bonus(prev_hay, hay);

        for (&ch, c, d) in izip!(needle, c.iter_mut(), d.iter_mut()) {
            let oldc = *c;
            *d = min(*d, *c + g);
            *c = if hay == ch { min(*d, s - bonus) } else { *d };
            s = oldc;
        }
        prev_hay = hay;
    }
    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    let ans = min(*d.last().unwrap(), *c.last().unwrap() + g) + 5 * (haystack.len() as Cost);
    ans
}

fn filter_impl<'a>(
    bump: &Bump,
    cf: &Config,
    needle: Value,
    mut cands: Value<'a>,
) -> Result<Value<'a>> {
    let env = cands.env;

    let mut needle = parse_to_string(bump, try_hard_into_bytes(bump, needle)?.1.into());
    if needle.is_empty() {
        return Ok(e::nil.bind(env));
    }
    if cf.downcase_needle {
        needle.make_ascii_lowercase();
    }
    let needle = Vec::from_iter_in(needle.chars(), bump).into_boxed_slice();
    let mut cands_vec: Vec<Value> = vec![in bump];
    while cands.is_not_nil() {
        let v: Value = cands.car()?;
        cands_vec.push(v);
        cands = cands.cdr()?;
    }
    let mut costs: Vec<Option<Cost>> = vec![in bump; None; cands_vec.len()];
    rayon::ThreadPoolBuilder::new()
        .num_threads(cf.num_threads)
        .build()?
        .in_place_scope(|s| -> Result<_> {
            for (chunk, ans) in izip!(
                cands_vec.chunks_mut(cf.chunk_size),
                costs.chunks_mut(cf.chunk_size)
            ) {
                let mut cands_str = Vec::with_capacity_in(cf.chunk_size, bump);
                for v in chunk {
                    let parsed = try_hard_into_bytes(bump, *v)?;
                    *v = parsed.0;
                    cands_str.push(parsed.1.into_boxed_slice());
                }
                let cands_str = cands_str.into_boxed_slice();
                s.spawn(|_| {
                    let mut bump = Bump::new();
                    assert!(cands_str.len() == ans.len());
                    for (haystack, ans) in izip!(BoxIntoIter::from(cands_str), ans) {
                        let haystack = parse_to_string(&bump, Box::into_inner(haystack));
                        let matched = if cf.downcase_haystack {
                            is_match::<true>(&needle, &haystack)
                        } else {
                            is_match::<false>(&needle, &haystack)
                        };
                        if matched {
                            *ans = Some(get_cost(&bump, &needle, haystack, cf));
                        } else {
                            drop(haystack);
                        }
                        bump.reset();
                    }
                });
            }
            Ok(())
        })?;
    let mut cands_filtered = Vec::from_iter_in(
        izip!(cands_vec, costs).filter_map(|(i, x)| x.map(|x| (i, x))),
        bump,
    );
    cands_filtered.sort_unstable_by_key(|(_, x)| cmp::Reverse(*x));

    let mut ans = e::nil.bind(env);
    for (v, _) in cands_filtered {
        ans = env.cons(v, ans)?;
    }
    Ok(ans)
}

#[defun(name = "filter")]
fn filter_cands<'a>(needle: Value, cands: Value<'a>) -> Result<Value<'a>> {
    with_bump(|bump| {
        filter_impl(
            bump,
            &Config {
                downcase_needle: true,
                downcase_haystack: true,
                chunk_size: 1000,
                num_threads: 0,
            },
            needle,
            cands,
        )
    })
}

fn try_hard_into_bytes<'b, 'v>(bump: &'b Bump, v: Value<'v>) -> Result<(Value<'v>, Vec<'b, u8>)> {
    match try_into_bytes(bump, v) {
        Ok(s) => Ok((v, s)),
        Err(e) => {
            let env = v.env;
            if let Ok(s) = e::symbol_name
                .call(env, [v])
                .and_then(|v| Ok((v, try_into_bytes(bump, v)?)))
            {
                return Ok(s);
            }
            if let Ok(s) = v.car().and_then(|v| Ok((v, try_into_bytes(bump, v)?))) {
                return Ok(s);
            }
            Err(e)
        }
    }
}
fn try_into_bytes<'b>(bump: &'b Bump, v: Value) -> Result<Vec<'b, u8>> {
    if let Ok(s) = copy_to_bytes(bump, v) {
        Ok(s)
    } else {
        let env = v.env;
        let v =
            e::encode_coding_string.call(env, [v, e::no_conversion.bind(env), e::t.bind(env)])?;
        copy_to_bytes(bump, v)
    }
}

// slightly modified emacs::Env::string_bytes
// this dont always return valid utf8:
// emacs do not give valid utf8 if calling this from the result of
// (encode-coding-string str 'no-conversion 'nocopy)
// if str have non-utf8 codepoints
fn copy_to_bytes<'b>(bump: &'b Bump, v: Value) -> Result<Vec<'b, u8>> {
    let env = v.env;
    let env_raw = env.raw();
    let mut len: isize = 0;
    unsafe {
        let copy_string_contents = (*env_raw).copy_string_contents.unwrap();

        if !copy_string_contents(env_raw, v.raw(), ptr::null_mut(), &mut len) {
            env.handle_exit(false)?;
            unreachable!();
        }
        #[allow(clippy::cast_sign_loss)]
        let mut bytes = Vec::<u8>::with_capacity_in(len as usize, bump);

        if !copy_string_contents(
            env_raw,
            v.raw(),
            bytes.as_mut_ptr().cast::<os::raw::c_char>(),
            &mut len,
        ) {
            env.handle_exit(false)?;
            unreachable!();
        }
        #[allow(clippy::cast_sign_loss)]
        let len = len as usize;
        bytes.set_len(len);
        // should always be 0 terminated
        assert!(bytes[len - 1] == 0);
        // safety: should always satisfy len>0, we checked above too
        bytes.set_len(len - 1);
        Ok(bytes)
    }
}
