use bumpalo::{boxed::Box, collections::string::String, Bump};
use std::{marker::PhantomData, mem};

fn into_boxed_str(v: String) -> Box<str> {
    unsafe { Box::from_raw(Box::into_raw(v.into_bytes().into_boxed_slice()) as *mut str) }
}

pub(crate) fn parse_to_string<'b>(bump: &'b Bump, v: Box<'b, [u8]>) -> Box<'b, str> {
    match std::str::from_utf8(&v) {
        Ok(_) => unsafe { Box::from_raw(Box::into_raw(v) as *mut str) },
        Err(_) => into_boxed_str(String::from_utf8_lossy_in(&v, bump)),
    }
}

// T: 'b due to bound on Box so cant yield Box<'b, T> otherwise
pub(crate) struct BoxIntoIter<'b, T: 'b> {
    cur: *const T,
    end: *const T,
    marker1: PhantomData<&'b ()>,
    // not really needed, in theory we can #[may_dangle] T on drop then we would need this
    marker2: PhantomData<T>,
}

impl<'b, T> From<Box<'b, [T]>> for BoxIntoIter<'b, T> {
    #[inline]
    fn from(value: Box<'b, [T]>) -> Self {
        if mem::size_of::<T>() == 0 {
            todo!()
        }
        let range = unsafe { &mut *Box::into_raw(value) }.as_mut_ptr_range();
        Self {
            cur: range.start,
            end: range.end,
            marker1: PhantomData,
            marker2: PhantomData,
        }
    }
}

impl<'b, T> Drop for BoxIntoIter<'b, T> {
    fn drop(&mut self) {
        for _ in self {}
    }
}

impl<'b, T> Iterator for BoxIntoIter<'b, T> {
    type Item = Box<'b, T>;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if self.cur == self.end {
                None
            } else {
                let old = self.cur;
                self.cur = self.cur.offset(1);
                Some(Box::from_raw(old.cast_mut()))
            }
        }
    }
}

impl<'b, T> ExactSizeIterator for BoxIntoIter<'b, T> {
    #[inline]
    fn len(&self) -> usize {
        #[allow(clippy::cast_sign_loss)]
        unsafe {
            self.end.offset_from(self.cur) as usize
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let b = Bump::new();
        let arr = Box::from_iter_in((0..10).map(std::boxed::Box::new), &b);
        let mut iter: BoxIntoIter<_> = arr.into();
        for _ in 0..5 {
            let obj = iter.next().unwrap();
            println!("{obj}");
        }
    }
}
