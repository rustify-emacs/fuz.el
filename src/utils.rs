pub use emacs::{defun, Env, Value, IntoLisp, FromLisp, Vector as LispVec};

pub type LispResult<T> = emacs::Result<T>;

pub fn make_lisp_list<'a, T, I>(env: &'a Env, iter: I)
                             -> LispResult<Value<'a>>
where
    T: IntoLisp<'a>,
    I: IntoIterator<Item = T>
{
    let mut retval = vec![];
    for it in iter.into_iter() {
        retval.push(it.into_lisp(env)?);
    }
    return Ok(env.list(&retval[..])?);
}

pub fn take_lisp_vector<'a, T>(vec: LispVec<'a>)
                           -> LispResult<Vec<T>>
where
    T: FromLisp<'a>
{
    let len = vec.size()?;
    let mut retval = Vec::with_capacity(len);
    for idx in 0..len {
        retval.push(vec.get::<T>(idx)?);
    }
    return Ok(retval);
}
