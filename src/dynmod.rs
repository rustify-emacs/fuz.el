use crate::utils::*;
// use emacs::{defun, Env, Value, IntoLisp, FromLisp, Vector as LispVec};
use fuzzy_matcher::clangd::{
    fuzzy_indices as fuzzy_indices_clangd,
    fuzzy_match as fuzzy_match_clangd,
};
use fuzzy_matcher::skim::{
    fuzzy_indices as fuzzy_indices_skim,
    fuzzy_match as fuzzy_match_skim
};

fn find_indices_into_lisp<'a, F>(env: &'a Env, fun: F, pat: &str, src: &str)
                        -> Option<Vec<Value<'a>>>
where
    F: Fn(&str, &str) -> Option<(i64, Vec<usize>)>,
{
    if let Some((_score, indices)) = fun(&src, &pat) {
        let indices: Vec<Value<'a>> = indices
            .into_iter()
            .map(|it| {
                let it_i64 = it as i64;
                return it_i64.into_lisp(env).unwrap();
            })
            .collect::<Vec<Value<'a>>>();
        return Some(indices);
    } else {
        return None;
    }
}

// Output Functions

/// Return the PATTERN fuzzy score about SOURCE, using skim's fuzzy algorithm.
///
/// Sign: (-> Str Str (Option Long))
///
/// Return nil if no match happened.
///
/// (fn PATTERN SOURCE)
#[defun]
fn calc_score_skim(_env: &Env, pat: String, src: String)
                   -> LispResult<Option<i64>> {
    Ok(fuzzy_match_skim(&src, &pat))
}

/// Return the PATTERN fuzzy score abount SOURCE, using clangd's fuzzy algorithm.
///
/// Sign: (-> Str Str (Option Long))
///
/// See `fuz-calc-score-skim' for more information
///
/// (fn PATTERN SOURCE)
#[defun]
fn calc_score_clangd(_env: &Env, pat: String, src: String)
                     -> LispResult<Option<i64>> {
    Ok(fuzzy_match_clangd(&src, &pat))
}

/// Find the indices for a PATTERN matching SOURCE, using skim's fuzzy algorithm.
///
/// Sign: (-> Str Str (Listof Long))
///
/// Return a list of integer that marks the position of matched char.
///
/// Return nil if nothing was matched.
///
/// (fn PATTERN SOURCE)
#[defun]
fn find_indices_skim(env: &Env, pat: String, src: String)
                      -> LispResult<Option<Value<'_>>> {
    if let Some(val) = find_indices_into_lisp(env,
                                              fuzzy_indices_skim,
                                              &pat,
                                              &src,) {
        return Ok(Some(make_lisp_list(env, val)?));
    } else {
        return Ok(None);
    }
}


/// Find the indices for a PATTERN matching SOURCE, using clangd's fuzzy algorithm.
///
/// Sign: (-> Str Str (Listof Long))
///
/// See `fuz-find-indices-skim' for more infomation
///
/// (fn PATTERN SOURCE)
#[defun]
fn find_indices_clangd(env: &Env, pat: String, src: String)
                        -> LispResult<Option<Value<'_>>> {
    if let Some(val) = find_indices_into_lisp(env,
                                              fuzzy_indices_clangd,
                                              &pat,
                                              &src,) {
        return Ok(Some(make_lisp_list(env, val)?));
    } else {
        return Ok(None);
    }
}
    }
}
