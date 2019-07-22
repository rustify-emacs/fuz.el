use crate::utils::*;
use fuzzy_matcher::clangd::{
    fuzzy_indices as fuzzy_indices_clangd, fuzzy_match as fuzzy_match_clangd,
};
use fuzzy_matcher::skim::{fuzzy_indices as fuzzy_indices_skim, fuzzy_match as fuzzy_match_skim};
use rayon::prelude::*;

fn find_indices_into_lisp<'a, F>(
    env: &'a Env,
    fun: F,
    pat: &str,
    src: &str,
) -> Option<Vec<Value<'a>>>
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
fn calc_score_skim(_env: &Env, pat: String, src: String) -> LispResult<Option<i64>> {
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
fn calc_score_clangd(_env: &Env, pat: String, src: String) -> LispResult<Option<i64>> {
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
fn find_indices_skim(env: &Env, pat: String, src: String) -> LispResult<Option<Value<'_>>> {
    if let Some(val) = find_indices_into_lisp(env, fuzzy_indices_skim, &pat, &src) {
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
fn find_indices_clangd(env: &Env, pat: String, src: String) -> LispResult<Option<Value<'_>>> {
    if let Some(val) = find_indices_into_lisp(env, fuzzy_indices_clangd, &pat, &src) {
        return Ok(Some(make_lisp_list(env, val)?));
    } else {
        return Ok(None);
    }
}

#[defun]
fn filter_cands_with_pat_skim<'a>(
    env: &'a Env,
    pat: String,
    cands: LispVec<'a>,
    sortp: Value<'a>,
) -> LispResult<Value<'a>>
{
    let strs = take_lisp_vector_indexed::<String>(cands)?;
    let mut filtered = strs
        .into_par_iter()
        .filter(|(_, s)| fuzzy_match_skim(&s, &pat).is_some())
        .collect::<Vec<(usize, String)>>();
    if sortp.is_not_nil() {
        filtered.par_sort_unstable_by_key(
            |(_, s)| fuzzy_match_skim(&s, &pat).map(|num| -num)
        )
    }
    let mut retvec = Vec::with_capacity(filtered.len());
    // let mut retvec = vec![];
    for (idx, _) in filtered.into_iter() {
        retvec.push(cands.get::<Value<'a>>(idx)?)
    }
    return make_lisp_list(env, retvec);
}


#[defun]
fn filter_cands_list_with_pat_skim<'a>(
    env: &'a Env,
    pat: String,
    cands: Value<'a>,
    sortp: Value<'a>,
) -> LispResult<Value<'a>> {
    let strs = take_lisp_list::<String>(cands)?;
    let mut filtered = strs
        .into_par_iter()
        .filter(|s| match fuzzy_match_skim(&s, &pat) {
            Some(_) => true,
            None => false,
        })
        .collect::<Vec<String>>();
    if sortp.is_not_nil() {
        filtered.par_sort_unstable_by_key(|s| fuzzy_match_skim(&s, &pat).map(|num| -num))
    }
    return make_lisp_list::<String, _>(env, filtered);
}


#[defun]
fn filter_cands_with_pat_clangd<'a>(
    env: &'a Env,
    pat: String,
    cands: LispVec<'a>,
    sortp: Value<'a>,
) -> LispResult<Value<'a>> {
    let strs = take_lisp_vector::<String>(cands)?;
    let mut filtered = strs
        .into_par_iter()
        .filter(|s| match fuzzy_match_skim(&s, &pat) {
            Some(_) => true,
            None => false,
        })
        .collect::<Vec<String>>();
    if sortp.is_not_nil() {
        filtered.par_sort_unstable_by_key(|s| fuzzy_match_clangd(&s, &pat).map(|num| -num))
    }
    return make_lisp_list::<String, _>(env, filtered);
}
