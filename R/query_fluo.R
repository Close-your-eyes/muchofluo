#' Query the
#'
#' @param query
#' @param multimatch
#' @param ref
#'
#' @return
#' @export
#'
#' @examples
query_fluo <- function(query,
                       multimatch = c("best", "all"),
                       ref = system.file("extdata", "fluo_lookup.tsv", package = "muchofluo")) {

  # oversensitive matching, but anayway. e.g.: pure
  multimatch <- rlang::arg_match(multimatch)

  ref <- vroom::vroom(ref) # silence
  lookupvec <- setNames(ref$fluorochrome, ref$syn)

  preprocess <- function(x) {
    x <- tolower(gsub("[^[:alnum:]]", "", x))
    return(x)
  }
  proc_query <- preprocess(query)
  if (nchar(proc_query) == 0) {
    return(NULL)
  }
  origsyn <- ref$syn
  #ref$syn <- preprocess(fluoro_names)


  ## check for immediate hit
  hit <- which(adist(proc_query, ref$syn)[1,] == 0)
  if (length(hit) == 1) {
    return(unname(lookupvec[ref$syn[hit]]))
  }

  # Numeric‐substring filter
  nums <- str_extract_all(proc_query, "\\d+")[[1]]
  if (length(nums) > 0) {
    longest    <- nums[which.max(nchar(nums))]
    # only finds longest not preceded or followed by another digit
    pattern  <- paste0("(?<!\\d)", longest, "(?!\\d)")
    has_num   <- str_detect(ref$syn, regex(pattern))
    if (any(has_num)) {
      candidate_idx <- which(has_num)
      # remove that numeric run from both proc_query & candidates
      proc_query <- str_squish(str_replace_all(proc_query, fixed(longest), ""))
      ref$syn[candidate_idx] <- str_squish(str_replace_all(ref$syn[candidate_idx], fixed(longest), ""))
    } else {
      candidate_idx <- seq_along(ref$syn)
    }
  } else {
    candidate_idx <- seq_along(ref$syn)
  }

  candidates <- origsyn[candidate_idx]
  proc_cand  <- ref$syn[candidate_idx]

  proc_cand_idx <- which(match_subseq_scan(proc_query, proc_cand))
  matches <- unique(unname(lookupvec[candidates[proc_cand_idx]]))
  if (length(matches) > 1) {
    message("multiple matches for ", query, ": ", paste(matches, collapse = "; "), "\n")
    # options: return all or lowest lv-dist
    if (multimatch == "best") {
      return(matches[which.min(adist(query, matches)[1,])])
    } else {
      return(matches)
    }
  } else if (length(matches) == 1) {
    return(matches)
  } else {
    message(query, ": no match")
    return(NULL)
  }
}


match_subseq_scan <- function(query, refs) {
  # helper: test one ref string
  single_check <- function(q, s) {
    qchars <- strsplit(q, "")[[1]]
    remainder <- s
    for (ch in qchars) {
      # find first occurrence of ch
      pos <- regexpr(ch, remainder, fixed = TRUE)[1]
      if (pos == -1) return(FALSE)
      # chop off up through that character
      remainder <- substring(remainder, pos + 1)
    }
    TRUE
  }
  return(vapply(refs, single_check, logical(1), q = query))
}
