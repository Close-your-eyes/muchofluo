#' Query fluorochrome names with string matching
#'
#' This may be used to harmonize different spellings of fluorochromes.
#' The query is matched against full names and short forms. Anything but
#' letters and numbers are ignored, case is irrelevant. Matching can be very
#' sensitive when match_cutoff is increased.
#'
#' @param query vector of query strings
#' @param multimatch how to handle multiple matches, return all of them
#' or the best only
#' @param ref reference table with synonymes and names to return
#' @param match_cutoff min match score to return results, smaller value =
#' stricter
#'
#' @return vector of fluorochrome names
#' @export
#'
#' @examples
#' query <- c("abc", "pr", "0", "pecy5", "pecy")
#' query_fluo(query)
query_fluo <- function(query,
                       multimatch = c("best", "all"),
                       match_cutoff = 0.5,
                       ref = system.file("extdata", "fluo_lookup.tsv", package = "muchofluo")) {

  # oversensitive matching, but anayway. e.g.: pure
  multimatch <- rlang::arg_match(multimatch)

  ref <- vroom::vroom(ref, col_types = "cc", progress = F) # silence
  lookupvec <- stats::setNames(ref$fluorochrome, ref$syn)

  proc_query <- preprocess(query)
  origsyn <- ref$syn
  #ref$syn <- preprocess(fluoro_names)

  out <- purrr::map2(stats::setNames(query, query), proc_query, function(query, proc_query) {
    if (nchar(proc_query) == 0) {
      return(NULL)
    }

    ## check for immediate hit
    hit <- which(utils::adist(proc_query, ref$syn)[1,] == 0)
    if (length(hit) == 1) {
      return(unname(lookupvec[ref$syn[hit]]))
    }

    # Numeric‐substring filter
    nums <- stringr::str_extract_all(proc_query, "\\d+")[[1]]
    if (length(nums) > 0) {
      longest <- nums[which.max(nchar(nums))]
      digits <- strsplit(as.character(longest), "")[[1]]
      has_all_digits <- purrr::map_lgl(strsplit(ref$syn,""), ~all(digits %in% .x))
      if (any(has_all_digits)) {
        ref <- ref[which(has_all_digits),]
      }
    }

    # pre-filter
    idx <- unique(c(which(match_subseq_scan(proc_query, ref$syn)),
                    which(purrr::map_lgl(ref$syn, match_subseq_scan, refs = proc_query)),
                    which(utils::adist(proc_query, ref$syn)[1,] <= 3)))
    ref <- ref[idx,]
    matches <- unname(lookupvec[ref$syn])

    # look for best match in pre filter
    if (length(unique(matches)) > 1 && multimatch == "all") {
      return(unique(matches))
    } else if ((length(unique(matches)) > 1 && multimatch == "best") || length(unique(matches)) == 1) {
      res <- stringdist::stringdist(proc_query, ref$syn) #utils::adist(proc_query, ref$syn)[1,]
      resrel <- res/pmax(nchar(proc_query), nchar(ref$syn))
      inds <- which(resrel < match_cutoff)

      if (!length(inds)) {
        message(query, ": no match below threshold.")
        return(NULL)
      }
      matches <- matches[inds]
      if (length(unique(matches)) > 1) {
        message("multiple matches for ", query, ": ", paste(unique(matches), collapse = "; "), "\n")
      }
      return(matches[which.min(resrel[inds])])
    } else {
      message(query, ": no match")
      return(NULL)
    }
  })
}


preprocess <- function(x) {
  x <- tolower(gsub("[^[:alnum:]]", "", x))
  return(x)
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


