#' Get spectral data from internal package data
#'
#' @param fluos fluorochrome names, will be matched by query_fluo
#' @param spectra_df data frame with spectral data
#' @param maxima_df data frame with data on max emission and excitation
#'
#' @returns list of df
#' @export
#'
#' @examples
#' out <- get_spectral_data(c("A647", "FITC"))
get_spectral_data <- function(fluos,
                              spectra_df = vroom::vroom(system.file("extdata", "spectra.tsv.gz", package = "muchofluo"), show_col_types = F, progress = F),
                              maxima_df = vroom::vroom(system.file("extdata", "em_ex_maxima.tsv", package = "muchofluo"), show_col_types = F, progress = F)) {

  if (!requireNamespace("brathering", quietly = TRUE)) {
    stop("The 'brathering' package is required. Install it with:\n",
         "pak::pak('close-your-eyes/brathering')",
         call. = FALSE)
  }

  # no error catching

  # fluos_intersect <- intersect(unique(spectra_df$fluorochrome),
  #                              unique(maxima_df$fluorochrome))

  query <- unlist(query_fluo(fluos))

  spec_data <- purrr::map(query, function(x) {
    data <- dplyr::filter(spectra_df, fluorochrome == x)
    maxdata <- dplyr::filter(maxima_df, fluorochrome == x)
    sources <- intersect(unique(data$source),
                         unique(maxdata$source))
    purrr::map(purrr::set_names(sources), function(y) {
      data2 <-
        data |>
        dplyr::filter(source == y) |>
        tidyr::pivot_longer(cols = c(em, ex), names_to = "type", values_to = "value") |>
        tidyr::drop_na()
      attr(data2, "max") <- dplyr::filter(maxdata, source == y)
      return(data2)
    })
  })

  spec_data <- brathering::list_invert(spec_data)
  spec_data2 <- purrr::map(spec_data, ~dplyr::bind_rows(.x))
  maxes <- purrr::map(spec_data, ~purrr::map_dfr(.x, ~attr(.x, "max")))
  spec_data2 <- purrr::map2(spec_data2, maxes, function(x,y) {
    attr(x, "max") <- y
    return(x)
  })

  return(spec_data2)
}
