#' Title
#'
#' @param fluos
#' @param spectra_df
#' @param maxima_df
#' @param progress
#'
#' @returns
#' @export
#'
#' @examples
#' out <- get_spectral_data(c("A647", "FITC"))
get_spectral_data <- function(fluos,
                              spectra_df = vroom::vroom(system.file("extdata", "spectra.tsv.gz", package = "muchofluo"), show_col_types = F, progress = F),
                              maxima_df = vroom::vroom(system.file("extdata", "em_ex_maxima.tsv", package = "muchofluo"), show_col_types = F, progress = F),
                              progress = F) {

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
      data <-
        data |>
        dplyr::filter(source == y) |>
        tidyr::pivot_longer(cols = c(em, ex), names_to = "type", values_to = "value") |>
        tidyr::drop_na()
    })
  }, .progress = progress)

  spec_data <- brathering::list_invert(spec_data)
  spec_data <- purrr::map(spec_data, ~dplyr::bind_rows(.x))

  return(spec_data)
}
