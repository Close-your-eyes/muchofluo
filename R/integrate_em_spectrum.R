#' Integrate emission spectra
#'
#' Computes the area under each emission spectrum using trapezoidal
#' integration.
#'
#' @param data A spectral data frame or a list containing a spectral data
#'   frame.
#' @param min Minimum wavelength (nm). Defaults to the minimum wavelength
#'   available for each fluorochrome.
#' @param max Maximum wavelength (nm). Defaults to the maximum wavelength
#'   available for each fluorochrome.
#'
#' @returns
#' A named numeric vector containing the integrated emission area for each
#' fluorochrome.
#'
#' @details
#' Integration is performed with `pracma::trapz()`.
#'
#' @export
integrate_em_spectrum <- function(data,
                                  min = NULL,
                                  max = NULL) {

  if (!is.data.frame(data)) {
    if (is.list(data)) {
      data <- data[[1]]
    } else {
      stop("data should be data frame.")
    }
  }

  data <- data |> dplyr::filter(type == "em")
    #dplyr::filter(is.null(fluo) | fluorochrome %in% fluo)

  areas <- purrr::map_dbl(split(data, data$fluorochrome), function(x) {
    if (is.null(min)) {
      min <- min(x$nm)
    }
    if (is.null(max)) {
      max <- max(x$nm)
    }
    x <- x |>
      dplyr::filter(nm >= min, nm <= max) |>
      dplyr::arrange(nm)
    pracma::trapz(x$nm, x$value)
  })
  return(areas)
}
