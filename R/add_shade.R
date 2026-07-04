#' Add shaded excitation or emission ribbons
#'
#' Creates a reusable `ggplot2::geom_ribbon()` layer for shading excitation
#' or emission spectra. Intended for use within `plot_spectra()` or other
#' ggplot objects containing spectral data.
#'
#' @param fluo Optional character vector of fluorochrome names to shade.
#'   If `NULL`, all fluorochromes of the selected type are used.
#' @param type Character string specifying which spectra to shade.
#'   One of `"em"` (emission) or `"ex"` (excitation).
#' @param fun Geom function used to create the ribbon. Defaults to
#'   `ggplot2::geom_ribbon()`.
#' @param args Named list of additional arguments passed to `fun`.
#'
#' @returns
#' A ggplot layer that can be added to a plot with `+`.
#'
#' @export
#' @examples
#' data <- get_spectral_data(fluos = c("FITC", "PE-cy7"))
#' plot_spectra(data$bd)[[1]] +
#'   add_shade2(fluo = "FITC") +
#'   ggplot2::scale_fill_manual(values = "white")
add_shade2 <- function(fluo = NULL,
                       type = "em",
                       fun = ggplot2::geom_ribbon,
                       args = list(alpha = 0.3, show.legend = F)) {

  type <- rlang::arg_match(type, c("em", "ex"))
  do.call(fun, args = c(args,
                        list(data = ~dplyr::filter(.x, type == !!type) |>
                               dplyr::filter(is.null(fluo) | fluorochrome %in% fluo),
                             mapping = ggplot2::aes(ymin = 0, ymax = value, fill = fluorochrome))))

  # scale_fill must be defined outside

}




#' Highlight a wavelength range on a spectral plot
#'
#' Adds a shaded ribbon to a spectral plot over a specified wavelength range.
#' The range can be supplied either as minimum and maximum wavelengths or
#' using filter notation such as `"530/30"`. Optionally annotates the plot
#' with the percentage of the emission spectrum contained within the selected
#' range.
#'
#' @param obj A ggplot object returned by `plot_spectra()`.
#' @param type Spectrum type to shade: `"em"` or `"ex"`.
#' @param fluo Optional fluorochrome name. Required when the plot contains
#'   multiple fluorochromes.
#' @param min Minimum wavelength (nm).
#' @param max Maximum wavelength (nm).
#' @param range Character string specifying a filter as `"center/width"`
#'   (e.g. `"530/30"`). Overrides `min` and `max`.
#' @param fill Fill colour of the shaded region.
#' @param alpha Ribbon transparency.
#' @param annotate_pct Logical; if `TRUE`, annotate the plot with the
#'   percentage of the emission spectrum within the selected range.
#'
#' @returns
#' A ggplot object with the shaded wavelength region added.
#'
#' @seealso [filter_pct()], [plot_spectra()]
#'
#' @export
#' @examples
#' library(muchofluo)
#' data <- get_spectral_data(fluos = c("FITC", "PE-cy7"))
#' pp <- plot_spectra(data$bd)[[1]]
#' add_shade(pp,
#'           fluo = "FITC",
#'           range = "515/50",
#'           annotate_pct = TRUE)
#' add_shade(pp,
#'           fluo = "FITC",
#'           annotate_pct = TRUE)
add_shade <- function(obj,
                      type = "em",
                      fluo = NULL,
                      min = NULL,
                      max = NULL,
                      range = NULL,
                      fill = "white",
                      alpha = 0.3,
                      annotate_pct = F) {

  type <- rlang::arg_match(type, c("em", "ex"))

  # If range like "530/30" is provided
  if (!is.null(range)) {
    parts <- strsplit(range, "/")[[1]]

    if (length(parts) != 2) {
      stop("Range must be in format 'center/width'")
    }

    center <- as.numeric(parts[1])
    width  <- as.numeric(parts[2])

    if (is.na(center) || is.na(width)) {
      stop("Invalid numeric values in range")
    }

    min <- center - width/2
    max <- center + width/2
  }

  # Validate min/max
  # if (is.null(min) || is.null(max)) {
  #   stop("Provide either min & max OR range = 'center/width'")
  # }

  data <- obj@data |>
    dplyr::filter(type == !!type) |>
    dplyr::filter(is.null(fluo) | fluorochrome %in% fluo)


  if (is.null(min) && is.null(max) && length(unique(data$fluorochrome)) == 1) {
    min <- min(data$nm)
    max <- max(data$nm)
  }

  if (annotate_pct) {
    pct <- round(filter_pct(data = data,
                            min = min,
                            max = max), 1)
  }
  if (!is.null(min)) {
    data <- dplyr::filter(data, nm >= min)
  }
  if (!is.null(max)) {
    data <- dplyr::filter(data, nm <= max)
  }

  if (length(unique(data$fluorochrome)) > 1) {
    stop("filter data for one fluorochrome only.")
  }



  obj <- obj +
    ggplot2::geom_ribbon(data = data,
                         mapping = ggplot2::aes(ymin = 0, ymax = value),
                         alpha = alpha,
                         fill = fill)

  if (annotate_pct) {
    obj <- obj +
      ggplot2::annotate(
        geom = "text",
        y = 10,
        x = min+(max-min)/1.3,
        label = pct,
        size = 3
      )
  }


  return(obj)

}
