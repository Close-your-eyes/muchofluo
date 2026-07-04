#' Add filter to spectral plot
#'
#' @param min lower limit
#' @param max upper limit
#' @param range range in form of 530/30, center/width
#' @param fill fill color
#' @param alpha opacity from 0 to 1
#'
#' @returns geom rect
#' @export
#'
#' @examples
#' plot_spectra(get_spectral_data(c("PE", "FITC"))[[1]])[[1]] +
#'   geom_filter(range = "515/30")
geom_filter <- function(min = NULL,
                       max = NULL,
                       range = NULL,
                       fill = "grey90",
                       alpha = 0.2) {

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
  if (is.null(min) || is.null(max)) {
    stop("Provide either min & max OR range = 'center/width'")
  }

  band_data <- data.frame(
    xmin = min,
    xmax = max,
    ymin = -Inf,
    ymax = Inf
  )

  ggplot2::geom_rect(data = band_data,
                     ggplot2::aes(xmin = xmin, xmax = xmax,
                                  ymin = ymin, ymax = ymax),
                     fill = fill,
                     alpha = alpha,
                     inherit.aes = FALSE)
}
