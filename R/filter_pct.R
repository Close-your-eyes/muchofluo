#' Calculate emission passing through a wavelength filter
#'
#' Calculates the percentage of the total emission spectrum contained within
#' a specified wavelength interval.
#'
#' @param data Spectral data frame containing emission spectra.
#' @param min Minimum wavelength (nm).
#' @param max Maximum wavelength (nm).
#' @param range Character string specifying a filter as `"center/width"`
#'   (e.g. `"530/30"`). Overrides `min` and `max`.
#'
#' @returns
#' A numeric vector giving the percentage of the integrated emission spectrum
#' within the specified wavelength range for each fluorochrome.
#'
#' @seealso [integrate_em_spectrum()]
#'
#' @export
filter_pct <- function(data,
                       min = NULL,
                       max = NULL,
                       range = NULL) {

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

  full <- integrate_em_spectrum(data)
  filter <- integrate_em_spectrum(data, min = min, max = max)

  return(filter/full*100)
}
