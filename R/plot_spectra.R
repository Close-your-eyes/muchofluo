#' Title
#'
#' @param spec_data
#'
#' @returns
#' @export
#'
#' @examples
#' out <- get_spectral_data(c("A647", "FITC"))
#' plots <- plot_spectra(out)
plot_spectra <- function(spec_data) {

  # no max yet

  plots <- purrr::map(spec_data, function(data) {
    ggplot2::ggplot(data, ggplot2::aes(x = nm, y = value)) +
      ggplot2::geom_line(ggplot2::aes(linetype = type, color = fluorochrome)) + # colrr::col_pal("material")[1]
      colrr::theme_material(text_fun = ggplot2::element_text) +
      ggplot2::theme(
        plot.subtitle = ggplot2::element_text(size = 12),
        plot.caption = ggplot2::element_text(size = 6)
      ) +
      ggplot2::labs(
        x = "wavelength [nm]",
        y = "norm intensity [%]",
        caption = paste0("source: ", unique(data[["source"]]))
      ) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 2)),
                      linetype = ggplot2::guide_legend(override.aes = list(linewidth = 1, color="white")))
    # ggplot2::geom_segment(
    #   data = dplyr::filter(maxdata, source == y),
    #   ggplot2::aes(x = nm, y = 0, yend = norm_intensity, linetype = type),
    #   color = "white"
    # )
  })
  return(plots)
}
