#' Make ggplots of emission and excitation spectra
#'
#' @param spectra_df data frame of path to file on disk with spectral data
#' @param maxima_df data frame of path to file on disk with maxima/peaks of
#' emission and excitation
#' @param path path to folder where to save plots to
#' @param format format to save, see ?ggsave for possible formats
#' @param width width of plots
#' @param height height of plots
#'
#' @return nothing, files on disk created
#' @export
#'
#' @examples
#' \dontrun{
#' # run with all values default
#' make_spectra_plots()
#' }
make_spectra_plots <- function(spectra_df = system.file("extdata", "spectra.tsv.gz", package = "muchofluo"),
                               maxima_df = system.file("extdata", "em_ex_maxima.tsv", package = "muchofluo"),
                               path = file.path(getwd(), "spectra_plots"),
                               format = "pdf",
                               width = 5,
                               height = 3.5) {

  if (!requireNamespace("colrr")) {
    devtools::install_github("Close-your-eyes/colrr")
  }
  if (!requireNamespace("brathering")) {
    devtools::install_github("Close-your-eyes/brathering")
  }

  dir.create(path = path, recursive = T, showWarnings = F)
  if (!dir.exists(path)) {
    stop("path directory not found. not able to create it.")
  }

  if (is.character(maxima_df) && file.exists(maxima_df)) {
    maxima_df <- vroom::vroom(maxima_df, show_col_types = F, progress = F)
  }
  if (is.character(maxima_df) && file.exists(maxima_df)) {
    spectra_df <- vroom::vroom(spectra_df, show_col_types = F, progress = F)
  }

  fluos <- intersect(unique(spectra_df$fluorochrome),
                     unique(maxima_df$fluorochrome))

  purrr::map(fluos, function(x) {
    data <- dplyr::filter(spectra_df, fluorochrome == x)
    maxdata <- dplyr::filter(maxima_df, fluorochrome == x)
    sources <- intersect(unique(data$source),
                         unique(maxdata$source))
    purrr::map(sources, function(y) {
      data <-
        data |>
        dplyr::filter(source == y) |>
        tidyr::pivot_longer(cols = c(em, ex), names_to = "type", values_to = "value") |>
        tidyr::drop_na()
      pp <- ggplot2::ggplot(data, ggplot2::aes(x = nm, y = value)) +
        ggplot2::geom_line(ggplot2::aes(linetype = type), color = "white") + # colrr::col_pal("material")[1]
        colrr::theme_material(text_fun = ggplot2::element_text) +
        ggplot2::theme(
          legend.position.inside = c(0.95,0.9),
          legend.key.size = ggplot2::unit(0, "pt"),
          legend.key.spacing.y = ggplot2::unit(10, "pt"),
          plot.subtitle = ggplot2::element_text(size = 12),
          plot.caption = ggplot2::element_text(size = 6)
        ) +
        ggplot2::labs(
          linetype = NULL,
          x = "wavelength [nm]",
          y = "norm intensity [%]",
          subtitle = x,
          caption = paste0("source: ", y)
        ) +
        ggplot2::geom_segment(
          data = dplyr::filter(maxdata, source == y),
          ggplot2::aes(x = nm, y = 0, yend = norm_intensity, linetype = type),
          color = "white"
        ) +
        ggplot2::guides(linetype = ggplot2::guide_legend(
          position = "inside",
          label.position = "bottom"
        ))
      ggplot2::ggsave(
        filename = paste0(brathering::make_portable_filename(x), "__", y, ".", format),
        plot = pp,
        device = format,
        path = path,
        width = width,
        height = height
      )
    })
  }, .progress = T)
}
