#' Plot spectral data
#'
#' Make ggplot with spectral data.
#'
#' @param spec_data data from get_spectral_data fun, should be a list
#' @param col_pal color spectra by wavelength (max em/ex) or any palette
#' @param col_em_ex color excitation and emission of one fluorochrome differently
#' (T) or both according max(em) (F)
#' @param type what to plot em and/or ex
#' @param shade_type what to shade: none, ex and/or em
#' @param shade_type_args args to geom_ribbon
#'
#' @returns ggplot
#' @export
#'
#' @examples
#' out <- get_spectral_data(c("A647", "FITC"))
#' plots <- plot_spectra(out)
plot_spectra <- function(spec_data,
                         type = c("em", "ex"),
                         col_pal = "wavelength",
                         col_em_ex = T,
                         shade_type = c("none"), # em, ex
                         shade_type_args = list(alpha = 0.3, show.legend = F)) {

  ## work with cases?
  ## color by wavelength exactly:
  # ggplot(data$bd |> dplyr::mutate(fluorochrome_type = paste0(fluorochrome, "_", type)), aes(x = nm, y = value)) +
  #   ggplot2::geom_line(ggplot2::aes(group = fluorochrome_type, color = nm))  +
  #   colrr::theme_material() +
  #   colrr::scale_color_spectral(colors = colrr::wl_to_hex(unique(data$bd$nm)))

  # currently: color by wavelength_max
  # other option: color by closest max value: useful for ex/em with multiple peaks

  # geom_line args passing?


  if (!requireNamespace("colrr", quietly = T)) {
    devtools::install_github("close-your-eyes/colrr")
  }
  type <- rlang::arg_match(type, multiple = T)

  if (is.data.frame(spec_data)) {
    spec_data <- list(spec_data)
  }

  plots <- purrr::map(spec_data, function(data) {

    data <- data |>
      dplyr::mutate(fluorochrome_type = paste0(fluorochrome,"_",type)) |>
      dplyr::filter(type %in% !!type)
    data_max <- attr(data, "max") |>
      dplyr::mutate(fluorochrome_type = paste0(fluorochrome,"_",type)) |>
      dplyr::filter(type %in% !!type)

    col_aes <- ifelse(col_em_ex, "fluorochrome_type", "fluorochrome")


    if (col_pal == "wavelength") {
      max <- data_max |>
        dplyr::filter(col_em_ex | type == "em") |>
        dplyr::slice_max(norm_intensity, with_ties = F, by = !!rlang::sym(col_aes))
      colpal <- stats::setNames(colrr::wl_to_hex(max[["nm"]]), max[[col_aes]])
    } else {
      colpal <- colrr::col_pal(name = col_pal, n = unique(data[[col_aes]]), return = "c")
    }


    p <- ggplot2::ggplot(data, ggplot2::aes(x = nm, y = value)) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 2), order = 1),
                      linetype = ggplot2::guide_legend(override.aes = list(linewidth = 1, color="white", order = 2))) +
      ggplot2::geom_segment(
        data = data_max,
        ggplot2::aes(x = nm, y = 0, yend = norm_intensity, linetype = type, color = !!rlang::sym(col_aes))
      ) +
      ggplot2::geom_line(ggplot2::aes(linetype = type, color = !!rlang::sym(col_aes))) +
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
      ggplot2::scale_y_continuous(expand = ggplot2::expansion()) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion()) +
      ggplot2::scale_color_manual(values = colpal) # aesthetics = c("color", "fill")
    #  ggplot2::scale_fill_manual(values = colpal)

    shade_type <- intersect(shade_type, type)
    if (length(intersect(c("ex", "em"), shade_type))) {
      for (i in shade_type) {
        p <- p + do.call(ggplot2::geom_ribbon,
                         args = c(shade_type_args,
                                  list(data = dplyr::filter(data, type %in% i),
                                       mapping = ggplot2::aes(ymin = 0,
                                                              ymax = value,
                                                              fill = !!rlang::sym(col_aes)))))
      }
      p <- p + ggplot2::scale_fill_manual(values = colpal)
    }

    return(p)

  })
  return(plots)
}

