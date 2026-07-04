#' Plot spectral data
#'
#' Make ggplot with spectral data.
#'
#' @param spec_data data from get_spectral_data fun, list or df
#' @param col_pal how to color spectra; (i) wavelength_max, (ii) wavelength_localmax,
#' (iii) wavelength, (iv) palette name from colrr::col_pal, (v) color vector
#' @param col_em_ex color excitation and emission of one fluorochrome differently
#' (T) or both according max(em) (F)
#' @param type what to plot em and/or ex
#' @param shade_type what to shade: none, ex and/or em
#' @param shade_type_args args to geom_ribbon
#' @param geoms what to plot, line and/or point
#' @param plot_max plot vertical lines at local maxima
#' @param theme ggplot theme
#'
#' @returns ggplot
#' @export
#'
#' @examples
#' # select fluorochromes to be plotted together
#' data <- get_spectral_data(fluos = c("FITC", "PE-cy7"))
#' # default plot; color histograms by maximum wavelength (em or ex)
#' plot_spectra(data$bd)[[1]]
#' # points, no lines
#' plot_spectra(data$bd, geoms = "point")[[1]]
#' # omit vertical max lines
#' plot_spectra(data$bd, geoms = "point", plot_max = F)[[1]]
#' # points and lines
#' plot_spectra(data$bd, geoms = c("point", "line"), plot_max = F)[[1]]
#' # color each segment by respective wavelength
#' plot_spectra(spec_data = data$bd, col_pal = "wavelength", geoms = c("point", "line"), plot_max = F)[[1]]
#' # add shade fill according to max wavelength
#' plot_spectra(spec_data = data$bd, col_pal = "wavelength_max", shade_type = "ex")[[1]]
#' plot_spectra(spec_data = data$bd, col_pal = "wavelength_max", shade_type = c("ex", "em"))[[1]]
#' # add shade fill according to exact wavelength
#' plot_spectra(spec_data = data$bd, col_pal = "wavelength", shade_type = "ex")[[1]]
#' # use custom coloring
#' plot_spectra(data$bd, col_pal = "hue")[[1]]
#' plot_spectra(data$bd, col_pal = "custom")[[1]]
#' plot_spectra(data$bd, col_pal = c("hotpink", "firebrick", "forestgreen", "blue"))[[1]]
#' # plot all local maxima as legend
#' plot_spectra(data$bd, col_pal = "wavelength_localmax")[[1]]
#' # same color for ex and em
#' plot_spectra(data$bd, col_pal = "hue", col_em_ex = F)[[1]]
plot_spectra <- function(spec_data,
                         type = c("em", "ex"),
                         col_pal = "wavelength_max",
                         col_em_ex = T,
                         shade_type = c("none"),
                         shade_type_args = list(alpha = 0.3, show.legend = F),
                         geoms = "line",
                         plot_max = T,
                         theme = colrr::theme_material(text_fun = ggplot2::element_text,
                                                       bg_color = "grey40")
) {

  if (!requireNamespace("colrr", quietly = TRUE)) {
    stop("The 'colrr' package is required. Install it with:\n",
         "pak::pak('close-your-eyes/colrr')",
         call. = FALSE)
  }

  type <- rlang::arg_match(type, multiple = T)
  geoms <- rlang::arg_match(geoms, values = c("line", "point"), multiple = T)
  shade_type <- rlang::arg_match(shade_type, values = c("none", "ex", "em"), multiple = T)

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


    if (col_pal[1] == "wavelength_max") {
      max <- data_max |>
        dplyr::filter(col_em_ex | type == "em") |>
        dplyr::slice_max(norm_intensity, with_ties = F, by = !!rlang::sym(col_aes))
      palette <- stats::setNames(colrr::wl_to_hex(max[["nm"]]), max[[col_aes]])
    } else if (col_pal[1] == "wavelength") {
      palette <- colrr::wl_to_hex(unique(data[["nm"]]))
      # overwrite col_aes
      col_aes <- "nm"
    } else if (col_pal[1] == "wavelength_localmax") {
      # hack: make unique to allow plotting w/o more ifelse below
      # maybe fix legend
      data_max[["nm"]][duplicated(data_max[["nm"]])] <- data_max[["nm"]][duplicated(data_max[["nm"]])] + 0.1

      data <- split(data, data[["fluorochrome_type"]])
      data <- purrr::map_dfr(names(data), function(x) {
        data_max <- data_max |> dplyr::filter(fluorochrome_type == x)
        # find value of closest maximum
        data[[x]][["nm2"]] <- as.character(data_max[["nm"]][max.col(-abs(outer(data[[x]][["nm"]], data_max[["nm"]], "-")))])
        return(data[[x]])
      })
      palette <- stats::setNames(colrr::wl_to_hex(sort(unique(data_max[["nm"]]))), sort(unique(data_max[["nm"]])))
      data_max[["nm2"]] <- as.character(data_max[["nm"]])
      col_aes <- "nm2"
    } else if (length(col_pal) == 1) {
      palette <- colrr::col_pal(name = col_pal[1], n = unique(data[[col_aes]]), return = "c")
    } else {
      palette <- col_pal
    }


    p <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = nm, y = value)) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 2), order = 1),
                      linetype = ggplot2::guide_legend(override.aes = list(linewidth = 1, color="white", order = 2)),
                      shape = ggplot2::guide_legend(override.aes = list(size = 2.5, color="white", order = 2))) +
      theme +
      ggplot2::theme(
        plot.subtitle = ggplot2::element_text(size = 12),
        plot.caption = ggplot2::element_text(size = 6)
      ) +
      ggplot2::labs(
        x = "wavelength [nm]",
        y = "norm intensity [%]",
        caption = paste0("source: ", unique(data[["source"]]))
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(), limits = c(0,100)) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion())

    if (col_pal[1] %in% c("wavelength")) {
      p <- p + colrr::scale_color_spectral(colors = palette)
    } else {
      p <- p + ggplot2::scale_color_manual(values = palette)
    }


    if (plot_max) {
      p <- p +
        ggplot2::geom_segment(data = data_max,
                              mapping = ggplot2::aes(
                                x = nm,
                                y = 0,
                                yend = norm_intensity,
                                linetype = type,
                                color = !!rlang::sym(col_aes)),
                              show.legend = F)
    }


    if ("line" %in% geoms) {
      if (col_pal[1] == "wavelength") {
        p <- p + ggplot2::geom_line(mapping = ggplot2::aes(group = fluorochrome_type,
                                                           color = !!rlang::sym(col_aes)),
                                    show.legend = F)
      } else {
        p <- p + ggplot2::geom_line(mapping = ggplot2::aes(linetype = type,
                                                           color = !!rlang::sym(col_aes)))
      }
    }


    if ("point" %in% geoms) {
      p <- p + ggplot2::geom_point(mapping = ggplot2::aes(shape = type,
                                                          color = !!rlang::sym(col_aes)))
      if (col_pal[1] == "wavelength") {
        p <- p + ggplot2::guides(color = "none")
      }
    }



    if (!is.null(shade_type)) {
      shade_type <- intersect(shade_type, type)
      if (length(intersect(c("ex", "em"), shade_type))) {
        if (col_pal[1] == "wavelength") {
          for (i in unique(data[which(data$type %in% shade_type), "fluorochrome_type",drop = T])) {

            palette <- colrr::wl_to_hex(unique(dplyr::filter(data, fluorochrome_type == i) |> dplyr::pull(nm)))
            p <- p + do.call(ggplot2::geom_ribbon,
                             args = c(shade_type_args,
                                      list(data = dplyr::filter(data, fluorochrome_type == i),
                                           mapping = ggplot2::aes(ymin = 0,
                                                                  ymax = value,
                                                                  fill = !!rlang::sym(col_aes))))) +
              ggplot2::scale_fill_gradientn(colors = palette) +
              ggnewscale::new_scale_fill()
          }

        } else {

          for (i in unique(data[which(data$type %in% shade_type), "fluorochrome_type",drop = T])) {
            p <- p + do.call(ggplot2::geom_ribbon,
                             args = c(shade_type_args,
                                      list(data = dplyr::filter(data, fluorochrome_type %in% i),
                                           mapping = ggplot2::aes(ymin = 0,
                                                                  ymax = value,
                                                                  fill = !!rlang::sym(col_aes)))))
          }
          p <- p + ggplot2::scale_fill_manual(values = palette)
        }
      }
    }

    return(p)

  })
  return(plots)
}

