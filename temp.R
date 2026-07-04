devtools::load_all()
library(ggplot2)
data <- get_spectral_data(fluos = c("FITC", "PE-cy7"))

ggplot(data$bd |> dplyr::mutate(fluorochrome_type = paste0(fluorochrome, "_", type)), aes(x = nm, y = value)) +
  ggplot2::geom_line(ggplot2::aes(group = fluorochrome_type, color = nm))  +
  colrr::theme_material() +
  colrr::scale_color_spectral()

ggplot(data$bd |> dplyr::mutate(fluorochrome_type = paste0(fluorochrome, "_", type)), aes(x = nm, y = value)) +
  ggplot2::geom_line(ggplot2::aes(group = fluorochrome_type, color = nm))  +
  colrr::theme_material() +
  colrr::scale_color_spectral(colors = colrr::wl_to_hex(unique(data$bd$nm)))

plot_spectra(data$bd)[[1]]
plot_spectra(data$bd, geoms = "point")[[1]]
plot_spectra(data$bd, geoms = "point", plot_max = F)[[1]]
plot_spectra(data$bd, geoms = c("point", "line"), plot_max = F)[[1]]
plot_spectra(spec_data = data$bd, col_pal = "wavelength", geoms = c("point", "line"), plot_max = F)[[1]]
plot_spectra(spec_data = data$bd, col_pal = "wavelength", geoms = c("point", "line"), plot_max = T)[[1]]
plot_spectra(spec_data = data$bd, col_pal = "wavelength_max", geoms = c("point", "line"), plot_max = T, shade_type = "em")[[1]]
plot_spectra(spec_data = data$bd, col_pal = "wavelength", geoms = c("point", "line"), plot_max = T, shade_type = "em")[[1]]
plot_spectra(spec_data = data$bd, col_pal = "wavelength", geoms = c("line"), plot_max = F)[[1]]
plot_spectra(data$bd, col_pal = "hue")[[1]]
plot_spectra(data$bd, col_pal = "custom")[[1]]
plot_spectra(data$bd, col_pal = c("hotpink", "firebrick", "forestgreen", "blue"))[[1]]
plot_spectra(spec_data = data$bd, col_pal = "wavelength_localmax")[[1]]

data <- get_spectral_data(fluos = c("FITC", "PE-cy7"))
plot_spectra(data$bd)[[1]] +
  add_shade2(fluo = "FITC") +
  scale_fill_manual(values = "white")

data <- get_spectral_data(fluos = c("FITC", "PE-cy7"))
pp <- plot_spectra(data$bd)[[1]]
add_shade(pp,
          fluo = "FITC",
          range = "515/50",
          annotate_pct = T)
add_shade(pp,
          fluo = "FITC",
          annotate_pct = T)

plot_spectra(data$bd, col_em_ex = T)[[1]]
plot_spectra(data$bd, col_pal = "hue", col_em_ex = F)[[1]]
plot_spectra(data$bd, col_em_ex = T, shade_type = c("em", "ex"))[[1]]
plot_spectra(data$bd, col_pal = "hue", col_em_ex = T)[[1]]

library(dplyr)
df <- data$bd
emission <- df %>%
  filter(type == "em")

emission <- emission %>%
  group_by(fluorochrome) %>%
  mutate(value = value / max(value))

integrate_filter <- function(data, fluor, min_nm, max_nm) {

  sub <- data %>%
    dplyr::filter(fluorochrome == fluor,
                  nm >= min_nm,
                  nm <= max_nm) %>%
    dplyr::arrange(nm)

  pracma::trapz(sub$nm, sub$value)
}

# FITC spill into PE channel

fitc_in_pe <- integrate_filter(emission, "FITC", 564, 606)
fitc_in_fitc <- integrate_filter(emission, "FITC", 515, 545)

spillover_fraction <- fitc_in_pe / fitc_in_fitc
spillover_percent <- spillover_fraction * 100


# how much light does filter capture? relative?
integrate_em_spectrum(data)
integrate_em_spectrum(data, 515, 530)
filter_pct(data, range = "515/30")






data <- get_spectral_data(c("Resazurin","PE"))[[1]]

rszr <- plot_spectra(get_spectral_data(c("Resazurin"))[[2]], col_em_ex = F)[[1]] +
  add_filter(range = "584/15") +
  ggplot2::xlim(c(400,680))
rsfr <- plot_spectra(get_spectral_data(c("Resorufin"))[[2]], col_em_ex = F)[[1]] +
  add_filter(range = "584/15") +
  ggplot2::xlim(c(400,680))
pe <- plot_spectra(get_spectral_data(c("PE"))[[2]], col_em_ex = F)[[1]] +
  add_filter(range = "584/15") +
  ggplot2::xlim(c(400,680))

cowplot::plot_grid(rszr, rsfr, pe, ncol = 1)

plot_spectra(get_spectral_data(c("Resorufin")), col_em_ex = F)
