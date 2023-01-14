# Load libraries ---------------------------------------------------------------

library(ggplot2)
# Also requires {ggfx}, {scales}, and
# {dplyr}, {tibble}, {tidyr}, {stringr}, {tidyselect} are used in custom function

# Source custom function -------------------------------------------------------

source(here::here("code/functions/triforce.R"))

# Modifiable parameters --------------------------------------------------------

seed <- 715
n_across <- 8

bg_colour <- "#E7EFEF"
palette1 <- c("#034563", "#8E3B46")
palette2 <- c("#D3A736", "#B1713E")
palette3 <- c("#77ACA2", "#6C6060")

# Create data ------------------------------------------------------------------

divvied_up <- triforce(
  n_across = n_across, seed = seed,
  palette_list = list(palette1, palette2, palette3))

set.seed(seed)
polka <- tibble::tibble(
  x = scales::rescale(rnorm(20000), to = c(0-n_across, n_across*2)),
  y = scales::rescale(rnorm(20000), to = c(0-n_across, n_across*2))) |>
  dplyr::mutate(
    colour_hex = sample(
      c(bg_colour, palette1, palette2, palette3), dplyr::n(), replace = TRUE))

# Build plot -------------------------------------------------------------------

ggplot() +
  ggfx::as_reference(
    geom_polygon(
      data = divvied_up,
      aes(x = x, y = y, group = element_id, fill = colour_hex)),
    id = "base") +
  ggfx::with_blend(
    geom_point(
      data = polka,
      aes(x = x, y = y, colour = colour_hex),
      size = 0.25, shape = 15),
    bg_layer = "base",
    blend_type = "atop") +
  scale_colour_identity() +
  ggnewscale::new_scale_colour() +
  geom_polygon(
    data = divvied_up |> dplyr::filter(solid_fill == "yes"),
    aes(x = x, y = y, group = element_id, fill = colour_hex),
    colour = bg_colour, linewidth = 1.5) +
  geom_polygon(
    data = divvied_up |> dplyr::filter(solid_fill == "no"),
    aes(x = x, y = y, group = element_id),
    fill = NA, colour = bg_colour, linewidth = 1.5) +
  scale_fill_identity() +
  theme_void() +
  coord_cartesian(
    xlim = c(0,n_across), ylim = c(0,n_across), expand = FALSE) +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(6,4,6,4, unit = "cm"))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230117.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)
