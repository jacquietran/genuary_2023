# Source custom function -------------------------------------------------------

source(here::here("R/fx_release.R"))

# Set parameters ---------------------------------------------------------------

iteration_id <- "release_0001"
seed_num <- 1574301
bg_colour <- "#387780"
line_colour <- "#E2F1F3"
# for line_colour, use coolors - specify bg_colour, pick 2 stops back from white

# Create data ------------------------------------------------------------------

release <- release_1x1(seed = seed_num, lines = 2, control_points = 6)

# Plot -------------------------------------------------------------------------

# Build plot
p <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = release,
    ggplot2::aes(x = x, y = y, group = group, size = index, alpha = index),
    shape = 16, colour = line_colour) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    plot.background = ggplot2::element_rect(
      fill = bg_colour, colour = bg_colour),
    plot.margin = ggplot2::margin(-2,-2,-2,-2, unit = "cm"))

# Export to file
ggplot2::ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  p, width = 20, height = 20, units = "cm", dpi = 600, device = ragg::agg_png)
