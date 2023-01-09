# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)
# Also requires {tibble}

# Modifiable parameters --------------------------------------------------------

initial_seed <- 95867861
n_points_per_tri <- 6
n_triangles <- 36

shading_fill <- "#9EBC9F"
shading_colour <- "#C3D5C3"
line_colour <- "#230903"
grid_colour <- "#9B8B6F"
bg_colour <- "#D3B88C"
  
# Derived parameters -----------------------------------------------------------

set.seed(initial_seed)
seed_vec <- sample(seq(1:10000000), 2, replace = FALSE)

# Create data ------------------------------------------------------------------

# Lines
set.seed(seed_vec[1])
df_lines <- tibble::tibble(
  x = sample(
    seq(1, 10, by = 0.2), (n_triangles * n_points_per_tri), replace = TRUE),
  y = sample(
    seq(1, 10, by = 0.2), (n_triangles * n_points_per_tri), replace = TRUE),
  group = rep(1:n_triangles, each  = n_points_per_tri))

# Shading
set.seed(seed_vec[2])
df_shading <- tibble::tibble(
  x = sample(
    seq(1, 10, by = 0.2), (n_triangles * n_points_per_tri), replace = TRUE),
  y = sample(
    seq(1, 10, by = 0.2), (n_triangles * n_points_per_tri), replace = TRUE),
  group = rep(1:n_triangles, each  = n_points_per_tri))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_delaunay_tile(
    data = df_shading,
    aes(x = x, y = y),
    fill = shading_fill, colour = shading_colour, alpha = 0.4) +
  geom_delaunay_segment2(
    data = df_lines,
    aes(x = x, y = y),
    colour = line_colour, linewidth = 1, n = 500, lineend = "round") +
  facet_wrap(~group, ncol = 6) +
  theme_void() +
  theme(
    panel.grid = element_line(linewidth = 0.3, colour = grid_colour),
    strip.text = element_blank(),
    panel.spacing = unit(2, "lines"),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(160,160,160,160, unit = "pt"))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230104.png"), last_plot(),
  width = 16, height = 16, units = "in", dpi = 600)