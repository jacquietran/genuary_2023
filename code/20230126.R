# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)
# Also requires {tibble}, {scales}, {here}
# 
# {ggfx}, {png}, {grid}

# Modifiable parameters --------------------------------------------------------

seed <- 858
n_shapes <- 60
n_lines <- 10
bg_colour <- "#E9EAED"
palette <- c("#FF311F", "#E4C14E", "#41DCBD", "#388894", "#BE3E82")

# Create data ------------------------------------------------------------------

set.seed(seed)
df_shapes <- tibble::tibble(
  group = 1:n_shapes,
  x = scales::rescale(rnorm(n_shapes), to = c(0,9)),
  y = scales::rescale(rnorm(n_shapes), to = c(0,12))) %>%
  mutate(
    point_colour_hex = sample(palette, n(), replace = TRUE))

set.seed(seed + 1)
df_specks <- tibble::tibble(
  group = 1:(n_shapes*8),
  x = scales::rescale(rnorm(n_shapes*8), to = c(0,12)),
  y = scales::rescale(rnorm(n_shapes*8), to = c(-6,16))) %>%
  mutate(
    point_colour_hex = sample(palette, n(), replace = TRUE))

df_cover <- tibble::tibble(
  x = c(0,0,6,3),
  y = c(0,12,12,0))

set.seed(seed)
df_diags <- tibble::tibble(
  x_start = seq(5,7, length.out = n_lines),
  x_end = sample(seq(-4,6, by = 0.2), n_lines, replace = TRUE),
  y_start = -1,
  y_end = 13) %>%
  mutate(
    diag_line_width = sample(seq(0.5,4, by = 0.5), n(), replace = TRUE))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_voronoi_tile(
    data = df_shapes,
    aes(x = x, y = y, fill = point_colour_hex),
    max.radius = 8) +
  geom_point(
    data = df_specks %>% filter(group < 20),
    aes(x = x, y = y, colour = point_colour_hex),
    shape = "\u25bc", size = 3) +
  geom_point(
    data = df_specks %>% filter(group >= 20 & group < 40),
    aes(x = x, y = y, colour = point_colour_hex),
    shape = "\u25ba", size = 3) +
  geom_point(
    data = df_specks %>% filter(group >= 40),
    aes(x = x, y = y, colour = point_colour_hex),
    shape = "\u25c4", size = 3) +
  geom_polygon(
    data = df_cover,
    aes(x = x, y = y),
    fill = bg_colour) +
  geom_diagonal(
    data = df_diags,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end,
        linewidth = diag_line_width)) +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_linewidth(trans = "identity") +
  coord_cartesian(xlim = c(0,9), ylim = c(0,12), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230126.png"), last_plot(),
  width = 9, height = 12, units = "in", dpi = 600)

