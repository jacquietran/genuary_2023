# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggpattern)
# Also requires {tibble}, {scales}, {here}, {ggforce}, {ggpattern}, {ggfx},
# {png}, {grid}

# Modifiable parameters --------------------------------------------------------

seed <- 857
n_shapes <- 60
bg_colour <- "#E9EAED"
palette <- c("#FF311F", "#E4C14E", "#41DCBD", "#388894", "#BE3E82")

# Create data ------------------------------------------------------------------

# Prep function for detecting points above and below a diagonal line
straight_line <- tibble::tibble(
  x = seq(3,7, length.out = 11),
  y = 0:10)
detect_point <- approxfun(x = straight_line$x, y = straight_line$y)

set.seed(seed)
df_shapes <- tibble::tibble(
  group = 1:n_shapes,
  x = scales::rescale(rnorm(n_shapes), to = c(0,9)),
  y = scales::rescale(rnorm(n_shapes), to = c(0,12))) %>%
  mutate(
    below_line = detect_point(y) > x,
    keep_shape = case_when(
      below_line == TRUE  ~ "drop",
      below_line == FALSE ~ sample(
        c("keep", "drop"), replace = TRUE, n(), prob = c(0.9,0.1)),
      TRUE                ~ "keep")) %>%
  filter(keep_shape == "keep") %>%
  select(-below_line, -keep_shape) %>%
  mutate(
    point_size = sample(seq(40,90, by = 10), n(), replace = TRUE),
    point_colour_hex = sample(palette, n(), replace = TRUE))

df_pattern_shape <- tibble::tibble(
  x = c(0,0,9,9),
  y = c(0,12,12,0),
  group = 1)

df_overlay_shape <- tibble::tibble(
  x = c(5,1,5,6.5),
  y = c(0,12,12,0),
  group = 1)

# Build plot -------------------------------------------------------------------

# Colourful shapes
plot_shapes <- ggplot() +
  # Squares
  geom_point(
    data = df_shapes %>% filter(group < 15),
    aes(x = x, y = y, group = group, size = point_size, colour = point_colour_hex),
    shape = 15, alpha = 0.7) +
  # Triangles
  geom_point(
    data = df_shapes %>% filter(group >= 15 & group < 35),
    aes(x = x, y = y, group = group, size = point_size, colour = point_colour_hex),
    shape = 17, alpha = 0.7) +
  # Circles
  geom_point(
    data = df_shapes %>% filter(group >= 35),
    aes(x = x, y = y, group = group, size = point_size, colour = point_colour_hex),
    shape = 16, alpha = 0.7) +
  coord_cartesian(xlim = c(0,9), ylim = c(0,12), expand = FALSE) +
  scale_size_identity() +
  scale_colour_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Pattern-filled polygon
plot_pattern <- ggplot() +
  # Polygon shape
  ggfx::as_reference(
    ggforce::geom_diagonal_wide(
      data = df_overlay_shape,
      aes(x = x, y = y, group = group),
      orientation = "y"),
    id = "diag") +
  # Polygon pattern
  ggfx::with_blend(
    ggpattern::geom_polygon_pattern(
      data = df_pattern_shape,
      aes(x = x, y = y, group = group),
      pattern = "weave", pattern_type = "plain", colour = '#000000',
      pattern_density = 1, pattern_fill = "#000000", pattern_fill2 = "#FFFFFF"),
    bg_layer = "diag",
    blend_type = "atop") +
  coord_cartesian(xlim = c(0,9), ylim = c(0,12), expand = FALSE) +
  scale_size_identity() +
  scale_colour_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/ingredients/20230126_shapes.png"), plot_shapes,
  width = 9, height = 12, units = "in", dpi = 600)

ggsave(
  here::here("img/ingredients/20230126_pattern.png"), plot_pattern,
  width = 9, height = 12, units = "in", dpi = 600)

# Blend pieces -----------------------------------------------------------------

# Import paper texture image
shapes <- png::readPNG(here::here("img/ingredients/20230126_shapes.png"))
shapes_raster <- grid::rasterGrob(shapes, width = unit(1,"npc"), height = unit(1,"npc"))

pattern <- png::readPNG(here::here("img/ingredients/20230126_pattern.png"))
pattern_raster <- grid::rasterGrob(pattern, width = unit(1,"npc"), height = unit(1,"npc"))

plot_combined <- ggplot() +
  ggfx::as_reference(
    annotation_custom(shapes_raster, -Inf, Inf, -Inf, Inf),
    id = "base") +
  ggfx::with_blend(
    annotation_custom(pattern_raster, -Inf, Inf, -Inf, Inf),
    bg_layer = "base",
    blend_type = "multiply") +
  coord_cartesian(expand = FALSE) +
  theme(
    plot.margin = margin(-1,-1,-1,-1, unit = "cm"))

ggsave(
  here::here("img/20230126.png"), plot_combined,
  width = 9, height = 12, units = "in", dpi = 600)