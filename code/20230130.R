# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(magick)
# Also requires {tibble}, {ggforce}, {ggpattern}, {ggfx}, {png}, {grid}

# Modifiable parameters --------------------------------------------------------

seed <- 9210
bg_colour <- "#011928"
palette_speckles <- c("#010D14", "#011927", "#033758", "#DE3817", "#F1917E", "#FBE0DA")

# Create data ------------------------------------------------------------------

# Square for gradient fill
df_square <- tibble::tibble(
  x = c(-2,-2,12,12),
  y = c(-2,12,12,-2))

# Polygon cutout for square gradient
set.seed(seed)
df_polygon_cutout <- tibble::tibble(
  x = c(
    sample(seq(-1,2.5, by = 0.2), 3),
    sample(seq(6,7.5, by = 0.2), 3)),
  y = c(
    sample(seq(0,2, by = 0.2), 1),
    sample(seq(4.5,5.5, by = 0.2), 1),
    sample(seq(6,7.5, by = 0.2), 2),
    sample(seq(4.5,5.5, by = 0.2), 1),
    sample(seq(0,2, by = 0.2), 1)))

# Split gradient - left side
df_polygon_left <- tibble::tibble(
  x = c(-2,-2,5,12,12),
  y = c(-2,12,12,0,-2))

# Split gradient - right side
df_polygon_right <- tibble::tibble(
  x = c(5,12,12),
  y = c(12,12,0))

# Circle cutout for split gradient
set.seed(seed)
df_circle_cutout <- tibble::tibble(
  x = sample(seq(6,8, by = 0.2), 1),
  y = sample(seq(5,7, by = 0.2), 1),
  r = sample(seq(1,3, by = 0.5), 1))

# Build plot -------------------------------------------------------------------

poly <- ggplot() +
  ggfx::as_reference(
    geom_polygon(
      data = df_polygon_cutout,
      aes(x = x, y = y)),
    id = "polygon") +
  ggfx::with_blend(
    ggpattern::geom_polygon_pattern(
      data = df_square,
      aes(x = x, y = y),
      pattern = "gradient", pattern_fill = "#033758", pattern_fill2 = "#B0DFFC",
      pattern_orientation = "vertical", fill = NA, colour = NA),
    bg_layer = "polygon",
    blend_type = "atop") +
  coord_cartesian(xlim = c(-2,12), ylim = c(-2,12), expand = FALSE) +
  theme_void()

circle <- ggplot() +
  ggforce::geom_circle(
    data = df_circle_cutout,
    aes(x0 = x, y0 = y, r = r),
    fill = "#000000", colour = NA) +
  coord_cartesian(xlim = c(-2,12), ylim = c(-2,12), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

gradients_for_circle <- ggplot() +
  ggpattern::geom_polygon_pattern(
    data = df_polygon_left,
    aes(x = x, y = y),
    pattern = "gradient", pattern_fill = "#F9D0C8", pattern_fill2 = "#DE3817",
    pattern_orientation = "horizontal", fill = NA, colour = NA) +
  ggpattern::geom_polygon_pattern(
    data = df_polygon_right,
    aes(x = x, y = y),
    pattern = "gradient", pattern_fill = "#370E06", pattern_fill2 = "#DE3817",
    pattern_orientation = "horizontal", fill = NA, colour = NA) +
  coord_cartesian(xlim = c(-2,12), ylim = c(-2,12), expand = FALSE) +
  theme_void()

# Save elements to file
ggsave(
  here::here("img/ingredients/20230130_poly.png"), poly,
  width = 10, height = 10, units = "in", dpi = 600)

ggsave(
  here::here("img/ingredients/20230130_circle.png"), circle,
  width = 10, height = 10, units = "in", dpi = 600)

ggsave(
  here::here("img/ingredients/20230130_grad_circle.png"), gradients_for_circle,
  width = 10, height = 10, units = "in", dpi = 600)

# Shift to {magick} workflow ---------------------------------------------------

img_poly <- image_read(here::here("img/ingredients/20230130_poly.png"))

img_circle <- image_read(here::here("img/ingredients/20230130_circle.png"))

img_grad_circle <- image_read(here::here("img/ingredients/20230130_grad_circle.png"))

# Blend gradient with circle
img_circle_blend <- image_flatten(
  c(img_grad_circle, img_circle), "Screen") %>%
  image_transparent("#FFFFFF")

# Blend circle gradient with polygon gradient
img_combined <- image_flatten(
  c(img_poly, img_circle_blend), "Over") %>%
  image_transparent("#000000") %>%
  image_noise(noisetype = "Laplacian") %>%
  image_noise(noisetype = "Laplacian") %>%
  image_noise(noisetype = "Laplacian")

image_write(
  img_combined,
  path = here::here("img/ingredients/20230130_combined.png"))

# Shift back to {ggplot2} workflow ---------------------------------------------

# Read in combined, gradient-filled shapes
fg <- png::readPNG(here::here("img/ingredients/20230130_combined.png"))
fg_raster <- grid::rasterGrob(fg, width = unit(1,"npc"), height = unit(1,"npc"))

# Create dusty speckles
set.seed(seed)
df_dust <- tibble::tibble(
  x = scales::rescale(rnorm(700), to = c(-12,22)),
  y = scales::rescale(rnorm(700), to = c(-12,22))) %>%
  mutate(
    colour_hex = sample(palette_speckles, n(), replace = TRUE),
    point_size = sample(seq(0.2,0.4, by = 0.05), n(), replace = TRUE))

# Build plot
final <- ggplot() +
  ggpattern::geom_polygon_pattern(
    data = df_square,
    aes(x = x, y = y),
    pattern = "gradient", pattern_fill = "#131313", pattern_fill2 = "#02253C",
    pattern_orientation = "vertical", fill = NA, colour = NA) +
  annotation_custom(fg_raster, -Inf, Inf, -Inf, Inf) +
  geom_point(
    data = df_dust,
    aes(x = x, y = y, colour = colour_hex, size = point_size),
    shape = 46) +
  scale_colour_identity() +
  scale_size_identity() +
  coord_cartesian(xlim = c(-2,12), ylim = c(-2,12), expand = FALSE) +
  theme_void()

# Save final image to file
ggsave(
  here::here("img/20230130.png"), final,
  width = 10, height = 10, units = "in", dpi = 600)
