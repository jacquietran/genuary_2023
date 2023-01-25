# Load libraries ---------------------------------------------------------------

library(tibble)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(magick)

# Create data ------------------------------------------------------------------

df_base <- tibble(
  group = c(
    rep(c("outer_border", "thick_border", "inner_border"), each = 6),
    rep("corner_triangle", times = 3),
    rep("middle_fill", times = 5)),
  x = c(
    0,0,0.5,0.5,4.5,4.5,     # Outer border
    0.5,0.5,1.5,1.5,4.5,4.5, # Thick border
    1.5,1.5,2,2,4.5,4.5,     # Inner border
    2,2,4,                   # Corner triangle
    2,2,4.5,4.5,4            # Middle fill
  ),
  y = c(
    0,6,6,0.5,0.5,0,         # Outer border
    0.5,6,6,1.5,1.5,0.5,     # Thick border
    1.5,6,6,2,2,1.5,         # Inner border
    2,4,2,                   # Corner triangle
    4,6,6,2,2                # Middle fill
  ),
  colour_hex = c(
    # Outer border - Thick border - Inner border
    rep(c("#1F1A38", "#1B998B", "#1F1A38"), each = 6),
    # Corner triangle
    rep("#DD99BB", times = 3),
    # Middle fill
    rep("#CC2936", times = 5))
)

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_polygon_pattern(
    data = df_base |> filter(group %in% c("outer_border", "inner_border", "thick_border")),
    aes(x = x, y = y, group = group, fill = colour_hex,
        pattern_fill = colour_hex, pattern_shape = group),
    pattern = "pch", pattern_colour = "#3D3370", pattern_density = 0.7) +
  geom_polygon_pattern(
    data = df_base |> filter(group == "corner_triangle"),
    aes(x = x, y = y, group = group, pattern_fill = colour_hex),
    pattern = "weave", pattern_type = "twill", fill = "#D685AD", pattern_colour = "#D685AD") +
  geom_polygon_pattern(
    data = df_base |> filter(group == "middle_fill"),
    aes(x = x, y = y, group = group, fill = colour_hex, pattern_fill = colour_hex),
    pattern = "wave", pattern_type = "triangle", pattern_colour = "#991E29") +
  scale_fill_identity() +
  scale_pattern_fill_identity() +
  coord_fixed(xlim = c(0,4.5), ylim = c(0,6), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none")

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/ingredients/20230121_corner.png"), last_plot(),
  width = 4.5, height = 6, units = "in", dpi = 600)

# Shift to {magick} workflow ---------------------------------------------------

img_bottom_left <- image_read(here::here("img/ingredients/20230121_corner.png"))

img_top_left <- image_flip(img_bottom_left)
img_top_right <- image_flop(img_top_left)
img_bottom_right <- image_flop(img_bottom_left)

img_app_left <- image_append(c(img_top_left, img_bottom_left), stack = TRUE)
img_app_right <- image_append(c(img_top_right, img_bottom_right), stack = TRUE)
img_combined <- image_append(c(img_app_left, img_app_right), stack = FALSE)
