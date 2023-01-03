# Load libraries ---------------------------------------------------------------

library(tibble)
library(dplyr)
library(ambient)
library(ggplot2)
library(ggfx)
library(magick)
# {ggnewscale} also used

# Modifiable parameters --------------------------------------------------------

seed_num <- 8936514
palette <- c("#06AED5", "#086788", "#F0C808", "#FFF1D0", "#DD1C1A", "#131313")
bg_colour <- "#000000"
n_objects <- 500

# Derived parameters -----------------------------------------------------------

set.seed(seed_num)
seed_vec <- sample(1:10000000, 3, replace = FALSE)

# Function for generating square coordinates -----------------------------------

create_square_coord <- function(
    xmin_start, ymin_start, side_length, point_range, point_variation,
    palette, n_objects, seed_num){
  
  # Requires {tibble}, {dplyr}
  
  # Set arg defaults
  if(missing(xmin_start)){
    xmin_start <- 0
  }
  
  if(missing(ymin_start)){
    ymin_start <- 0
  }
  
  if(missing(side_length)){
    side_length <- 5
  }
  
  if(missing(point_range)){
    point_range <- 0.5
  }
  
  if(missing(point_variation)){
    point_variation <- 0.1*point_range
  }
  
  if(missing(n_objects)){
    n_objects <- 100
  }
  
  # Create data
  set.seed(seed_num)
  df <- tibble::tibble(
    xmin = sample(
      seq(xmin_start, xmin_start + point_range, by = point_variation),
      n_objects, replace = TRUE),
    ymin = sample(
      seq(ymin_start, ymin_start + point_range, by = point_variation),
      n_objects, replace = TRUE)) |>
    dplyr::mutate(
      add_amount = sample(
        seq(side_length, side_length + point_range, by = point_variation),
        n_objects, replace = TRUE),
      xmax = xmin + add_amount,
      ymax = ymin + add_amount,
      colour_hex = sample(palette, n_objects, replace = TRUE))
  
  return(df)
  
}

# Create data ------------------------------------------------------------------

# Noise backgrounds
set.seed(seed_vec[1])
noise_grid1 <- long_grid(
  x = seq(-4, 10, length.out = 100),
  y = seq(-4, 10, length.out = 100)) %>% 
  mutate(
    value = gen_white(x, y))

set.seed(seed_vec[2])
noise_grid2 <- long_grid(
  x = seq(-4, 10, length.out = 1000),
  y = seq(-4, 10, length.out = 1000)) %>% 
  mutate(
    value = gen_white(x, y))

set.seed(seed_vec[3])
noise_grid3 <- long_grid(
  x = seq(-4, 10, length.out = 100),
  y = seq(-4, 10, length.out = 100)) %>% 
  mutate(
    value = gen_white(x, y))

# Central focus
centre <- create_square_coord(
    palette = palette, n_objects = n_objects, seed_num = seed_vec[1])

# Central overlay
centre_overlay <- create_square_coord(
  palette = palette, n_objects = n_objects, seed_num = seed_vec[2])

# Offset element
offset_element <- create_square_coord(
  xmin_start = 4.5, ymin_start = -0.5, side_length = 2.5, point_range = 0.5,
  point_variation = 0.05, palette = palette, n_objects = n_objects,
  seed_num = seed_vec[3])

# Build plot -------------------------------------------------------------------

plot_base <- ggplot() +
  geom_raster(
    data = noise_grid1,
    ggplot2::aes(x = x, y = y, fill = value)) +
  scale_fill_gradient(low = "#000000", high = "#444444") +
  ggnewscale::new_scale_fill() +
  as_reference(
    geom_rect(
      data = centre,
      aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = colour_hex),
      alpha = 0.8, colour = NA),
    id = "base") +
  with_blend(
    geom_rect(
      data = centre_overlay,
      aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = colour_hex),
      alpha = 0.8, colour = NA),
    bg_layer = "base",
    blend_type = "darken",
    id = "blend1") +
  with_blend(
    geom_rect(
      data = centre_overlay,
      aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = colour_hex),
      alpha = 0.8, colour = NA),
    bg_layer = "blend1",
    blend_type = "color_dodge",
    id = "blend2") +
  with_blend(
    geom_rect(
      data = centre,
      aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = colour_hex),
      alpha = 0.8, colour = NA),
    bg_layer = "blend2",
    blend_type = "hard_light") +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  with_blend(
    geom_raster(
      data = noise_grid2,
      ggplot2::aes(x = x, y = y, fill = value)),
    bg_layer = "base",
    blend_type = "overlay") +
  scale_fill_gradient(low = "#000000", high = "#444444") +
  ggnewscale::new_scale_fill() +
  geom_rect(
    data = offset_element,
    aes(
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
      fill = colour_hex),
    alpha = 0.8, colour = NA) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  geom_raster(
    data = noise_grid3,
    ggplot2::aes(x = x, y = y, fill = value),
    alpha = 0.3) +
  scale_fill_gradient(low = "#000000", high = "#444444") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(-20,-20,-20,-20, unit = "pt"))

# Save base plot to file -------------------------------------------------------

ggsave(
  here::here("img/ingredients/20230103_base.png"), plot_base,
  width = 10, height = 10, units = "cm", dpi = 600)

# Shift to {magick} workflow ---------------------------------------------------

# Read in image
img <- image_read(here::here("img/ingredients/20230103_base.png"))

# Apply blending
img_blend1 <- image_flatten(
  c(img, img), "HardLight")
img_blend2 <- image_flatten(
  c(img, img_blend1), "LinearDodge")
img_blend3 <- image_flatten(
  c(img_blend1, img_blend2), "LinearDodge")
img_blend4 <- image_flatten(
  c(img_blend2, img_blend3), "HardLight")
img_blend5 <- image_flatten(
  c(img_blend3, img_blend4), "Screen")

# Export
image_write(
  img_blend5,
  path = here::here("img/20230103.png"),
  format = "png")
