# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
# Also requires {tibble}, {scales}, {ggfx}

# Define custom function -------------------------------------------------------

blotchy <- function(seed, n_points, x_min, x_max, y_min, y_max, palette){
  
  if(missing(n_points)){
    n_points <- 1000
  }
  
  if(missing(x_min)){
    x_min <- 0
  }
  
  if(missing(x_max)){
    x_max <- 1
  }
  
  if(missing(y_min)){
    y_min <- 0
  }
  
  if(missing(y_max)){
    y_max <- 1
  }
  
  # Requires {tibble}, {scales}, {dplyr}
  
  set.seed(seed)
  df <- tibble::tibble(
    x = scales::rescale(rnorm(n_points), to = c(x_min, x_max)),
    y = scales::rescale(rnorm(n_points), to = c(x_min, x_max))) |>
    dplyr::mutate(
      colour_hex = sample(palette, dplyr::n(), replace = TRUE))
  
  return(df)
  
}

# Modifiable parameters --------------------------------------------------------

seed <- 9210
bg_colour <- "#040D10"
glow_hex <- "#F6F1D1"
palette <- c("#07151A", "#09191F", "#0A1D23", "#0D242B", "#132E36")

# Create data ------------------------------------------------------------------

df_square <- tibble::tibble(
  x = c(0,0,10,10),
  y = c(0,10,10,0))

df_blotches <- blotchy(
  seed = seed, n_points = 4000, palette = palette,
  x_min = -4, x_max = 14, y_min = -4, y_max = 14)

# Build plot -------------------------------------------------------------------

ggplot() +
  ggfx::with_blur(
    geom_polygon(
      data = df_square,
      aes(x = x, y = y),
      fill = glow_hex),
    sigma = 50) +
  ggfx::as_reference(
    geom_polygon(
      data = df_square,
      aes(x = x, y = y),
      fill = bg_colour),
    id = "base") +
  ggfx::with_blend(
    geom_point(
      data = df_blotches,
      aes(x = x, y = y, colour = colour_hex),
      shape = 16, size = 30, alpha = 0.1),
    bg_layer = "base",
    blend_type = "atop") +
  scale_colour_identity() +
  coord_cartesian(xlim = c(-4,14), ylim = c(-4,14), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230130.png"), last_plot(),
  width = 10, height = 10, units = "cm", dpi = 600)
