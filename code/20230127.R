# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(scales)
# Also requires {tibble}, {ggforce}, {scales}, {ggfx}

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

initial_seed <- 1102
bg_colour <- "#EBE2D5"
outline_colour <- "#7F2C24"
palette1 <- c("#E68D57", "#DF8256", "#D87755")
palette2 <- c("#F4B264", "#F4A259", "#ED9858")
palette3 <- c("#F4E285", "#F4D27A", "#F4C26F")
palette4 <- c("#3793BC", "#43A6CA", "#4EB9D7")
palette5 <- c("#5ACCE4", "#65DEF1", "#87DDE1")
palette6 <- c("#98DDD9", "#A8DCD1", "#B5DECF")

# Derived parameters -----------------------------------------------------------

set.seed(initial_seed)
seed_vec <- sample(1:5000000, 6, replace = FALSE)

# Create data: Left side -------------------------------------------------------

# Blots
blots_left1 <- blotchy(
  seed = seed_vec[1], n_points = 4000, palette = palette1)
blots_left2 <- blotchy(
  seed = seed_vec[2], n_points = 4000, palette = palette2)
blots_left3 <- blotchy(
  seed = seed_vec[3], n_points = 4000, palette = palette3)

blots_merged_left <- dplyr::bind_rows(
  blots_left1, blots_left2, blots_left3)

# Semi circle
semi_circle_left <- data.frame(
  start = pi,
  end = 2*pi,
  r = 0.2)

# Create data: Right side ------------------------------------------------------

# Blots
blots_right1 <- blotchy(
  seed = seed_vec[4], n_points = 4000, palette = palette4)
blots_right2 <- blotchy(
  seed = seed_vec[5], n_points = 4000, palette = palette5)
blots_right3 <- blotchy(
  seed = seed_vec[6], n_points = 4000, palette = palette6)

blots_merged_right <- dplyr::bind_rows(
  blots_right1, blots_right2, blots_right3)

# Semi circle
semi_circle_right <- data.frame(
  start = 0,
  end = pi,
  r = 0.15)

semi_circle_right_outline <- data.frame(
  start = 0,
  end = pi,
  r = 0.2)

# Build plot -------------------------------------------------------------------

ggplot() +
  ggfx::as_reference(
    ggforce::geom_arc_bar(
      data = semi_circle_left,
      aes(x0 = 0.5, y0 = 0.5, r0 = r - r, r = r, start = start, end = end),
      fill = bg_colour, colour = NA),
    id = "base_left") +
  ggfx::with_blend(
    geom_point(
      data = blots_merged_left,
      aes(x = x, y = y, colour = colour_hex),
      shape = 16, size = 20, alpha = 0.1),
    bg_layer = "base_left",
    blend_type = "atop") +
  ggfx::as_reference(
    ggforce::geom_arc_bar(
      data = semi_circle_right,
      aes(x0 = 0.5, y0 = 0.5, r0 = r - r, r = r, start = start, end = end),
      fill = bg_colour, colour = NA),
    id = "base_right") +
  ggfx::with_blend(
    geom_point(
      data = blots_merged_right,
      aes(x = x, y = y, colour = colour_hex),
      shape = 16, size = 12, alpha = 0.1),
    bg_layer = "base_right",
    blend_type = "atop") +
  ggforce::geom_arc_bar(
    data = semi_circle_right_outline,
    aes(x0 = 0.5, y0 = 0.5, r0 = r - r, r = r, start = start, end = end),
    fill = NA, colour = outline_colour) +
  scale_colour_identity() +
  coord_cartesian(xlim = c(0.27,0.72), ylim = c(0.27,0.72), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(5,5,5,5, unit = "cm"),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230127.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)
