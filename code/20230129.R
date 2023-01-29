# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(magick)
# Also requires {tibble}, {scales}, {ggforce}

# Modifiable parameters --------------------------------------------------------

initial_seed <- 8021
# 5-colour palettes
palette1 <- c("#2A2A28", "#373734", "#434340", "#616160", "#89898B")
palette2 <- c("#EB9486", "#F4B942", "#97D8C4", "#6B9AC4", "#4059AD")

# Derived parameters -----------------------------------------------------------

set.seed(initial_seed)
seed_vec <- sample(1:5000000, 6, replace = FALSE)

# Set custom function ----------------------------------------------------------

cranes <- function(seed, palette, n_points, rescale_min, rescale_max){
  
  # Requires {tibble}, {scales}, {dplyr}
  
  if(missing(n_points)){
    n_points <- 8000
  }
  
  if(missing(rescale_min)){
    rescale_min <- 0
  }
  
  if(missing(rescale_max)){
    rescale_max <- 10
  }
  
  set.seed(seed)
  df <- tibble::tibble(
    group = rep(1:(n_points / 4), each = 4),
    x_walk_abs = c(scales::rescale(rnorm(n_points), to = c(0.01, 0.2))),
    y_walk_abs = c(scales::rescale(rnorm(n_points), to = c(0.01, 0.2)))) |>
    dplyr::mutate(
      x_dir = sample(c("plus", "minus"), dplyr::n(), replace = TRUE),
      y_dir = sample(c("plus", "minus"), dplyr::n(), replace = TRUE),
      x_walk = dplyr::case_when(
        x_dir == "minus" ~ -x_walk_abs,
        TRUE             ~ x_walk_abs),
      y_walk = dplyr::case_when(
        y_dir == "minus" ~ -y_walk_abs,
        TRUE             ~ y_walk_abs),
      x = scales::rescale(cumsum(x_walk), to = c(rescale_min, rescale_max)),
      y = scales::rescale(cumsum(y_walk), to = c(rescale_min, rescale_max)),
      colour_hex = dplyr::case_when(
        y < 2.5          ~ sample(
          palette, dplyr::n(), replace = TRUE, prob = c(0.5,0.2,0.15,0.075,0.075)),
        y >= 2.5 & y < 5 ~ sample(
          palette, dplyr::n(), replace = TRUE, prob = c(0.25,0.4,0.15,0.1,0.1)),
        y >= 5 & y < 7.5 ~ sample(
          palette, dplyr::n(), replace = TRUE, prob = c(0.1,0.1,0.15,0.4,0.25)),
        y >= 7.5         ~ sample(
          palette, dplyr::n(), replace = TRUE, prob = c(0.075,0.075,0.15,0.2,0.5))))
  
  return(df)
  
}

# Create data ------------------------------------------------------------------

df1 <- cranes(
  seed = seed_vec[1], palette = palette2, n_points = 1000)

df2 <- cranes(
  seed = seed_vec[2], palette = rev(palette2), n_points = 2000)

df3 <- cranes(
  seed = seed_vec[3], palette = palette2, n_points = 4000)

df_lines1 <- cranes(
  seed = seed_vec[4], palette = palette1, n_points = 4000)

df_lines2 <- cranes(
  seed = seed_vec[5], palette = palette1, n_points = 4000)

df_lines3 <- cranes(
  seed = seed_vec[6], palette = palette1, n_points = 4000)

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_path(
    data = df_lines1,
    aes(x = x, y = y, colour = colour_hex, group = group),
    linewidth = 0.2) +
  geom_path(
    data = df_lines2,
    aes(x = x, y = y, colour = colour_hex),
    linewidth = 0.125) +
  geom_path(
    data = df_lines3,
    aes(x = x, y = y, colour = colour_hex),
    linewidth = 0.125) +
  ggforce::geom_diagonal_wide(
    data = df1,
    aes(x = x, y = y, fill = colour_hex, group = group),
    colour = NA) +
  ggforce::geom_diagonal_wide(
    data = df2,
    aes(x = x, y = y, fill = colour_hex, group = group),
    colour = NA) +
  ggforce::geom_diagonal_wide(
    data = df3,
    aes(x = x, y = y, fill = colour_hex, group = group),
    colour = NA) +
  scale_colour_identity() +
  scale_fill_identity() +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    plot.margin = margin(-40,-50,-40,-40, unit = "cm"),
    plot.background = element_rect(fill = "#131313", colour = "#131313"))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230129.png"), last_plot(),
  width = 12, height = 9, units = "in", dpi = 600)
