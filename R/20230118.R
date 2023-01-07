# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Modifiable parameters --------------------------------------------------------

iteration_id <- "20230118"
seed_num <- 3511
dot_colour <- "#AFDEDC"
bg_colour <- "#19647E"
initial_grid_size <- 70
warp_factor <- 10 # closer to zero = more warping

# Make some noise --------------------------------------------------------------

# Data set 1
set.seed(seed_num)
grid <- long_grid(
  seq(0, 1, length.out = initial_grid_size),
  seq(0, 1, length.out = initial_grid_size)) %>%
  mutate(
    curl = curl_noise(gen_simplex, x = x, y = y)) %>%
  purrr::reduce(data.frame) %>%
  rename(x = out, y = elt, curl_x = x, curl_y = y) %>%
  mutate(
    x_warped = x + (curl_x / warp_factor),
    y_warped = y + (curl_y / warp_factor),
    subset = sample(1:20, n(), replace = TRUE))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_path(
    data = grid,
    aes(x = y, y = x_warped),
    colour = dot_colour, linewidth = 1, lineend = "square", linejoin = "mitre") +
  coord_polar() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  device = "png", width = 6000, height = 6000, units = "px", dpi = 600)
