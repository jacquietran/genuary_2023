# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Modifiable parameters --------------------------------------------------------

iteration_id <- "20230102"
seed_num <- 15780
dot_colour1 <- "#999999"
dot_colour2 <- "#00A5E0"
dot_colour3 <- "#CF1259"
dot_colour4 <- "#EF9CDA"
bg_colour <- "#222222"
  
# Derived parameters -----------------------------------------------------------

set.seed(seed_num)
seed_vec <- sample(seq(1:10000000), 2, replace = FALSE)
      
# Make some noise --------------------------------------------------------------

# Data set 1
set.seed(seed_vec[1])
grid1 <- long_grid(
  seq(0, 1, length.out = 70),
  seq(0, 1, length.out = 70)) %>%
  mutate(
    curl = curl_noise(gen_simplex, x = x, y = y)) %>%
  purrr::reduce(data.frame) %>%
  rename(x = out, y = elt, curl_x = x, curl_y = y) %>%
  mutate(
    x_warped = x + (curl_x / 100),
    y_warped = y + (curl_y / 100),
    size = sample(seq(0.2, 1.5, by = 0.1), n(), replace = TRUE),
    subset = sample(1:20, n(), replace = TRUE))

set.seed(seed_vec[2])
grid2 <- long_grid(
  seq(0, 1, length.out = 10),
  seq(0, 1, length.out = 10)) %>%
  mutate(
    curl = curl_noise(gen_simplex, x = x, y = y)) %>%
  purrr::reduce(data.frame) %>%
  rename(x = out, y = elt, curl_x = x, curl_y = y) %>%
  mutate(
    x_warped = x + (curl_x / 40),
    y_warped = y + (curl_y / 40),
    size = sample(seq(0.2, 1.5, by = 0.1), n(), replace = TRUE),
    subset = sample(1:20, n(), replace = TRUE))

# Build plot -------------------------------------------------------------------
    
ggplot() +
  geom_point(
    data = grid1,
    aes(x = x_warped, y = y_warped, size = size / 2),
    shape = 16, colour = dot_colour1) +
  geom_path(
    data = grid2 |> dplyr::filter(subset <= 5),
    aes(x = x_warped, y = y_warped, group = subset),
    colour = dot_colour2) +
  geom_path(
    data = grid2 |> dplyr::filter(subset >= 15),
    aes(x = x_warped, y = y_warped, group = subset),
    colour = dot_colour4) +
  geom_point(
    data = grid2,
    aes(x = x_warped, y = y_warped, size = size * 5),
    shape = 16, colour = dot_colour2) +
  scale_size_identity() +
  coord_equal(expand = TRUE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin     = margin(100,100,100,100, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  device = "png", width = 6000, height = 6000, units = "px", dpi = 600)
  