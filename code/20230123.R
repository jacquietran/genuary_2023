# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Modifiable parameters --------------------------------------------------------

iteration_id <- "20230123"
seed_num <- 61022
bg_colour <- "#142429"
initial_grid_size <- 100

# Derived parameters -----------------------------------------------------------

set.seed(seed_num)
seed_vec <- sample(1:50000000, 3, replace = FALSE)

set.seed(seed_num)
warp_vec <- sample(seq(10, 30, by = 2), 3, replace = FALSE)

warp_df <- tibble::tibble(
  seed_num = seed_vec,
  warp_factor = warp_vec)

# Make some noise --------------------------------------------------------------

grids <- purrr::map_df(seed_vec, function(i){
  
  # Requires {ambient}, {dplyr}, {purrr}
  
  # Extract warp factor based on seed number
  warp_val <- warp_df |>
    dplyr::filter(seed_num == i) |>
    dplyr::pull(warp_factor)
  
  set.seed(i)
  grid1 <- ambient::long_grid(
    seq(0, 1, length.out = initial_grid_size),
    seq(0, 1, length.out = initial_grid_size)) |>
    dplyr::mutate(
      curl = ambient::curl_noise(ambient::gen_simplex, x = x, y = y)) |>
    purrr::reduce(data.frame) |>
    dplyr::rename(x = out, y = elt, curl_x = x, curl_y = y) |>
    dplyr::mutate(
      x_warped = x + (curl_x / warp_val),
      y_warped = y + (curl_y / warp_val),
      seed_num = i)
  
})

grids_tidy <- grids %>%
  mutate(
    colour_hex = case_when(
      seed_num == seed_vec[1] ~ "#9E2A2B",
      seed_num == seed_vec[2] ~ "#E09F3E",
      seed_num == seed_vec[3] ~ "#F0C977"))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_path(
    data = grids_tidy,
    aes(x = y, y = x_warped, group = seed_num, colour = colour_hex),
    linewidth = 0.4, lineend = "square", linejoin = "mitre") +
  facet_wrap(~seed_num, ncol = 1) +
  scale_colour_identity() +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing = unit(4, "lines"),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(60,120,60,120, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  device = "png", width = 4000, height = 6000, units = "px", dpi = 600)
