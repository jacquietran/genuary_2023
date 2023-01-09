# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Source custom function -------------------------------------------------------

devtools::source_url(
  "https://raw.githubusercontent.com/jacquietran/art_noise_studies/main/R/functions/weave_noise.R")

# Modifiable parameters --------------------------------------------------------

iteration_id <- "20230111"
seed_num <- 663452
initial_grid_size <- 5
warp_factor <- 20 # lower = more warping

bg_colour <- "#FFD791"
colour_vec1 <- c("#9C6615", "#0E79B2", "#CDD1DE", "#EF2E38", "#CD798A") # 5 cols
colour_vec2 <- c("#303A2B", "#785589", "#F3F7F0", "#2C2B3C")
outline_colour1 <- "#292214"
outline_colour2 <- "#F3F7F0"

# Make some noise --------------------------------------------------------------

# Generate data
grid <- weave_noise(
  seed = seed_num, x_max = 1, y_max = 1.5, grid_length = initial_grid_size,
  warp_factor = warp_factor, geom_size_min = 12, geom_size_max = 20,
  noise_type = "simplex")

# Add colours
set.seed(seed_num)
grid <- grid %>%
  mutate(
    colour1 = case_when(
      y <= 0.25*max(grid$y)      ~ sample(
          colour_vec1, n(), replace = TRUE,
          prob = c(0.7, 0.15, 0.1, 0.025, 0.025)),
      y <= 0.5*max(grid$y) &
        y > 0.25*max(grid$y)     ~ sample(
          colour_vec1, n(), replace = TRUE,
          prob = c(0.5, 0.2, 0.15, 0.075, 0.075)),
      y <= 0.75*max(grid$y) &
        y > 0.5*max(grid$y)     ~ sample(
          colour_vec1, n(), replace = TRUE,
          prob = c(0.3, 0.25, 0.2, 0.125, 0.125)),
      y > 0.75*max(grid$y)      ~ sample(
          colour_vec1, n(), replace = TRUE,
          prob = c(0.1, 0.3, 0.25, 0.175, 0.175))),
    colour2 = sample(c(colour_vec2), n(), replace = TRUE))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_point(
    data = grid %>% filter(subset < 12),
    aes(x = x_warped, y = y_warped, size = size / 4),
    colour = outline_colour1, shape = 5) +
  ggfx::as_reference(
    geom_curve(
      data = grid %>% filter(subset >= 8),
      aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size,
          colour = colour1),
      curvature = 0),
    id = "base") +
  geom_point(
    data = grid %>% filter(subset >= 12),
    aes(x = x_warped, y = y_warped, size = size * 0.375),
    colour = bg_colour) +
  ggfx::with_blend(
    geom_point(
      data = grid %>% filter(subset < 12 & subset >= 6),
      aes(x = x_warped, y = y_warped, size = size, colour = colour2),
      shape = 16),
    bg_layer = "base",
    blend_type = "overlay") +
  geom_point(
    data = grid %>% filter(subset > 16),
    aes(x = x_warped, y = y_warped, size = size / 4),
    colour = outline_colour2, shape = 5) +
  scale_size_identity() +
  scale_colour_identity() +
  coord_equal(expand = TRUE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin     = margin(40,40,40,40, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  device = "png", width = 6, height = 6, units = "in", dpi = 600)
