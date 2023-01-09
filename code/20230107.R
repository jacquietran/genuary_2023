# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(truchet)

# Set parameters ---------------------------------------------------------------

iteration_id <- "20230107"
seed_num <- 24870
n_tile_types <- 5
inset_lower <- 5
inset_higher <- 10

# Set custom colour vec - ~10 colours
colour_vec <- ghibli::ghibli_palette("PonyoMedium")

# Create data ------------------------------------------------------------------

xlim <- c(0, 20)
ylim <- c(0, 20)

tile_types <- c(
  'dl', 'dr', '-', '|', '+.', '+', 'x.', 'tn', 'fnw', 'fne', 'fsw', 'fse',
  'ane', 'asw')

# Create a data frame with the spots for tiles
set.seed(seed_num)
container <- expand.grid(
  x = seq(xlim[1], xlim[2], 1),
  y = seq(ylim[1], ylim[2], 1)) %>%
  # Sample from the list of tiles provided
  # Types can be: {'dl','dr','-','|','+.','+','x.','tn','fnw','fne','fsw','fse','ane','asw'}
  mutate(
    tiles = sample(
      sample(tile_types, n_tile_types, replace = FALSE), n(), replace = TRUE),
    # scale_p = 1/2)
    scale_p = case_when(
      (x < inset_lower | x > inset_higher) |
        (y < inset_lower | y > inset_higher) ~ 1/8,
      (x > inset_lower & x < inset_higher) &
        (y > inset_lower & y < inset_higher) ~ 1/4,
      TRUE                                   ~ 1/2))

# Set colour sampling probability
set.seed(seed_num)
colour_weights <- tibble::tibble(
  prob_initial = runif(length(colour_vec)),
  prob_sum = sum(prob_initial),
  prob = prob_initial / prob_sum) %>%
  pull(prob)

set.seed(seed_num)
dissolved <- st_truchet_ms(df = container) %>%
  st_truchet_dissolve() %>%
  mutate(
    colour_hex = sample(colour_vec, n(), replace = TRUE, prob = colour_weights))

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  geom_sf(
    data = dissolved,
    aes(colour = colour_hex, fill = colour_hex),
    stroke = 1, size = 0.2) +
  scale_fill_identity() +
  scale_colour_identity() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
    plot.margin = margin(-60, -60, -60, -60, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  last_plot(), width = 6, height = 6, units = "in", dpi = 600)

beepr::beep(10)

