# Load libraries ---------------------------------------------------------------

library(tibble)
library(dplyr)
library(ggplot2)
library(magick)
# Also requires {tidyr}, {ggfx}

# Modifiable parameters --------------------------------------------------------

seed_num <- 6827
bg_colour <- "#101219"
palette <- c("#C33149", "#A8C256", "#5DB7DE", "#FFADC6", "#FB8B24")
length_out <- 700

# Derived parameters -----------------------------------------------------------

set.seed(seed_num)
seed_vec <- sample(1:700000, 2, replace = FALSE)

# Create data ------------------------------------------------------------------

# Data set 1
set.seed(seed_vec[1])
df1 <- tibble(
  x = seq(0, 80, length.out = length_out),
  y = sin(x)) %>%
  mutate(
    rando = sample(1:10, n(), replace = TRUE),
    colour_hex_initial = sample(palette, n(), replace = TRUE),
    colour_start = case_when(
      row_number() == 1 ~ "start",
      rando == 6        ~ "start",
      TRUE              ~ NA_character_),
    colour_hex = case_when(
      colour_start == "start" ~ colour_hex_initial,
      TRUE                    ~ NA_character_)) %>%
  tidyr::fill(colour_hex, .direction = "down") %>%
  select(-rando, -colour_hex_initial, -colour_start)

# Data set 2
set.seed(seed_vec[2])
df2 <- tibble(
  x = seq(0, 80, length.out = length_out),
  y = sin(x)) %>%
  mutate(
    rando = sample(1:10, n(), replace = TRUE),
    colour_hex_initial = sample(palette, n(), replace = TRUE),
    colour_start = case_when(
      row_number() == 1 ~ "start",
      rando == 6        ~ "start",
      TRUE              ~ NA_character_),
    colour_hex = case_when(
      colour_start == "start" ~ colour_hex_initial,
      TRUE                    ~ NA_character_)) %>%
  tidyr::fill(colour_hex, .direction = "down") %>%
  select(-rando, -colour_hex_initial, -colour_start)

# Build plot -------------------------------------------------------------------

ggplot() +
  ggfx::as_reference(
    geom_point(
      data = df1,
      aes(x = x, y = y, colour = colour_hex),
      shape = "\U25AC", size = 15, alpha = 0.8),
    id = "base") +
  ggfx::with_blend(
    geom_point(
      data = df2,
      aes(x = x, y = y, colour = colour_hex),
      shape = "\U25AC", size = 15, alpha = 0.8),
    bg_layer = "base",
    blend_type = "linear_burn",
    id = "blend1") +
  ggfx::with_blend(
    geom_point(
      data = df2,
      aes(x = x, y = y, colour = colour_hex),
      shape = "\U25AC", size = 15, alpha = 0.8),
    bg_layer = "blend1",
    blend_type = "soft_light") +
  scale_colour_identity() +
  theme_void() +
  theme(
    plot.margin = margin(4,4,4,4, unit = "cm"),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/ingredients/20230115.png"), last_plot(),
  width = 12, height = 9, units = "in", dpi = 600)

# Switch to {magick} workflow --------------------------------------------------

img <- image_read(here::here("img/ingredients/20230115.png"))

img_implode <- img %>%
  image_implode(factor = -0.7) %>%
  image_morphology(
    method = "Dilate", kernel = "Disk", iterations = 6) %>%
  image_noise(noisetype = "laplacian") %>%
  image_noise(noisetype = "laplacian") %>%
  image_noise(noisetype = "laplacian") %>%
  image_scale("4000x3000!")

image_write(
  img_implode, path = here::here("img/20230115.png"))
