# Load libraries ---------------------------------------------------------------

library(ggplot2)
library(magick)
# Also requires {dplyr}, {here}

# Source custom function -------------------------------------------------------

source(here::here("code/functions/release.R"))

# Modifiable parameters --------------------------------------------------------

seed_num <- 46573
bg_colour <- "#F4F1DE"
line_colour <- "#101118"
n_marks <- 50

# Derived parameters -----------------------------------------------------------

set.seed(seed_num)
seed_vec <- sample(1:3000000, n_marks, replace = FALSE)

# Create data ------------------------------------------------------------------

# Brush strokes
tight_strokes <- purrr::map_df(seed_vec, function(i){
  
  # Requires {dplyr}
  # and custom function release_1x1()
  
  set.seed(i)
  df <- release_1x1(
    seed = i,
    lines = sample(1:3, 1),
    control_points = sample(5:10, 1))
  
  set.seed(i)
  df_tidy <- df |>
    dplyr::mutate(
      seed = i,
      seed_group_id = paste("seed", seed, "group", group, sep = "_"))
  
  return(df_tidy)
  
})

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  geom_point(
    data = tight_strokes,
    aes(x = x, y = y, group = seed_group_id, size = index),
    shape = 16, colour = line_colour) +
  facet_wrap(~seed_group_id, nrow = 6) +
  scale_size_continuous(range = c(0.2, 0.6)) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing = unit(-5, "lines"),
    plot.margin = margin(4,-2,14,-2, unit = "cm"),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export to file
ggsave(
  here::here("img/ingredients/20230131_base.png"), p,
  width = 8, height = 12, units = "in", dpi = 600, device = ragg::agg_png)

# Shift to {magick} workflow ---------------------------------------------------

img <- image_read(here::here("img/ingredients/20230131_base.png"))

img_mod <- img |>
  image_noise(noisetype = "Laplacian") |>
  image_noise(noisetype = "Laplacian") |>
  image_noise(noisetype = "Laplacian")

image_write(
  img_mod, path = here::here("img/offcuts/20230131_01.png"))
