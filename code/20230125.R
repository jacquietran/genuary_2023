# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)
library(magick)

# Modifiable parameters --------------------------------------------------------

seed <- 5612

# Create data ------------------------------------------------------------------

set.seed(seed)
df <- tibble::tibble(
  x = rnorm(3500),
  y = rnorm(3500))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_voronoi_segment(
    data = df,
    aes(x = x, y = y),
    linewidth = 1.5, colour = "#32853F") +
  coord_cartesian(xlim = c(0.1,0.9), ylim = c(0.1,0.9), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#E5D4CE", colour = "#E5D4CE"))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/ingredients/20230125.png"), last_plot(),
  width = 8, height = 10, units = "in", dpi = 600)

# Shift to {magick} workflow ---------------------------------------------------

img <- image_read(
  here::here("img/ingredients/20230125.png"))

img_mod <- img %>%
  image_morphology(method = "Erode", kernel = "Disk", iterations = 4) %>%
  image_border("#E5D4CE", "1200x1200") %>%
  image_scale("4800x6000!")

image_write(
  img_mod, path = here::here("img/20230125.png"))
