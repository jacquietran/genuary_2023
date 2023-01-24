# Load libraries ---------------------------------------------------------------

library(paintr)
library(ggplot2)
library(magick)

# Import image -----------------------------------------------------------------

img <- here::here("img/ingredients/jean-beller-6cLBliGpaoM-unsplash.jpg")

# Generate paint-by-numbers data
painted <- paint_by_numbers(
  img,
  brightness = 170, saturation = 200, simplify = 0.1,
  target_palette = c("#FFFFFF", "#000000"))

# Build plot -------------------------------------------------------------------

p <- painted$painted_picture +
  theme_void() +
  theme(
    legend.key = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/ingredients/20230119_base.png"), p,
  width = 12, height = 10, units = "in", dpi = 600)

# Shift to {magick} workflow ---------------------------------------------------

base <- image_read(here::here("img/ingredients/20230119_base.png"))

base_cropped <- base |>
  image_crop("7200x5500") |>
  image_trim(fuzz = 50) |>
  image_morphology(method = "Erode", kernel = "Disk", iterations = 4) |>
  image_noise(noisetype = "Gaussian") |>
  image_noise(noisetype = "Gaussian") |>
  image_noise(noisetype = "Laplacian") |>
  image_border("#FFFFFF", "800x800") |>
  image_scale("6000x5000!")

image_write(
  image = base_cropped, path = here::here("img/20230119.png"))

