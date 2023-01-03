# Load libraries ---------------------------------------------------------------

library(av)
library(ggplot2)
library(magick)

# Load audio data --------------------------------------------------------------

data <- tuneR::readWave(here::here("misc/batflowers_snippet.wav"))

# Create spectogram ------------------------------------------------------------

plot_base <- seewave::ggspectro(data, ovlp=50) +
  geom_tile(
    aes(fill = amplitude),
    colour = NA) +
  scale_fill_gradient(
    low = "#094074",
    high = "#FE9000") +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(-250,-40,0,-40, unit = "pt"))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/ingredients/20230110_base.png"), plot_base,
  width = 10, height = 8, units = "in", dpi = 600)

# Switch to {magick} workflow --------------------------------------------------

# Read in image
img <- image_read(here::here("img/ingredients/20230110_base.png"))

# Apply blending
img_blend1 <- image_flatten(
  c(img, img), "LinearDodge")
img_blend2 <- image_flatten(
  c(img, img_blend1), "HardLight")
img_blend3 <- image_flatten(
  c(img_blend1, img_blend2), "Exclusion")
img_blend4 <- image_flatten(
  c(img_blend3, img_blend2), "Add")

# Export
image_write(
  img_blend4,
  path = here::here("img/20230110.png"),
  format = "png")
