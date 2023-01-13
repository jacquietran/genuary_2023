library(ggfx)
library(ggplot2)

cos_wave <- function(width, height) {
  x <- matrix(0, ncol = width, nrow = height)
  x <- cos(col(x)/100)
  as.raster((x + 1) / 2)}

p <- ggplot() +
  as_reference(
    cos_wave,
    id = "wave") +
  with_variable_blur(
    geom_point(aes(disp, mpg), mtcars, size = 4),
    x_sigma = ch_red("wave"),
    y_sigma = ch_alpha("wave"),
    angle = ch_red("wave"),
    x_scale = 15,
    y_scale = 15,
    angle_range = c(-45, 45)) +
  geom_point(aes(disp, mpg), mtcars, size = 3, alpha = 0.7) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))
  
ggsave(
  here::here("img/ingredients/20230119.png"), p,
  width = 3, height = 3, units = "in", dpi = 600)

beepr::beep(5)

ggplot() +
  geom_point(aes(disp, mpg), mtcars, size = 15, shape = "\u259b")

# point types:
# https://unicode-table.com/en/blocks/block-elements/
# https://unicode-table.com/en/blocks/geometric-shapes/