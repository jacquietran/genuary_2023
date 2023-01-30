# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
# Also requires {tibble}, {ggforce}, {here}

# Modifiable parameters --------------------------------------------------------

square_fill <- "#F2E2C4"
line_colour <- "#2E70DC"

# Create data ------------------------------------------------------------------

# Squares
squares <- tibble::tibble(
  group = rep(1:4, each = 4),
  x = rep(c(
    0,0,1,1,
    1,1,2,2), times = 2),
  y = c(
    rep(c(0,1,1,0), times = 2),
    rep(c(1,2,2,1), times = 2)))

# Build plot -------------------------------------------------------------------

ggplot() +
  ggforce::geom_shape(
    data = squares,
    aes(x = x, y = y, group = group),
    fill = square_fill, expand = unit(-1, "mm"), radius = unit(4, "mm")) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = line_colour, colour = line_colour))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20230104_01.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)