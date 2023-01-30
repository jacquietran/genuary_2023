# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
# Also requires {tibble}, {ggforce}, {here}

# Source custom function -------------------------------------------------------

source(here::here("code/functions/create_subdiv_grid.R"))

# Modifiable parameters --------------------------------------------------------

square_fill <- "#F2E2C4"
line_colour <- "#2E70DC"

# Create data ------------------------------------------------------------------

# Squares
squares <- create_subdiv_grid(n_across = 12)

# Build plot -------------------------------------------------------------------

ggplot() +
  ggforce::geom_shape(
    data = squares,
    aes(x = x, y = y, group = group),
    fill = square_fill, expand = unit(-0.5, "mm"), radius = unit(4, "mm")) +
  coord_cartesian(xlim = c(0,9), ylim = c(0,12), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = line_colour, colour = line_colour))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20230104_01.png"), last_plot(),
  width = 9, height = 12, units = "in", dpi = 600)