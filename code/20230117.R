# Load libraries ---------------------------------------------------------------

library(ggplot2)
# Also requires {dplyr}, {tibble}, {tidyr}, {stringr}, {tidyselect}
# for use in custom function

# Source custom function -------------------------------------------------------

source(here::here("code/functions/triforce.R"))

# Modifiable parameters --------------------------------------------------------

seed <- 715
n_across <- 8

bg_colour <- "#E7EFEF"
palette1 <- c("#034563", "#8E3B46")
palette2 <- c("#D3A736", "#B1713E")
palette3 <- c("#77ACA2", "#6C6060")

# Create data ------------------------------------------------------------------

divvied_up <- triforce(
  n_across = n_across, seed = seed,
  palette_list = list(palette1, palette2, palette3))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = divvied_up,
    aes(x = x, y = y, group = element_id, fill = colour_hex),
    colour = bg_colour, linewidth = 1.5) +
  scale_fill_identity() +
  theme_void() +
  coord_cartesian(expand = FALSE) +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(6,4,6,4, unit = "cm"))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230117.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)
