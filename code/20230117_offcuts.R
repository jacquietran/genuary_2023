# Load libraries ---------------------------------------------------------------

library(ggplot2)
# Also requires {dplyr}, {tibble}, {tidyr}, {stringr}, {tidyselect}
# for use in custom function

# Source custom function -------------------------------------------------------

source(here::here("code/functions/triforce.R"))

# Offcut 1 ---------------------------------------------------------------------

# Modifiable parameters
seed <- 987
n_across <- 20

bg_colour <- "#000000"
palette1 <- c("#BFB5AF", "#ECE2D0")
palette2 <- c("#DC493A", "#582C4D")
palette3 <- c("#4392F1", "#012A36")

# Create data
offcut <- triforce(
  n_across = n_across, seed = seed,
  palette_list = list(palette1, palette2, palette3),
  size_weightings = c(0.1,0.3,0.6))

# Build plot
ggplot() +
  geom_polygon(
    data = offcut,
    aes(x = x, y = y, group = element_id, fill = colour_hex),
    colour = bg_colour, linewidth = 1.5) +
  scale_fill_identity() +
  theme_void() +
  coord_cartesian(expand = FALSE) +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(6,4,6,4, unit = "cm"))

# Save to file
ggsave(
  here::here("img/offcuts/20230117_01.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)

# Offcut 2 ---------------------------------------------------------------------

# Modifiable parameters
seed <- 445
n_across <- 20

bg_colour <- "#131313"
palette1 <- c("#861388", "#C32B77", "#FF4365")
palette2 <- c("#00D9C0", "#79A988", "#086788")
palette3 <- c("#FF6F54", "#FF9B42", "#E9EB87")

# Create data
offcut <- triforce(
  n_across = n_across, seed = seed,
  palette_list = list(palette1, palette2, palette3),
  size_weightings = c(0.2,0.4,0.4))

# Build plot
ggplot() +
  geom_polygon(
    data = offcut,
    aes(x = x, y = y, group = element_id, fill = colour_hex),
    colour = bg_colour, linewidth = 1.5) +
  scale_fill_identity() +
  theme_void() +
  coord_cartesian(expand = FALSE) +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(6,6,6,6, unit = "cm"))

# Save to file
ggsave(
  here::here("img/offcuts/20230117_02.png"), last_plot(),
  width = 20, height = 20, units = "in", dpi = 600)
  
# Offcut 3 ---------------------------------------------------------------------

# Modifiable parameters
seed <- 139
n_across <- 30

bg_colour <- "#F9DFC8"
palette1 <- c("#F2542D", "#DC7343", "#BF626E")
palette2 <- c("#7261A3", "#8C6FAE", "#A67DB8")
palette3 <- c("#1B3022", "#395756", "#4F5D75")

# Create data
offcut <- triforce(
  n_across = n_across, seed = seed,
  palette_list = list(palette1, palette2, palette3),
  size_weightings = c(0.2,0.4,0.4))

# Build plot
ggplot() +
  geom_polygon(
    data = offcut,
    aes(x = x, y = y, group = element_id, fill = colour_hex),
    colour = bg_colour, linewidth = 1.5) +
  scale_fill_identity() +
  theme_void() +
  coord_cartesian(expand = FALSE) +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(6,6,6,6, unit = "cm"))

# Save to file
ggsave(
  here::here("img/offcuts/20230117_03.png"), last_plot(),
  width = 20, height = 20, units = "in", dpi = 600)
  