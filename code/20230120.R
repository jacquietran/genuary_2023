# Load libraries ---------------------------------------------------------------

library(tibble)
library(dplyr)
library(ggplot2)

# Create data ------------------------------------------------------------------

df_artwork_base <- tibble(
  x = c(0,0,10,10),
  y = c(7.55,9.45,9.45,7.55))

df_artwork_border <- tibble(
  x = c(0,0,10,10),
  y = c(7.65,9.35,9.35,7.65))

df_tunnel_border <- tibble(
  x = c(0,0,10,10),
  y = c(7.75,9.25,9.25,7.75))

df_tunnel_border_hatch <- tibble(
  x_start = seq(0,10, by = 0.05),
  x_end = x_start,
  y_start = 9.15,
  y_end = 9.25)

df_tunnel <- tibble(
  x = c(0,0,10,10),
  y = c(8,9,9,8))

df_arrow_base <- tibble(
  x = c(0.5,2,10,10,2,0.5),
  y = c(8.5,9,9,8,8,8.5))

df_arrow_base_hatch <- tibble(
  x_start = c(0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.4, 1.7, 2.0), # plus 1-1-1-1-1-1-1-2-3-3
  x_end = x_start,
  y_start = 8,
  y_end = 9)

df_arrow_backlit <- tibble(
  x = c(1.5,2,10,10,2,1.5),
  y = c(8.5,8.75,8.75,8.25,8.25,8.5))

df_arrow_backlit_hatch <- tibble(
  x_start = c(9.9,9.8,9.7,9.6,9.5,9.4,9.3,9.1,8.8,8.5), # minus 1-1-1-1-1-1-1-2-3-3
  x_end = x_start,
  y_start = 8.25,
  y_end = 8.75)

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = df_artwork_base,
    aes(x = x, y = y),
    fill = "#3f3f3f") +
  geom_polygon(
    data = df_artwork_border,
    aes(x = x, y = y),
    fill = "#eac744") +
  geom_polygon(
    data = df_tunnel_border,
    aes(x = x, y = y),
    fill = "#d66d4d") +
  geom_segment(
    data = df_tunnel_border_hatch,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    colour = "#3f3f3f") +
  geom_polygon(
    data = df_tunnel,
    aes(x = x, y = y),
    fill = "#324970") +
  geom_polygon(
    data = df_arrow_base,
    aes(x = x, y = y),
    fill = "#6a9969") +
  geom_segment(
    data = df_arrow_base_hatch,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    colour = "#324970") +
  geom_polygon(
    data = df_arrow_backlit,
    aes(x = x, y = y),
    fill = "#e0d9c7") +
  geom_segment(
    data = df_arrow_backlit_hatch,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    colour = "#6a9969") +
  coord_polar() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#e0d9c7", colour = "#e0d9c7"),
    plot.margin = margin(2,2,2,2, unit = "cm"))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230120.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)
