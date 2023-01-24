# Load libraries ---------------------------------------------------------------

library(tibble)
library(dplyr)
library(ggplot2)

# Create data ------------------------------------------------------------------

seed_num <- 123

df_col1 <- tibble(
  group = rep(1:24, each = 4),
  x = c(
    rep(c(0, 0, 3, 3), times = 12),
    rep(c(6, 6, 9, 9), times = 12)),
  y = rep(
    c(0,1,1,0,
      1,2,2,1,
      2,3,3,2,
      3,4,4,3,
      4,5,5,4,
      5,6,6,5,
      6,7,7,6,
      7,8,8,7,
      8,9,9,8,
      9,10,10,9,
      10,11,11,10,
      11,12,12,11),
    times = 2),
  colour_hex = rep(
    c(rep("#e1d0c5", times = 3*4),
      rep("#69497e", times = 1*4),
      rep("#e1d0c5", times = 1*4),
      rep("#7b7774", times = 1*4)),
    times = 4))

df_col2 <- tibble(
  group = rep(1:12, each = 4),
  x = rep(c(3, 3, 4.5, 4.5), times = 12),
  y = c(0,1,1,0,
        1,2,2,1,
        2,3,3,2,
        3,4,4,3,
        4,5,5,4,
        5,6,6,5,
        6,7,7,6,
        7,8,8,7,
        8,9,9,8,
        9,10,10,9,
        10,11,11,10,
        11,12,12,11),
  colour_hex = rep(
    c(rep("#69497e", times = 1*4),
      rep("#e1d0c5", times = 1*4),
      rep("#7b7774", times = 1*4),
      rep("#e1d0c5", times = 2*4),
      rep("#7b7774", times = 1*4)),
    times = 2))

df_col3 <- tibble(
  group = rep(1:12, each = 4),
  x = rep(c(4.5, 4.5, 6, 6), times = 12),
  y = c(0,1,1,0,
        1,2,2,1,
        2,3,3,2,
        3,4,4,3,
        4,5,5,4,
        5,6,6,5,
        6,7,7,6,
        7,8,8,7,
        8,9,9,8,
        9,10,10,9,
        10,11,11,10,
        11,12,12,11),
  colour_hex = rep(
    c(rep("#69497e", times = 1*4),
      rep("#e1d0c5", times = 1*4),
      rep("#7b7774", times = 1*4),
      rep("#e1d0c5", times = 3*4)),
    times = 2))

set.seed(seed_num)
df_hlines <- tibble(
  group = seq(1, 23),
  x_start = 0,
  x_end = 9,
  y_start = seq(0.5, 11.5, by = 0.5),
  y_end = y_start) %>%
  mutate(
    colour_hex = sample(
      c("#7b7774", "#32396d", "#ba4646", "#c18c6c", "#e1d0c5"),
      n(), replace = TRUE))

set.seed(seed_num)
df_thinlines <- tibble(
  group = seq(1, 59),
  x_start = 0,
  x_end = 9,
  y_start = seq(0.2, 11.8, by = 0.2),
  y_end = y_start) %>%
  mutate(
    colour_hex = sample(
      c("#7b7774", "#32396d", "#ba4646", "#c18c6c", "#e1d0c5"),
      n(), replace = TRUE))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = df_col1,
    aes(x = x, y = y, group = group, fill = colour_hex)) +
  geom_polygon(
    data = df_col2,
    aes(x = x, y = y, group = group, fill = colour_hex)) +
  geom_polygon(
    data = df_col3,
    aes(x = x, y = y, group = group, fill = colour_hex)) +
  geom_segment(
    data = df_thinlines,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end, group = group,
        colour = colour_hex),
    linewidth = 1.25) +
  geom_segment(
    data = df_hlines,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end, group = group,
        colour = colour_hex),
    linewidth = 3) +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_cartesian(expand = FALSE) +
  theme_void()

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230124.png"), last_plot(),
  width = 9, height = 12, units = "in", dpi = 600)
