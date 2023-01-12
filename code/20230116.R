# Load libraries ---------------------------------------------------------------

library(tibble)
library(dplyr)
library(ggplot2)
# Also requires {ggfx}

# Modifiable parameters --------------------------------------------------------

n_beams <- 4
bg_colour <- "#022A3C"
glow_colour <- "#D3A736"
line_colour <- "#DEBD68"
highlight_colour <- "#EDDAAB"

# Create data ------------------------------------------------------------------

df_beams <- tibble(
  x_start = c(
    rep(3, times = n_beams),
    rep(1, times = n_beams),
    rep(3, times = n_beams),
    rep(1, times = n_beams),
    rep(3, times = n_beams)),
  x_end = c(
    rep(1, times = n_beams),
    rep(3, times = n_beams),
    rep(1, times = n_beams),
    rep(3, times = n_beams),
    rep(1, times = n_beams)),
  y_start = c(
    10,9.8,9.6,9.4,
    8,7.8,7.6,7.4,
    6,5.8,5.6,5.4,
    4,3.8,3.6,3.4,
    2,1.8,1.6,1.4),
  y_end = c(
    8,7.8,7.6,7.4,
    6,5.8,5.6,5.4,
    4,3.8,3.6,3.4,
    2,1.8,1.6,1.4,
    0,-0.2,-0.4,-0.6))

# Build plot -------------------------------------------------------------------

ggplot() +
  ggfx::with_blur(
    geom_segment(
      data = df_beams,
      aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
      colour = glow_colour, linewidth = 3.5, lineend = "round"),
    sigma = 30) +
  ggfx::with_blur(
    geom_segment(
      data = df_beams,
      aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
      colour = line_colour, linewidth = 2.5, lineend = "round"),
    sigma = 20) +
  ggfx::with_blur(
    geom_segment(
      data = df_beams,
      aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
      colour = highlight_colour, linewidth = 1.5, lineend = "round"),
    sigma = 10) +
  coord_cartesian(
    xlim = c(-0.5,4.5), ylim = c(0,9.4), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230116.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)
