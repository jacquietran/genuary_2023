# Load libraries ---------------------------------------------------------------

library(tibble)
library(ggplot2)
library(gganimate)

# Create data ------------------------------------------------------------------

# Background spiros
spiro_bg1 <- ggplot() +
  ggforce::geom_spiro(
    aes(R = 10, r = 3, d = 16),
    revolutions = 3, n = 5000)
spiro_bg1_data <- layer_data(spiro_bg1)

spiro_bg2 <- ggplot() +
  ggforce::geom_spiro(
    aes(R = 20, r = 5, d = 7),
    revolutions = 1, n = 5000)
spiro_bg2_data <- layer_data(spiro_bg2)

# Foreground spiro
spiro_fg <- ggplot() +
  ggforce::geom_spiro(
    aes(R = 30, r = 12, d = 3),
    revolutions = 2, n = 5000)
spiro_fg_data <- layer_data(spiro_fg)

# Build plot -------------------------------------------------------------------

p <- ggplot() +
  geom_point(
    data = spiro_bg1_data, 
    aes(x = x, y = y),
    colour = "#44BBA4", size = 2, shape = 16) +
  geom_point(
    data = spiro_bg2_data, 
    aes(x = x, y = y),
    colour = "#D5573B", size = 2, shape = 16) +
  geom_point(
    data = spiro_fg_data, 
    aes(x = x, y = y),
    colour = "#E7BB41", size = 10, shape = 16) +
  transition_time(index) +
  shadow_wake(wake_length = 0.8, alpha = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#131313", colour = "#131313"))

p_anim <- animate(
  p, fps = 30, height = 10, width = 10, units = "cm", res = 300)

# Save to file -----------------------------------------------------------------

anim_save(
  filename = "20230101.gif", p_anim, path = here::here("img/"))
