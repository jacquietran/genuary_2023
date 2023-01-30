# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
# Also requires {tibble}, {tidyr}, {here}

# Modifiable data --------------------------------------------------------------

seed <- 3001
bg_colour <- "#FFFFFF"
ceramic <- "#FFF9F2"
line_colour <- "#10247B"

# Create data ------------------------------------------------------------------

# Markings
set.seed(seed)
markings <- tibble::tibble(
  x_start = seq(1, 9, by = 0.25),
  x_end = x_start) %>%
  mutate(
    y_start = 1,
    # y_start = sample(seq(1, 2, by = 0.1), n(), replace = TRUE),
    y_end = sample(seq(7.5, 9, by = 0.1), n(), replace = TRUE))

# Gaps
set.seed(seed)
gaps <- markings %>%
  select(x_start, y_start, y_end) %>%
  rename(x = x_start) %>%
  mutate(
    y1 = sample(seq(1, 9, by = 0.1), n(), replace = TRUE),
    y2 = sample(seq(1, 9, by = 0.1), n(), replace = TRUE),
    y3 = sample(seq(1, 9, by = 0.1), n(), replace = TRUE),
    y4 = sample(seq(1, 9, by = 0.1), n(), replace = TRUE),
    y5 = sample(seq(1, 9, by = 0.1), n(), replace = TRUE)) %>%
  select(-y_start, -y_end) %>%
  tidyr::pivot_longer(
    cols = starts_with("y"),
    names_to = "key",
    values_to = "y") %>%
  select(-key) %>%
  mutate(
    point_size = sample(seq(0.1, 2, by = 0.1), n(), replace = TRUE))

# Grain


# Build plot -------------------------------------------------------------------

ggplot() +
  geom_segment(
    data = markings,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    colour = line_colour) +
  geom_point(
    data = gaps,
    aes(x = x, y = y, size = point_size),
    colour = ceramic) +
  scale_size_identity() +
  coord_cartesian(xlim = c(-1,11), ylim = c(-1,11)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = ceramic, colour = ceramic),
    panel.border = element_rect(colour = bg_colour, fill = NA, linewidth = 50))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/offcuts/20230130_01.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)
