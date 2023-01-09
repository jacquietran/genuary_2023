# Load libraries ---------------------------------------------------------------

library(tibble)
library(dplyr)
library(ggplot2)
library(ggforce)

# Modifiable parameters --------------------------------------------------------

seed_num <- 80713
bg_colour <- "#F5E9E2"
line_colour <- "#423E28"
input_palette <- c("#BB4430", "#613F75", "#A8C256", "#08605F", "#7EBDC2")
n_groups <- 18 # Choose a multiple of 9

# Create data ------------------------------------------------------------------

set.seed(seed_num)
df <- tibble(
  x = scales::rescale(rnorm(90)),
  y = scales::rescale(rnorm(90))) %>%
  mutate(
    group = as.factor(sample(1:n_groups, n(), replace = TRUE)),
    merged_groups = as.factor(sample(1:9, n(), replace = TRUE)))

# Generate colour palette
palette <- (grDevices::colorRampPalette(input_palette))(n_groups)

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_step(
    data = df,
    aes(x = x, y = y, group = merged_groups),
    colour = line_colour) +
  geom_mark_hull(
    data = df,
    aes(x = x, y = y, fill = group),
    radius = unit(3, "mm"), colour = NA, alpha = 1) +
  geom_line(
    data = df,
    aes(x = x, y = y, group = merged_groups),
    colour = bg_colour, orientation = "y") +
  facet_wrap(~merged_groups, ncol = 3) +
  scale_fill_manual(values = palette) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1), expand = TRUE) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    plot.margin = margin(120,120,120,120, unit = "pt"),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230108.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)
