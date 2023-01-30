# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
# Also requires {tibble}

# Create data ------------------------------------------------------------------

# Markings
set.seed(3001)
markings <- tibble::tibble(
  x_walk_amount = c(
    sample(seq(0.1, 1, by = 0.05), 1),
    scales::rescale(rnorm(24000), to = c(0.0003, 0.0005))),
  y_walk_amount = c(
    sample(seq(0.1, 1, by = 0.05), 1),
    scales::rescale(rnorm(24000), to = c(0.0005, 0.0008))),
  y = c(cumsum(y_walk_amount))) %>%
  mutate(
    x_walk_dir = case_when(
      row_number() == 1 ~ "plus",
      TRUE              ~ sample(c("plus", "minus"), n(), replace = TRUE)),
    x_walk_amount = case_when(
      x_walk_dir == "minus" ~ -x_walk_amount,
      TRUE                  ~ x_walk_amount),
    x = c(cumsum(x_walk_amount))) %>%
  select(x, y)

# Deletions

# Grain


# Build plot -------------------------------------------------------------------

ggplot() +
  geom_point(
    data = markings,
    aes(x = x, y = y),
    shape = 46, size = 0.01) +
  coord_cartesian(xlim = c(0,10), ylim = c(0,10)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#FFFCF8", colour = "#FFFCF8"),
    panel.border = element_rect(colour = "#FFFFFF", fill = NA, linewidth = 20))

# Save to file -----------------------------------------------------------------
