# Load libraries ---------------------------------------------------------------

library(tibble)
library(sonify)

# Create data ------------------------------------------------------------------

seed_num <- 516783

set.seed(seed_num)
df <- tibble(
  x = sample(seq(-10, 10, by = 1), 20, replace = FALSE),
  y = seq(1, 20, by = 1))

# Generate sound ---------------------------------------------------------------

sonify(
  x = df$x,
  y = df$y,
  waveform = "triangle",
  interpolation = "constant",
  duration = 6)

simulated_birdsong <- warbleR::sim_songs(
  n = 3, harms = 1, seed = seed_num, gaps = 1)
