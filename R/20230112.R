# Load libraries ---------------------------------------------------------------

library(ggplot2)

# Source custom functions ------------------------------------------------------

source(here::here("R/functions/triangular_tiling.R"))
# source(here::here("R/functions/hexagonal_tiling.R"))

# Create data ------------------------------------------------------------------

# Triangular tiling
# df <- create_tri_tile_set(n_strips = 10, n_triangles_vertical = 10)

# Hexagonal tiling
df <- create_hex_tile_vertical()

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = df,
    aes(x = x, y = y, group = column_shape_id),
    colour = "#000000", fill = NA)
  
  #theme_void() +
  #theme(
  #  plot.background = element_rect(fill = "#e2c7aa"))

# Save to file -----------------------------------------------------------------
