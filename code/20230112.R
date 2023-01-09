# Load libraries ---------------------------------------------------------------

library(ggplot2)

# Source custom functions ------------------------------------------------------

source(here::here("code/functions/hexagonal_tiling.R"))

# Modifiable parameters --------------------------------------------------------

# For hexagonal tiling
hex_side <- 2
n_hex_strips <- 16
n_hex_vertical <- 16

line_width_val <- 1

# Derived parameters -----------------------------------------------------------

hex_apothem <- hex_side / (2*tan(30 * pi / 180))

# Create data ------------------------------------------------------------------

# Hexagonal tiling
df_hex <- create_hex_tile_set(
  hex_side_length = hex_side,
  n_strips = n_hex_strips, n_hexagons_vertical = n_hex_vertical)

# Overlay lines - vertical
df_lines <- tibble::tibble(
  x = seq(
    from = hex_side / 2,
    to = (2*n_hex_strips) * (1.5*hex_side),
    by = 1.5 * hex_side))

# Overlay lines - diagonal, negative gradient
df_diags_neg_x <- tibble::tibble(
  x_start = hex_side / 2,
  intervals = seq(
    from = 1.5 * hex_side,
    to = (1.5*n_hex_vertical) * (3*hex_side),
    by = 3 * hex_side),
  x_end = x_start + intervals) |>
  dplyr::select(-intervals)

df_diags_neg_y <- tibble::tibble(
  intervals = seq(
    from = 1,
    to = 2*(1.5*n_hex_vertical),
    by = 2),
  y_start = hex_apothem * intervals,
  y_end = 0) |>
  dplyr::select(-intervals)

df_diags_neg <- dplyr::bind_cols(
  df_diags_neg_x, df_diags_neg_y)

# Overlay lines - diagonal, positive gradient
df_diags_pos_x <- tibble::tibble(
  x_start = hex_side / 2,
  intervals = seq(
    from = 1.5 * hex_side,
    to = (1.5*n_hex_vertical) * (3*hex_side),
    by = 3 * hex_side),
  x_end = x_start + intervals)

df_diags_pos_y <- tibble::tibble(
  intervals = seq(
    from = 1,
    to = 2*(1.5*n_hex_vertical),
    by = 2),
  multiplication_var = seq(
    from = 1,
    to = (1.5 * n_hex_strips * 2) - 1,
    by = 2),
  y_start = hex_apothem * intervals,
  y_end = y_start + (multiplication_var *hex_apothem)) |>
  dplyr::select(-intervals)

df_diags_pos <- dplyr::bind_cols(
  df_diags_pos_x, df_diags_pos_y)


# Build plot: Main output ------------------------------------------------------

# Build plot
ggplot() +
  # Vertical lines
  geom_vline(
    data = df_lines,
    aes(xintercept = x),
    colour = "#8783D1", linewidth = line_width_val) +
  # Diagonals - negative
  geom_segment(
    data = df_diags_neg,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    colour = "#5AAA95", linewidth = line_width_val) +
  # Diagonals - positive
  geom_segment(
    data = df_diags_pos,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    colour = "#BB9F06", linewidth = line_width_val) +
  # Hexagonal tiling
  geom_polygon(
    data = df_hex,
    aes(x = x, y = y, group = strip_column_shape_id),
    colour = "#095256", fill = NA, linewidth = line_width_val) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#EAD7C2", colour = "#EAD7C2"))

# Save to file
ggsave(
  here::here("img/20230112.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)

# Build plot: Offcut -----------------------------------------------------------

# Build plot
ggplot() +
  # Vertical lines
  geom_vline(
    data = df_lines,
    aes(xintercept = x),
    colour = "#8783D1", linewidth = line_width_val * 2) +
  # Diagonals - negative
  geom_segment(
    data = df_diags_neg,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    colour = "#5AAA95", linewidth = line_width_val * 2) +
  # Diagonals - positive
  geom_segment(
    data = df_diags_pos,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    colour = "#BB9F06", linewidth = line_width_val * 2) +
  # Hexagonal tiling
  geom_polygon(
    data = df_hex,
    aes(x = x, y = y, group = strip_column_shape_id),
    colour = "#095256", fill = NA, linewidth = line_width_val * 2) +
  coord_cartesian(xlim = c(10,40), ylim = c(35,65), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#EAD7C2", colour = "#EAD7C2"))

ggsave(
  here::here("img/offcuts/20230112_01.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 600)