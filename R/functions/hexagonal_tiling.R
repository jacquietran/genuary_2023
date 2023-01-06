# Contains two functions:
# create_hex_tile_vertical()
# create_hex_tile_set()

# Create a vertical 'strip' of hexagonal tiling --------------------------------

create_hex_tile_vertical <- function(
    x_start, y_start, hex_side_length, n_hexagons){
  
  # Requires {tibble}, {dplyr}
  
  if(missing(x_start)){
    x_start <- 0
  }
  
  if(missing(y_start)){
    y_start <- 0
  }
  
  if(missing(hex_side_length)){
    hex_side_length <- 3
  }
  
  if(missing(n_hexagons)){
    n_hexagons <- 10
  }
  
  hex_apothem <- hex_side_length / (2*tan(30 * pi / 180))
  
  # Create left "column" of tiles
  df_left <- tibble::tibble(
    column = "left",
    shape = rep(1:n_hexagons, each = 6),
    vertex = rep(1:6, times = n_hexagons),
    x = rep(
      c(x_start,
        x_start - (0.5*hex_side_length),
        x_start,
        hex_side_length,
        hex_side_length + (0.5*hex_side_length),
        hex_side_length),
      times = n_hexagons))
  
  y_vals_left <- df_left |>
    dplyr::select(column, shape, vertex) |>
    dplyr::mutate(
      shape_add_val = dplyr::case_when(
        shape == 1 ~ 0,
        TRUE       ~ shape - 2),
      y_shape1 = dplyr::case_when(
        shape == 1 & vertex %in% c(1,6) ~ y_start,
        shape == 1 & vertex %in% c(2,5) ~ y_start + hex_apothem,
        shape == 1 & vertex %in% c(3,4) ~ y_start + (2 * hex_apothem)),
      y_all_shapes = dplyr::case_when(
        vertex %in% c(1,6) ~ (shape + shape_add_val) * hex_apothem,
        vertex %in% c(2,5) ~ ((shape + shape_add_val) * hex_apothem) + hex_apothem,
        vertex %in% c(3,4) ~ ((shape + shape_add_val) * hex_apothem) + (2*hex_apothem)),
      y = dplyr::case_when(
        shape == 1 ~ y_shape1,
        TRUE       ~ y_all_shapes)) |>
    dplyr::select(column, shape, vertex, y)
  
  df_left_merged <- dplyr::left_join(
    df_left, y_vals_left, by = c("column", "shape", "vertex"))
  
  # Create right "column" of tiles
  df_right <- tibble::tibble(
    column = "right",
    shape = rep(1:n_hexagons, each = 6),
    vertex = rep(1:6, times = n_hexagons),
    x = rep(
      c((x_start + (1.5*hex_side_length)),
        (x_start + hex_side_length),
        (x_start + (1.5*hex_side_length)),
        (x_start + (2.5*hex_side_length)),
        (x_start + (3*hex_side_length)),
        (x_start + (2.5*hex_side_length))),
      times = n_hexagons))
  
  y_vals_right <- df_right |>
    dplyr::select(column, shape, vertex) |>
    dplyr::mutate(
      shape_add_val = dplyr::case_when(
        shape == 1 ~ 0,
        TRUE       ~ shape - 1),
      y_shape1 = dplyr::case_when(
        shape == 1 & vertex %in% c(1,6) ~ y_start + hex_apothem,
        shape == 1 & vertex %in% c(2,5) ~ y_start + (2 * hex_apothem),
        shape == 1 & vertex %in% c(3,4) ~ y_start + (3 * hex_apothem)),
      y_all_shapes = dplyr::case_when(
        vertex %in% c(1,6) ~ ((shape + shape_add_val) * hex_apothem),
        vertex %in% c(2,5) ~ ((shape + shape_add_val) * hex_apothem) + hex_apothem,
        vertex %in% c(3,4) ~ ((shape + shape_add_val) * hex_apothem) + (2*hex_apothem)),
      y = dplyr::case_when(
        shape == 1 ~ y_shape1,
        TRUE       ~ y_all_shapes)) |>
    dplyr::select(column, shape, vertex, y)
  
  df_right_merged <- dplyr::left_join(
    df_right, y_vals_right, by = c("column", "shape", "vertex"))
  
  # Merge tile columns together
  df_tidy <- dplyr::bind_rows(df_left_merged, df_right_merged) |>
    dplyr::mutate(
      column_shape_id = paste("column", column, "shape", shape, sep = "_"))
  
  return(df_tidy)
  
}

# Create multiple, side-by-side vertical strips of hexagonal tiling ------------
# using the above-defined function: create_hex_tile_vertical()     -------------

create_hex_tile_set <- function(
    n_strips, x_start, y_start, hex_side_length, n_hexagons_vertical){
  
  # Requires {tibble}, {dplyr}, {purrr}
  # and custom function create_hex_tile_vertical()
  
}