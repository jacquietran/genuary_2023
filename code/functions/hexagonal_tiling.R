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
        vertex %in% c(1,6) ~ y_start + (shape + shape_add_val) * hex_apothem,
        vertex %in% c(2,5) ~ y_start + ((shape + shape_add_val) * hex_apothem) + hex_apothem,
        vertex %in% c(3,4) ~ y_start + ((shape + shape_add_val) * hex_apothem) + (2*hex_apothem)),
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
        vertex %in% c(1,6) ~ y_start + ((shape + shape_add_val) * hex_apothem),
        vertex %in% c(2,5) ~ y_start + ((shape * 2) * hex_apothem),
        vertex %in% c(3,4) ~ y_start + ((shape * 2) * hex_apothem) + hex_apothem),
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
  
  if(missing(n_strips)){
    n_strips <- 1
  }
  
  if(missing(x_start)){
    x_start <- 0
  }
  
  if(missing(y_start)){
    y_start <- 0
  }
  
  if(missing(hex_side_length)){
    hex_side_length <- 3
  }
  
  if(missing(n_hexagons_vertical)){
    n_hexagons_vertical <- 10
  }
  
  n_strips_vec <- seq(from = 1, to = n_strips, by = 1)
  
  df_mapped <- purrr::map_df(n_strips_vec, function(i){
    
    df <- create_hex_tile_vertical(
      x_start = x_start,
      y_start = y_start,
      hex_side_length = hex_side_length,
      n_hexagons = n_hexagons_vertical)
    
    df_tidy <- df |>
      dplyr::mutate(
        strip = i)
    
  })
  
  strip_calc_val <- (3*(n_strips-1)) + 1
  val_for_calc <- seq(from = 1, to = strip_calc_val, by = 3)
  
  calculation_values_vec <- df_mapped |>
    dplyr::distinct(strip) |>
    dplyr::mutate(
      val_for_calc = val_for_calc)
  
  x_adjusted_data <- dplyr::left_join(
    df_mapped, calculation_values_vec, by = "strip") |>
    dplyr::select(vertex, shape, column, strip, val_for_calc) |>
    dplyr::mutate(
      x_left_vertex6_4 = (val_for_calc * hex_side_length) + x_start,
      x_left_vertex1_3 = (x_left_vertex6_4 - hex_side_length),
      x_left_vertex2 = x_left_vertex1_3 - (0.5*hex_side_length),
      x_left_vertex5 = x_left_vertex6_4 + (0.5*hex_side_length),
      x_right_vertex1_3 = x_left_vertex6_4 + (0.5*hex_side_length),
      x_right_vertex6_4 = (x_right_vertex1_3 + hex_side_length),
      x_right_vertex2 = x_right_vertex1_3 - (0.5*hex_side_length),
      x_right_vertex5 = x_right_vertex6_4 + (0.5*hex_side_length),
      x_adjusted = dplyr::case_when(
        column == "left" & vertex %in% c(1,3) ~ x_left_vertex1_3,
        column == "left" & vertex %in% c(4,6) ~ x_left_vertex6_4,
        column == "left" & vertex == 2        ~ x_left_vertex2,
        column == "left" & vertex == 5        ~ x_left_vertex5,
        column == "right" & vertex %in% c(1,3) ~ x_right_vertex1_3,
        column == "right" & vertex %in% c(4,6) ~ x_right_vertex6_4,
        column == "right" & vertex == 2        ~ x_right_vertex2,
        column == "right" & vertex == 5        ~ x_right_vertex5)) |>
    dplyr::select(vertex, shape, column, strip, x_adjusted)
  
  df_tidy <- dplyr::left_join(
    df_mapped, x_adjusted_data, by = c("vertex", "shape", "column", "strip")) |>
    dplyr::mutate(
      strip_column_shape_id = paste(
        "strip", strip, "column", column, "shape", shape, sep = "_")) |>
    dplyr::select(x_adjusted, y, vertex, shape, strip, column, strip_column_shape_id) |>
    dplyr::rename(x = x_adjusted)
  
  return(df_tidy)
  
}