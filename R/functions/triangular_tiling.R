# Contains two functions:
# create_tri_tile_vertical()
# create_tri_tile_set()

# Create a vertical 'strip' of triangular tiling -------------------------------

create_tri_tile_vertical <- function(
    x_start, y_start, tri_side_length, n_triangles){
  
  # Requires {tibble}, {dplyr}
  
  if(missing(x_start)){
    x_start <- 0
  }
  
  if(missing(y_start)){
    y_start <- 0
  }
  
  if(missing(tri_side_length)){
    tri_side_length <- 3
  }
  
  if(missing(n_triangles)){
    n_triangles <- 10
  }
  
  y_val1_interval <- tri_side_length / 2
  
  y_val1_vec <- tibble::tibble(
    shape = seq(1:n_triangles),
    y_val1 = seq(
      from = y_start, to = (n_triangles * y_val1_interval - y_val1_interval),
      by = y_val1_interval))
  
  # Create left "column" of tiles
  df_left_base <- tibble::tibble(
    vertex = rep(1:3, times = n_triangles),
    shape = rep(1:n_triangles, each = 3)) |>
    dplyr::mutate(
      x = dplyr::case_when(
        vertex %in% c(1,3) & shape %% 2 > 0  ~ x_start + tri_side_length,
        vertex == 2 & shape %% 2 > 0         ~ tri_side_length - tri_side_length,
        vertex %in% c(1,3) & shape %% 2 == 0 ~ tri_side_length - tri_side_length,
        vertex == 2 & shape %% 2 == 0        ~ x_start + tri_side_length),
      y_abs = rep(c(y_start, 0.5*tri_side_length, tri_side_length), times = n_triangles))
  
  df_left_merged <- dplyr::left_join(
    df_left_base, y_val1_vec, by = "shape") |>
    dplyr::mutate(
      y = dplyr::case_when(
        vertex == 1 ~ y_val1,
        TRUE        ~ y_val1 + y_abs)) |>
    dplyr::select(x,y,vertex,shape) |>
    dplyr::mutate(column = "left")
  
  # Create right "column" of tiles
  df_right_base <- tibble::tibble(
    vertex = rep(1:3, times = n_triangles),
    shape = rep(1:n_triangles, each = 3)) |>
    dplyr::mutate(
      x = dplyr::case_when(
        vertex %in% c(1,3) & shape %% 2 > 0  ~ tri_side_length,
        vertex == 2 & shape %% 2 > 0         ~ 2 * (x_start + tri_side_length),
        vertex %in% c(1,3) & shape %% 2 == 0 ~ 2 * (x_start + tri_side_length),
        vertex == 2 & shape %% 2 == 0        ~ tri_side_length),
      y_abs = rep(c(y_start, 0.5*tri_side_length, tri_side_length), times = n_triangles))
  
  df_right_merged <- dplyr::left_join(
    df_right_base, y_val1_vec, by = "shape") |>
    dplyr::mutate(
      y = dplyr::case_when(
        vertex == 1 ~ y_val1,
        TRUE        ~ y_val1 + y_abs)) |>
    dplyr::select(x,y,vertex,shape) |>
    dplyr::mutate(column = "right")
  
  # Merge tile columns together
  df_tidy <- dplyr::bind_rows(df_left_merged, df_right_merged) |>
    dplyr::mutate(
      column_shape_id = paste("column", column, "shape", shape, sep = "_"))
  
  return(df_tidy)
  
}

# Create multiple, side-by-side vertical strips of triangle tiling -------------
# using the above-defined function: create_tri_tile_vertical()     -------------

create_tri_tile_set <- function(
    n_strips, x_start, y_start, tri_side_length, n_triangles_vertical){
  
  # Requires {tibble}, {dplyr}, {purrr}
  # and custom function create_tri_tile_vertical()
  
  if(missing(n_strips)){
    n_strips <- 1
  }
  
  if(missing(x_start)){
    x_start <- 0
  }
  
  if(missing(y_start)){
    y_start <- 0
  }
  
  if(missing(tri_side_length)){
    tri_side_length <- 3
  }
  
  if(missing(n_triangles_vertical)){
    n_triangles_vertical <- 10
  }
  
  n_strips_vec <- seq(from = 1, to = n_strips, by = 1)
  
  df_mapped <- purrr::map_df(n_strips_vec, function(i){
    
    df <- create_tri_tile_vertical(
      x_start = x_start,
      y_start = y_start,
      tri_side_length = tri_side_length,
      n_triangles = n_triangles_vertical)
    
    df_tidy <- df |>
      dplyr::mutate(
        strip = i)
    
  })
  
  x_adjusted_data <- df_mapped |>
    dplyr::select(vertex, shape, column, strip) |>
    dplyr::mutate(
      x_midline = dplyr::case_when(
        strip == 1 ~ strip*tri_side_length,
        TRUE       ~ (strip*tri_side_length) + ((strip - 1)*tri_side_length)),
      x_adjusted = dplyr::case_when(
        column == "left" &
          vertex %in% c(1,3) &
          shape %% 2 > 0       ~ x_midline,
        column == "left" &
          vertex == 2 &
          shape %% 2 > 0       ~ x_midline - tri_side_length,
        column == "left" &
          vertex %in% c(1,3) &
          shape %% 2 == 0      ~ x_midline - tri_side_length,
        column == "left" &
          vertex == 2 &
          shape %% 2 == 0      ~ x_midline,
        column == "right" &
          vertex %in% c(1,3) &
          shape %% 2 > 0       ~ x_midline,
        column == "right" &
          vertex == 2 &
          shape %% 2 > 0       ~ x_midline + tri_side_length,
        column == "right" &
          vertex %in% c(1,3) &
          shape %% 2 == 0      ~ x_midline + tri_side_length,
        column == "right" &
          vertex == 2 &
          shape %% 2 == 0      ~ x_midline))
  
  df_tidy <- dplyr::left_join(
    df_mapped, x_adjusted_data, by = c("vertex", "shape", "column", "strip")) |>
    dplyr::mutate(
      strip_column_shape_id = paste(
        "strip", strip, "column", column, "shape", shape, sep = "_")) |>
    dplyr::select(x_adjusted, y, vertex, shape, strip, column, strip_column_shape_id) |>
    dplyr::rename(x = x_adjusted)
  
  return(df_tidy)
  
}