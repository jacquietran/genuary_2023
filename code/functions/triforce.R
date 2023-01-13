triforce <- function(n_across, seed, palette_list){
  
  if(missing(n_across)){
    n_across <- 10
  }
  
  if(missing(seed)){
    seed <- 123
  }
  
  if(missing(palette_list)){
    palette_list <- list(
      c("#034563", "#8E3B46"),
      c("#D3A736", "#B1713E"),
      c("#77ACA2", "#6C6060"))
  }
  
  # Requires {tibble}, {dplyr}, {tidyr}, {stringr}, {tidyselect}
  
  n_groups <- n_across^2
  
  # Create base grid -----------------------------------------------------------

  base_grid <- tibble::tibble(
    row = as.numeric(rep(1:n_across, each = 4*n_across)),
    group = as.character(rep(1:n_groups, each = 4)),
    coord_loc = as.numeric(rep(1:4, times = n_groups)),
    x = rep(
      c(
        rep(0, times = 2),
        rep(seq(from = 1, to = (n_across - 1), by = 1), each = 4),
        rep(n_across, times = 2)),
      times = n_across)) |>
    dplyr::mutate(
      y = dplyr::case_when(
        coord_loc %in% c(1,4) ~ row - 1,
        coord_loc %in% c(2,3) ~ row))
  
  # Label the base grid for subsequent subdivision -----------------------------
  
  set.seed(seed)
  subdiv_labels <- base_grid |>
    dplyr::distinct(group) |>
    dplyr::mutate(
      size = sample(c("small", "medium", "large"), dplyr::n(), replace = TRUE),
      orientation = dplyr::case_when(
        # tl = top left, tr = top right, br = bottom right, bl = bottom left
        size == "large"  ~ sample(c("tl", "tr", "br", "bl"), dplyr::n(), replace = TRUE),
        # t = top only, b = bottom only, tb = top and bottom
        size == "medium" ~ sample(c("t", "b", "tb"), dplyr::n(), replace = TRUE),
        # two columns or four columns of small shapes
        size == "small"  ~ sample(c("two", "four"), dplyr::n(), replace = TRUE)))
  
  subdiv_grid <- dplyr::left_join(
    base_grid, subdiv_labels, by = "group")
  
  # Specify coords: Large elements -----------------------------------------------
  
  subdiv_grid_l <- subdiv_grid |>
    dplyr::filter(size == "large") |>
    dplyr::mutate(
      coord_to_omit = dplyr::case_when(
        orientation == "tl" & coord_loc == 4 ~ "omit",
        orientation == "tr" & coord_loc == 1 ~ "omit",
        orientation == "br" & coord_loc == 2 ~ "omit",
        orientation == "bl" & coord_loc == 3 ~ "omit",
        TRUE                                 ~ "keep")) |>
    dplyr::filter(coord_to_omit == "keep") |>
    dplyr::select(-coord_to_omit)
  
  # Specify coords: Medium elements ----------------------------------------------
  
  subdiv_grid_m <- subdiv_grid |>
    dplyr::filter(size == "medium") |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      midpoint_x = max(x) - 0.5,
      midpoint_y = row - 0.5) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      coord_to_adjust = dplyr::case_when(
        orientation == "t" & coord_loc == 1 ~ "revalue",
        orientation == "t" & coord_loc == 4 ~ "omit",
        orientation == "b" & coord_loc == 2 ~ "revalue",
        orientation == "b" & coord_loc == 3 ~ "omit",
        TRUE                                ~ "keep"),
      x = dplyr::case_when(
        coord_to_adjust == "revalue" ~ midpoint_x,
        TRUE                         ~ x),
      y = dplyr::case_when(
        coord_to_adjust == "revalue" ~ midpoint_y,
        TRUE                         ~ y)) |>
    dplyr::filter(coord_to_adjust != "omit") |>
    dplyr::select(-coord_to_adjust) |>
    dplyr::mutate(
      coord_loc_new = dplyr::case_when(
        orientation == "tb" & coord_loc == 3 ~ 2,
        orientation == "tb" & coord_loc == 2 ~ 3,
        TRUE                                 ~ coord_loc)) |>
    dplyr::select(-coord_loc, -contains("midpoint")) |>
    dplyr::rename(coord_loc = coord_loc_new) |>
    dplyr::arrange(row, group, coord_loc)
  
  # Specify coords: Small elements - 2 columns -----------------------------------
  
  # X values
  subdiv_grid_s_2col_x <- subdiv_grid |>
    dplyr::filter(size == "small" & orientation == "two") |>
    dplyr::select(row, group, size, x, orientation) |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      x_min = min(x),
      x_max = max(x),
      midpoint_x = max(x) - 0.5,
      qtrpoint_x = midpoint_x - 0.25,
      threeqtrpoint_x = midpoint_x + 0.25) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      row_group_id = paste("row", row, "group", group, sep = "_")) |>
    dplyr::distinct(row_group_id, .keep_all = TRUE) |>
    dplyr::select(-x) |>
    dplyr::mutate(
      x1 = x_min,
      x2 = midpoint_x,
      x3 = qtrpoint_x,
      x4 = x1,
      x5 = x2,
      x6 = x3,
      x7 = midpoint_x,
      x8 = x_max,
      x9 = threeqtrpoint_x,
      x10 = x7,
      x11 = x8,
      x12 = x9) |>
    dplyr::select(
      row, group, size, orientation, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
      x11, x12) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("x"),
      names_to = "coord_loc",
      values_to = "x") |>
    dplyr::mutate(
      coord_loc = as.numeric(stringr::str_remove_all(coord_loc, "x")))
  
  # Y values
  subdiv_grid_s_2col_y <- subdiv_grid |>
    dplyr::filter(size == "small" & orientation == "two") |>
    dplyr::select(row, group, size, y, orientation) |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      y_min = min(y),
      y_max = max(y),
      midpoint_y = row - 0.5) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      row_group_id = paste("row", row, "group", group, sep = "_")) |>
    dplyr::distinct(row_group_id, .keep_all = TRUE) |>
    dplyr::select(-y) |>
    dplyr::mutate(
      y1 = y_max,
      y2 = y_max,
      y3 = midpoint_y,
      y4 = midpoint_y,
      y5 = midpoint_y,
      y6 = y_min,
      y7 = y1,
      y8 = y2,
      y9 = y3,
      y10 = y4,
      y11 = y5,
      y12 = y6) |>
    dplyr::select(
      row, group, size, orientation, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10,
      y11, y12) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("y"),
      names_to = "coord_loc",
      values_to = "y") |>
    dplyr::mutate(
      coord_loc = as.numeric(stringr::str_remove_all(coord_loc, "y")))
  
  # Merge X and Y values together
  subdiv_grid_s_2col <- dplyr::left_join(
    subdiv_grid_s_2col_x, subdiv_grid_s_2col_y,
    by = c("row", "group", "size", "orientation", "coord_loc")) |>
    dplyr::mutate(
      subgroup = rep(
        rep(1:4, each = 3),
        times = length(unique(group))),
      group = paste(group, subgroup, sep = "_")) |>
    dplyr::select(-subgroup)
  
  # Specify coords: Small elements - 4 columns -----------------------------------
  
  # X values
  subdiv_grid_s_4col_x <- subdiv_grid |>
    dplyr::filter(size == "small" & orientation == "four") |>
    dplyr::select(row, group, size, x, orientation) |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      x_min = min(x),
      x_max = max(x),
      midpoint_x = max(x) - 0.5,
      qtrpoint_x = midpoint_x - 0.25,
      threeqtrpoint_x = midpoint_x + 0.25,
      oneeighthpoint_x = x_min + 0.125,
      threeeighthspoint_x = midpoint_x - 0.125,
      fiveeightspoint_x = midpoint_x + 0.125,
      seveneighthspoint_x = x_max - 0.125) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      row_group_id = paste("row", row, "group", group, sep = "_")) |>
    dplyr::distinct(row_group_id, .keep_all = TRUE) |>
    dplyr::select(-x) |>
    dplyr::mutate(
      x1 = x_min,
      x2 = qtrpoint_x,
      x3 = oneeighthpoint_x,
      x4 = x1,
      x5 = x2,
      x6 = x3,
      x7 = qtrpoint_x,
      x8 = midpoint_x,
      x9 = threeeighthspoint_x,
      x10 = x7,
      x11 = x8,
      x12 = x9,
      x13 = midpoint_x,
      x14 = threeqtrpoint_x,
      x15 = fiveeightspoint_x,
      x16 = x13,
      x17 = x14,
      x18 = x15,
      x19 = threeqtrpoint_x,
      x20 = x_max,
      x21 = seveneighthspoint_x,
      x22 = x19,
      x23 = x20,
      x24 = x21) |>
    dplyr::select(
      row, group, size, orientation, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
      x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("x"),
      names_to = "coord_loc",
      values_to = "x") |>
    dplyr::mutate(
      coord_loc = as.numeric(stringr::str_remove_all(coord_loc, "x")))
  
  # Y values
  subdiv_grid_s_4col_y <- subdiv_grid |>
    dplyr::filter(size == "small" & orientation == "four") |>
    dplyr::select(row, group, size, y, orientation) |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      y_min = min(y),
      y_max = max(y),
      midpoint_y = row - 0.5) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      row_group_id = paste("row", row, "group", group, sep = "_")) |>
    dplyr::distinct(row_group_id, .keep_all = TRUE) |>
    dplyr::select(-y) |>
    dplyr::mutate(
      y1 = y_max,
      y2 = y_max,
      y3 = midpoint_y,
      y4 = midpoint_y,
      y5 = midpoint_y,
      y6 = y_min,
      y7 = y1,
      y8 = y2,
      y9 = y3,
      y10 = y4,
      y11 = y5,
      y12 = y6,
      y13 = y1,
      y14 = y2,
      y15 = y3,
      y16 = y4,
      y17 = y5,
      y18 = y6,
      y19 = y1,
      y20 = y2,
      y21 = y3,
      y22 = y4,
      y23 = y5,
      y24 = y6) |>
    dplyr::select(
      row, group, size, orientation, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10,
      y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("y"),
      names_to = "coord_loc",
      values_to = "y") |>
    dplyr::mutate(
      coord_loc = as.numeric(stringr::str_remove_all(coord_loc, "y")))
  
  # Merge X and Y values together
  subdiv_grid_s_4col <- dplyr::left_join(
    subdiv_grid_s_4col_x, subdiv_grid_s_4col_y,
    by = c("row", "group", "size", "orientation", "coord_loc")) |>
    dplyr::mutate(
      subgroup = rep(
        rep(1:8, each = 3),
        times = length(unique(group))),
      group = paste(group, subgroup, sep = "_")) |>
    dplyr::select(-subgroup)
  
  # Merge the small elements data into one data frame ----------------------------
  
  subdiv_grid_s <- dplyr::bind_rows(subdiv_grid_s_2col, subdiv_grid_s_4col)
  
  # Bring the subdivisions back together -----------------------------------------
  
  set.seed(seed)
  divvied_up <- dplyr::bind_rows(subdiv_grid_l, subdiv_grid_m, subdiv_grid_s) |>
    dplyr::mutate(
      element_id = paste("group", group, "size", size, sep = "_")) |>
    dplyr::group_by(element_id) |>
    dplyr::mutate(
      colour_hex = dplyr::case_when(
        size == "large"  ~ sample(unlist(palette_list[1]), dplyr::n(), replace = TRUE),
        size == "medium" ~ sample(unlist(palette_list[2]), dplyr::n(), replace = TRUE),
        size == "small"  ~ sample(unlist(palette_list[3]), dplyr::n(), replace = TRUE))) |>
    dplyr::ungroup()
  
  return(divvied_up)
  
}