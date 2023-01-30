create_subdiv_grid <- function(n_across){
  
  if(missing(n_across)){
    n_across <- 10
  }
  
  # Requires {tibble}, {dplyr}
  
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
  
  return(base_grid)
  
}