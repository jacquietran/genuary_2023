release_1x1 <- function(seed, lines, control_points, interpolation){
  
  # Requires {tibble}, {ggplot2}, {ggforce}
  
  if(missing(interpolation)){
    
    interpolation <- 20000
    
  }
  
  set.seed(seed)
  data <- tibble::tibble(
    group = rep(1:lines, each = control_points),
    x = runif(lines*control_points),
    y = runif(lines*control_points))
  
  d <- ggplot2::ggplot() +
    ggforce::stat_bspline(
      data = data,
      ggplot2::aes(x = x, y = y, group = group),
      n = interpolation)
  
  t <- ggplot2::layer_data(d)
  
  return(t)
  
}