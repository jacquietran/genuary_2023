dust_up <- function(
  seed_num, palette, n_strips, grain, y_limit,
  direction = c("horizontal", "vertical", "random")){

  # Requires {tibble}, {dplyr}, {tidyr}, and
  # custom gradients functions by Sharla Gelfand
  
  # Pick a highlight colour based on the supplied colour vec
  set.seed(seed_num)
  hi_start_hex <- sample(palette, 1)
  hi_start_rgb <- col2rgb(hi_start_hex)
  hi_lighten_r <- hi_start_rgb[1] + round(0.98 * hi_start_rgb[1], 0)
  hi_lighten_g <- hi_start_rgb[2] + round(0.98 * hi_start_rgb[2], 0)
  hi_lighten_b <- hi_start_rgb[3] + round(0.98 * hi_start_rgb[3], 0)
  
  hi_df <- tibble::tibble(
    red = hi_lighten_r,
    green = hi_lighten_g,
    blue = hi_lighten_b) |>
    dplyr::mutate(
      red = dplyr::case_when(
        red > 255 ~ 255,
        TRUE      ~ red),
      green = dplyr::case_when(
        green > 255 ~ 255,
        TRUE        ~ green),
      blue = dplyr::case_when(
        blue > 255 ~ 255,
        TRUE       ~ blue))
  
  highlight_hex <- rgb(hi_df$red, hi_df$green, hi_df$blue, maxColorValue = 255)
  
  # Randomly set strip width based on supplied values for n_strips and seed_num
  set.seed(seed_num)
  strip_width <- tibble::tibble(
    strip_width_initial = runif(n_strips)) |>
    dplyr::mutate(
      sum_of_widths = sum(strip_width_initial),
      strip_width = strip_width_initial / sum_of_widths) |>
    dplyr::pull(strip_width)
  
  # Set y values based on user-defined y limit
  ymin_lower <- 0.1 * y_limit
  ymin_upper <- 0.25 * y_limit

  # Single row
  set.seed(seed_num)
  layout <- tibble::tibble(
    size = strip_width,
    colour_1 = sample(palette, n_strips, replace = TRUE),
    colour_2 = sample(rev(palette), n_strips, replace = TRUE),
    ymin = 0,
    ymax = y_limit) |>
    dplyr::mutate(
      horizontal = dplyr::case_when(
        direction == "vertical"   ~ FALSE,
        direction == "horizontal" ~ TRUE,
        direction == "random"     ~ sample(
          c(TRUE, FALSE), dplyr::n(), replace = TRUE)),
      xmax = cumsum(size),
      xmin = xmax - size)
  
  # Create gradient data
  gradient_grid <- layout |>
    dplyr::mutate(
      points = purrr::pmap(
        list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal),
        generate_points_from_grid, granularity = grain)) |>
    dplyr::select(points) |>
    tidyr::unnest(cols = c(points)) |>
    dplyr::mutate(
      temp_var = sample(seq(1, 300, by = 1), dplyr::n(), replace = TRUE)) |>
    dplyr::filter(temp_var == 5) |>
    dplyr::mutate(
      temp_var = sample(seq(1, 60, by = 1), dplyr::n(), replace = TRUE),
      color = dplyr::case_when(
        temp_var == 10 ~ highlight_hex,
        TRUE           ~ color)) |>
    dplyr::select(-temp_var)
  
  return(gradient_grid)

}