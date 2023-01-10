# Load libraries ---------------------------------------------------------------

library(ggplot2)

# Source custom function -------------------------------------------------------

source(here::here("code/functions/gradients.R"))
source(here::here("code/functions/sweep.R"))
source(here::here("code/functions/dust_up.R"))
source(here::here("code/functions/release.R"))

# Modifiable parameters --------------------------------------------------------

iteration_id <- "20230114"
seed_num <- 68795
bg_colour <- "#FAE9DB"
line_colour <- "#3C2416"
n_marks <- 20

gradient_colours_bg <- c("#F28482", "#F4A171", "#F5AF69", "#F6BD60", "#F6BD60")
dust_colours <- c("#f4f3ee", "#bcb8b1", "#8a817c", "#463f3a")

# Derived parameters -----------------------------------------------------------

set.seed(seed_num)
seed_vec <- sample(1:3000000, n_marks, replace = FALSE)

# Create data ------------------------------------------------------------------

# Gradients
gradient_bg <- straight_sweep(
  seed_num = seed_num, palette = gradient_colours_bg, n_strips = 1,
  grain = 1000, x_limit = 1, y_limit = 1, direction = "vertical")

# Dust layer
dust <- dust_up(
  seed_num = seed_num, palette = dust_colours, n_strips = 1, grain = 800,
  y_limit = 1, direction = "random")

# Build plot: Paper texture ----------------------------------------------------

# Build plot
paper <- ggplot() +
  geom_point(
    data = gradient_bg,
    aes(x = x, y = y, color = color),
    size = 0.001, shape = 16, alpha = 0.25) +
  geom_point(
    data = dust,
    aes(x = x, y = y, color = color),
    size = 0.001, shape = 46, alpha = 0.8) +
  scale_colour_identity() +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Export to file
ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")),
  paper, width = 9, height = 12, units = "in", dpi = 600, device = ragg::agg_png)

# Create data ------------------------------------------------------------------

brush_strokes <- purrr::map_df(seed_vec, function(i){
  
  # Requires {dplyr}
  # and custom function release_1x1()
  
  set.seed(i)
  df <- release_1x1(
    seed = i,
    lines = sample(1:3, 1),
    control_points = sample(5:10, 1))
  
  set.seed(i)
  df_mod <- df |>
    dplyr::mutate(
      seed = i,
      seed_group_id = paste("seed", seed, "group", group, sep = "_"))
  
  df_subset <- df_mod |>
    dplyr::distinct(seed_group_id) |>
    dplyr::mutate(
      plot_or_not = sample(
        c("plot", "not"), dplyr::n(), replace = TRUE, prob = c(0.8, 0.2)))
  
  df_tidy <- dplyr::left_join(
    df_mod, df_subset, by = "seed_group_id")
  
  return(df_tidy)
  
})

# Build plot -------------------------------------------------------------------

# Brush strokes
strokes <- ggplot(
  data = brush_strokes,
  aes(x = x, y = y, group = seed_group_id)) +
  geom_point(
    data = brush_strokes |> dplyr::filter(plot_or_not == "plot"),
    aes(x = x, y = y, group = seed_group_id, size = index),
    shape = 16, colour = line_colour) +
  facet_wrap(~seed_group_id, ncol = 6) +
  scale_size_continuous(range = c(0.4, 1.2)) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    plot.margin = margin(2,6,2,6, unit = "cm"))

# Export to file
ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_strokes.png")),
  strokes, width = 9, height = 12, units = "in", dpi = 600, device = ragg::agg_png)

# Build plot -------------------------------------------------------------------

# Read in paper texture image
paper_bg <- png::readPNG(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")))
paper_raster <- grid::rasterGrob(
  paper_bg, width = unit(1,"npc"), height = unit(1,"npc"))

# Read in brush strokes image
strokes_bg <- png::readPNG(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_strokes.png")))
strokes_raster <- grid::rasterGrob(
  strokes_bg, width = unit(1,"npc"), height = unit(1,"npc"))

# Build combined plot
combined <- ggplot() +
  ggfx::as_reference(
    annotation_custom(paper_raster, -Inf, Inf, -Inf, Inf),
    id = "base") +
  ggfx::with_blend(
    annotation_custom(strokes_raster, -Inf, Inf, -Inf, Inf),
    bg_layer = "base",
    blend_type = "over") +
  theme(
    plot.margin = margin(-0.5,-1,-0.5,-1, unit = "cm"))

# Export to file
ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  combined, width = 9, height = 12, units = "in", dpi = 600, device = ragg::agg_png)
