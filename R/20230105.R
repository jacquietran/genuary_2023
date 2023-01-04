# Load libraries ---------------------------------------------------------------

library(ape)
library(magick)

# Modifiable parameters --------------------------------------------------------

seed_num <- 7039
n_points <- 15
n_solutions <- 4

bg_colour <- "#252422"
edge_colour <- "#FFFCF2"
solutions_colour <- "#EB5E28"
non_solutions_colour <- "#FFFCF2"
  
edge_width_val <- 5
tip_size_val <- 3.5

# Create data ------------------------------------------------------------------

# Data generated per code from:
# https://towardsdatascience.com/hierarchical-clustering-and-dendrograms-in-r-for-data-science-5ab076fabf76

set.seed(seed_num)
initial_data <- data.frame(
  x = rnorm(n_points, sd = 2),
  y = rnorm(n_points, sd = 1))

initial_data_scaled <- scale(initial_data)

distxy <- dist(initial_data_scaled)

cluster <- hclust(distxy)

# Build plots ------------------------------------------------------------------

# Plotting in base aRghhhh...

# Plot 1: Unrooted type
png(
  here::here("img/ingredients/20230105_01.png"),
  width = 10, height = 10, units = "in", res = 600)

par(bg = bg_colour) # Set background colour
colours <- c(
  rep(solutions_colour, times = n_solutions),
  rep(non_solutions_colour, times = n_points - n_solutions))
cluster_cut <- cutree(cluster, n_points) # Cut tree into separate parts
plot(
  as.phylo(cluster), type = "unrooted", no.margin = FALSE,
  edge.color = edge_colour, edge.width = edge_width_val, show.tip.label = FALSE)
tiplabels(pch = 19, cex = tip_size_val, col = colours[cluster_cut])

dev.off()

# Plot 2: Fan type
png(
  here::here("img/ingredients/20230105_02.png"),
  width = 10, height = 10, units = "in", res = 600)

par(bg = bg_colour) # Set background colour
colours <- c(
  rep(solutions_colour, times = n_solutions),
  rep(non_solutions_colour, times = n_points - n_solutions))
cluster_cut <- cutree(cluster, n_points) # Cut tree into separate parts
plot(
  as.phylo(cluster), type = "fan", no.margin = FALSE,
  edge.color = edge_colour, edge.width = edge_width_val, show.tip.label = FALSE)
tiplabels(pch = 19, cex = tip_size_val, col = colours[cluster_cut])

dev.off()

# Plot 3: Radial type
png(
  here::here("img/ingredients/20230105_03.png"),
  width = 10, height = 10, units = "in", res = 600)

par(bg = bg_colour) # Set background colour
colours <- c(
  rep(solutions_colour, times = n_solutions),
  rep(non_solutions_colour, times = n_points - n_solutions))
cluster_cut <- cutree(cluster, n_points) # Cut tree into separate parts
plot(
  as.phylo(cluster), type = "radial", no.margin = FALSE,
  edge.color = edge_colour, edge.width = edge_width_val, show.tip.label = FALSE)
tiplabels(pch = 19, cex = tip_size_val, col = colours[cluster_cut])

dev.off()

# Plot 4: Cladogram
png(
  here::here("img/ingredients/20230105_04.png"),
  width = 10, height = 10, units = "in", res = 600)

par(bg = bg_colour) # Set background colour
colours <- c(
  rep(solutions_colour, times = n_solutions),
  rep(non_solutions_colour, times = n_points - n_solutions))
cluster_cut <- cutree(cluster, n_points) # Cut tree into separate parts
plot(
  as.phylo(cluster), type = "cladogram", no.margin = FALSE,
  edge.color = edge_colour, edge.width = edge_width_val, show.tip.label = FALSE)
tiplabels(pch = 19, cex = tip_size_val, col = colours[cluster_cut])

dev.off()

# Switch to {magick} workflow --------------------------------------------------

# Read images in
plot1 <- image_read(here::here("img/ingredients/20230105_01.png"))
plot2 <- image_read(here::here("img/ingredients/20230105_02.png"))
plot3 <- image_read(here::here("img/ingredients/20230105_03.png"))
plot4 <- image_read(here::here("img/ingredients/20230105_04.png"))

# Append images
appendage1 <- image_append(c(plot1, plot2))
appendage2 <- image_append(c(plot3, plot4))
appendages_all <- image_append(
  c(appendage1, appendage2), stack = TRUE) |>
  image_scale("6000x6000!") |>
  image_border(bg_colour, "900x900") |>
  image_scale("6000x6000!")

# Save final image
image_write(
  appendages_all, path = here::here("img/20230105.png"))
