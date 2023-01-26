# Load libraries ---------------------------------------------------------------

library(rayrender)
library(raybonsai)
library(magick)
# Also requires {here}

# Create trees -----------------------------------------------------------------

tree1 = generate_tree(
  seed = 6871, branch_angle_vert = c(-45,0,45), 
  branch_depth = 8 , leaf_color = "#FFE5EC", leaf_depth_start = 5)

tree2 = generate_tree(
  seed = 6872, branch_angle_vert = seq(-30,30,by=5),
  branch_depth = 8 , leaf_color = "#FFB3C6", leaf_depth_start = 5)

tree3 = generate_tree(
  seed = 6873, branch_angle_vert = seq(-30,30,by=5),
  branch_depth = 8 , leaf_color = "#FB6F92", leaf_depth_start = 5)

# Render trees -----------------------------------------------------------------

group_objects(tree1, pivot_point = c(0,-10,0), angle = c(0,0,10)) %>%
  add_object(group_objects(tree2, pivot_point = c(0,-10,0), angle = c(0,0,-10))) %>%
  add_object(group_objects(tree3, pivot_point = c(0,-10,0), angle = c(0,0,-20))) %>%
  render_tree(
    lights = FALSE, environment_light = here::here("img/ingredients/ninomaru_teien_2k.hdr"), 
    fov=8, lookat=c(-2,3,0),lookfrom=c(20,2,30),
    aperture=1, width=1200, height=800,
    ground_color1 = "darkgreen", ground_color2 = "darkgreen")

# Save to file using GUI

# Switch to {magick} workflow --------------------------------------------------

img <- image_read(here::here("img/ingredients/20230122_export.png"))

img_mod <- img %>%
  image_trim() %>%
  image_border("#FFFFFF", "400x400")

image_write(
  img_mod, path = here::here("img/20230122.png"))
  
