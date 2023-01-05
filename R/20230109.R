# Load libraries ---------------------------------------------------------------

library(HilbertCurve)
library(circlize)
library(IRanges)
library(magick)

# Modifiable parameters --------------------------------------------------------

initial_seed <- 25713
palette <- c("#005377", "#FFB997", "#F67E7D", "#843B62") # 4 colours

# Derived parameters -----------------------------------------------------------

set.seed(initial_seed)
seed_vec <- sample(1:5000000, 4, replace = FALSE)

# Create data ------------------------------------------------------------------

# Reference:
# https://bioconductor.org/packages/release/bioc/vignettes/HilbertCurve/inst/doc/HilbertCurve.html

# Hilbert curve 1
set.seed(seed_vec[1])
hc1 <- HilbertCurve(1, 100, level = 6, reference = FALSE)

set.seed(seed_vec[1])
x = sort(sample(100, 20))
s = x[1:10*2 - 1]
e = x[1:10*2]
ir1 = IRanges(s, e)
ir1

# Hilbert curve 2
set.seed(seed_vec[2])
hc2 <- HilbertCurve(1, 100, level = 6, reference = FALSE)

set.seed(seed_vec[2])
x = sort(sample(100, 20))
s = x[1:10*2 - 1]
e = x[1:10*2]
ir2 = IRanges(s, e)
ir2

# Hilbert curve 3
set.seed(seed_vec[3])
hc3 <- HilbertCurve(1, 100, level = 6, reference = FALSE)

set.seed(seed_vec[3])
x = sort(sample(100, 40))
s = x[1:10*2 - 1]
e = x[1:10*2]
ir3 = IRanges(s, e)
ir3

# Hilbert curve 4
set.seed(seed_vec[4])
hc4 <- HilbertCurve(1, 100, level = 6, reference = FALSE)

set.seed(seed_vec[4])
x = sort(sample(100, 40))
s = x[1:10*2 - 1]
e = x[1:10*2]
ir4 = IRanges(s, e)
ir4

# Build plot -------------------------------------------------------------------

par(mar = c(0,0,0,0))
hc_segments(hc1, ir1, gp = gpar(lwd = 60, col = palette[1]))
hc_segments(hc2, ir2, gp = gpar(lwd = 60, col = palette[2]))
hc_segments(hc3, ir3, gp = gpar(lwd = 60, col = palette[3]))
hc_segments(hc4, ir4, gp = gpar(lwd = 60, col = palette[4]))
# Save image using RStudio UI

# Switch to {magick} workflow --------------------------------------------------

img <- image_read(here::here("img/ingredients/20230109_base.png"))

img_filtered <- image_negate(img) |>
  image_border(color = "#000000", "1000x1000") |>
  image_scale("6000x6000!")

image_write(
  img_filtered,
  path = here::here("img/20230109.png"),
  format = "png")
