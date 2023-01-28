# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
# Also requires {tibble}, {scales}

# Modifiable parameters --------------------------------------------------------

seed_num <- 784
bg_colour <- "#D8E3E9"
hex_main <- "#040926"
hex_A <- "#7EB814"
hex_B <- "#BC4749"
hex_C <- "#DEC617"
hex_D <- "#CD8987"
hex_E <- "#66B79B"

# Create data ------------------------------------------------------------------

# Typical Petrarchan sonnet structure:
# ABBA ABBA CDE CDE

df_initial <- tibble::tibble(
  group = rep(1:14, each = 16),
  point_id = rep(1:16, times = 14))

set.seed(seed_num)
df_max_list <- df_initial %>%
  distinct(group) %>%
  mutate(
      max = sample(seq(10, 16, by = 1), n(), replace = TRUE),
      y = c(
        rev(seq(32, 38, by = 2)),
        rev(seq(20, 26, by = 2)),
        rev(seq(10, 14, by = 2)),
        rev(seq(0, 4, by = 2))))

set.seed(seed_num)
df_complete <- left_join(df_initial, df_max_list, by = "group") %>%
  group_by(group) %>%
  mutate(
    keep_flag = case_when(
      point_id <= max ~ "keep",
      point_id > max  ~ "drop")) %>%
  ungroup() %>%
  filter(keep_flag == "keep") %>%
  select(-keep_flag) %>%
  mutate(
    x_count_var = sample(seq(0.02, 0.06, by = 0.01), n(), replace = TRUE),
    x_count_var = case_when(
      point_id == 1 ~ 0,
      TRUE          ~ x_count_var)) %>%
  group_by(group) %>%
  mutate(
    x = cumsum(x_count_var)) %>%
  ungroup() %>%
  mutate(
    shape_num = case_when(
      point_id == max ~ sample(seq(15, 18, by = 1), n(), replace = TRUE),
      TRUE            ~ sample(seq(0, 14, by = 1), n(), replace = TRUE)),
    rhyming_flag = case_when(
      group %in% c(1,4,5,8) ~ "A",
      group %in% c(2,3,6,7) ~ "B",
      group %in% c(9,12)    ~ "C",
      group %in% c(10,13)   ~ "D",
      group %in% c(11,14)   ~ "E"),
    colour_hex = case_when(
      point_id == max &
        rhyming_flag == "A" ~ hex_A,
      point_id == max &
        rhyming_flag == "B" ~ hex_B,
      point_id == max &
        rhyming_flag == "C" ~ hex_C,
      point_id == max &
        rhyming_flag == "D" ~ hex_D,
      point_id == max &
        rhyming_flag == "E" ~ hex_E,
      TRUE                  ~ hex_main),
    point_size = case_when(
      point_id == max ~ 5,
      TRUE            ~ 3)) %>%
  select(-max)

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_point(
    data = df_complete,
    aes(x = x, y = y, colour = colour_hex, shape = shape_num, group = group,
        size = point_size),
    stroke = 1) +
  scale_colour_identity() +
  scale_shape_identity() +
  scale_size_identity() +
  theme_void() +
  theme(
    plot.margin = margin(3,3,3,3, unit = "cm"),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour))

# Save to file -----------------------------------------------------------------

ggsave(
  here::here("img/20230128.png"), last_plot(),
  width = 12, height = 9, units = "in", dpi = 600)
