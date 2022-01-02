#
# DAY 1 CHALLENGE: Draw 10,000 of something
#
# by Courtney B. Hilton, January 1st, 2022

# libraries ---------------------------------------------------------------

library(pacman)
p_load(
  scales,
  here,
  patchwork,
  tidyverse
)

source(here("code", "misc_functions.R"))

# 1.  Data generation functions -------------------------------------------

# generates random points in 2D space
rand_points <- function(n, mean_x, mean_y, sd_x, sd_y) {
    tibble(
      x = rnorm(n, mean = mean_x, sd = sd_x),
      y = rnorm(n, mean = mean_y, sd = sd_y)
    )
}

# runs rand_points() and adds size and color to each point
point_creator <- function(n, mean_x = 0, mean_y = 0, sd_x = 1, sd_y = 1, colors) {
  rand_points(n, mean_x, mean_y, sd_x,sd_y) %>%
    mutate(
      # add point sizes
      size = sample(seq(0.5,10,.5), n(), replace = TRUE),
      # add point colors
      color = sample(colors, n(), replace = TRUE)
    )
}

# aRt creation -----------------------------------------------------------

for (i in 1:10) {
  colors <- rand_color(20)
  data1 <- point_creator(n = 3500, sd_x = 5, sd_y = 5, colors = colors)
  data2 <- point_creator(n = 100, colors = colors)
  data3 <- point_creator(n = 99, colors = colors)
  data4 <- point_creator(n = 800, sd_x = 3, sd_y = 3, colors = colors)
  data5 <- point_creator(n = 2000, sd_x = 1.6, sd_y = 8, colors = colors)
  data6 <- point_creator(n = 1000, sd_x = 5, sd_y = 5, colors = colors)
  data7 <- point_creator(n = 2500, sd_x = 4, sd_y = 4, colors = colors)
  
  output <- ggplot() + 
    # one giant dot in the middle
    geom_point(data = tibble(x = 0, y = 0), aes(x,y), size = 120, color = "black") +
    geom_point(data = data5, mapping = aes(x,y, size = size / 2.5, color = color)) + 
    geom_point(data = data2, mapping = aes(x*2.5,y*2.5, size = size / 3, color = color)) +
    geom_point(data = data3, mapping = aes(x*2.5,y*2.5, size = size / 3, color = color)) +
    geom_point(data = data1, mapping = aes(x,y, size = size / 20, color = color)) + 
    geom_point(data = data7, mapping = aes(x,y, size = size / 10, color = color)) + 
    geom_point(data = data6, mapping = aes(x,y, size = size / 8, color = color)) + 
    scale_size_identity() +
    scale_color_identity() +
    scale_fill_identity() +
    theme_void() + 
    theme(
      plot.background = element_rect(color = "black", fill = "white")
    )
  
  ggsave(filename = paste0("day1_", i, ".png"), path = here("viz"), plot = output,
         device = "png", width = 8, height = 8, dpi = 300)
}
