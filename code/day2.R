#
# DAY 2 CHALLENGE: Use Dithering
#
# by Courtney B. Hilton, January 2nd, 2022

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

