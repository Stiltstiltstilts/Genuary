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

# generate random points
rand_points <- function(n, mean_x, mean_y, sd_x, sd_y) {
    tibble(
      x = rnorm(n, mean = mean_x, sd = sd_x),
      y = rnorm(n, mean = mean_y, sd = sd_y)
    )
}

# 2.  Generate data -------------------------------------------------------

data <- rand_points(
  n = 100,
  mean_x = 0,
  mean_y = 0,
  sd_x = 1,
  sd_y = 1
)



