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

colors_used <- list()

for (i in 1:10) {
  colors_used[[i]] <- rand_color(20)
  data <- list()
  data[[1]] <- point_creator(n = 3500, sd_x = 5, sd_y = 5, colors = colors_used[[i]])
  data[[2]] <- point_creator(n = 100, colors = colors_used[[i]])
  data[[3]] <- point_creator(n = 99, colors = colors_used[[i]])
  data[[4]] <- point_creator(n = 800, sd_x = 3, sd_y = 3, colors = colors_used[[i]])
  data[[5]] <- point_creator(n = 2000, sd_x = 1.6, sd_y = 8, colors = colors_used[[i]])
  data[[6]] <- point_creator(n = 1000, sd_x = 6, sd_y = 6, colors = colors_used[[i]])
  data[[7]] <- point_creator(n = 2500, sd_x = 4, sd_y = 4, colors = colors_used[[i]])
  
  plot_limit <- map_dfr(.x = data, ~ {
    .x %>% 
      summarise(max_x = max(abs(x)),
                max_y = max(abs(y)),
                maxx = max(max_x, max_y))
  }) %>% 
    summarise(maxx = max(maxx)) %>% pull(maxx)
  
  output <- ggplot() + 
    # one giant dot in the middle
    geom_point(data = tibble(x = 0, y = 0), aes(x,y), size = 120, color = "black") +
    # other data, adding up to 9,999 dots
    geom_point(data = data[[5]], mapping = aes(x,y, size = size / 2.5, color = color)) + 
    geom_point(data = data[[2]], mapping = aes(x*2.5,y*2.5, size = size / 3, color = color)) +
    geom_point(data = data[[3]], mapping = aes(x*2.5,y*2.5, size = size / 3, color = color)) +
    geom_point(data = data[[1]], mapping = aes(x,y, size = size / 20, color = color)) + 
    geom_point(data = data[[7]], mapping = aes(x,y, size = size / 10, color = color)) + 
    geom_point(data = data[[6]], mapping = aes(x,y, size = size / 8, color = color)) + 
    scale_size_identity() +
    scale_color_identity() +
    scale_fill_identity() +
    coord_cartesian(xlim = c(-plot_limit - 1, plot_limit + 1), ylim = c(-plot_limit - 1, plot_limit + 1)) +
    theme_void() + 
    theme(
      plot.background = element_rect(color = "black", fill = "white")
    )
  
  ggsave(filename = paste0("day1_", i, ".png"), path = here("viz", "day1"), plot = output,
         device = "png", width = 8, height = 8, dpi = 300)
}

# saving the used colors so that it is easier to recreate specific plots later
save(colors_used, file = here("viz", "day1", "colors_used.RData"))
