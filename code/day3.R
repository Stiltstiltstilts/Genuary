#
# DAY 3 CHALLENGE: Space
#
# by Courtney B. Hilton, January 3rd, 2022

# libraries ---------------------------------------------------------------

library(pacman)
p_load(
  here,
  gganimate,
  tidyverse,
  ggfx,
  imager
)

source(here("code", "misc_functions.R"))

# functions ---------------------------------------------------------------

# Create stars randomly and uniformly positioned

# generates random points in 2D space
rand_unif_points <- function(n, colors) {
  tibble(
    x = runif(n),
    y = runif(n)
  ) %>% 
    mutate(
      # add point sizes
      size = sample(seq(0.3,5,.1), n, replace = TRUE),
      # add point colors
      color = sample(colors, n(), replace = TRUE)
    )
}

stars <- rand_unif_points(1000, rand_color(20)) %>% 
  mutate(iter = 1)

planets <- rand_unif_points(500, rand_color(20)) %>% 
  mutate(iter = 1)

sun <- rand_unif_points(300, "yellow") %>% 
  mutate(iter = 1)

# create iterations -------------------------------------------------------

iter_func <- function(data, iters, mov) {
  for (i in 2:(iters+1)) {
    data <- data %>% 
      bind_rows(., data %>% 
                  filter(iter == i - 1) %>% 
                  mutate(across(c(x,y), ~ .x + sample(mov, size = n(), replace = TRUE)),
                         iter = i,
                         color = sample(rand_color(200), size = n(), replace = TRUE))
      )
  }
  return(data)
}

stars <- iter_func(stars, 100, seq(-.02,.02,0.001))
planets <- iter_func(planets,100, seq(-.02,.02,0.001))
sun <- iter_func(sun, 100, seq(-.02,.02,0.001))

# plot --------------------------------------------------------------------

ggplot() + 
  geom_point(data = stars, aes(x,y, size = size / 6, color = color)) + 
  geom_point(data = planets, aes(x,y, size = size / 4, fill = color), color = "white", pch = 21, alpha = .8) +
  geom_point(data = sun, aes(x,y, size = size / 3, fill = color), color = "white", pch = 21, alpha = .8) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "black", color = "white")
  ) + 
  transition_time(iter)

ggplot() + 
  geom_point(data = stars, aes(x,y, size = size / 6, color = color)) + 
  geom_point(data = planets, aes(x,y, size = size / 4, color = color)) +
  geom_point(data = sun, aes(x,y, size = size / 3, color = color)) +
  scale_color_identity() +
  scale_size_identity() +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "white", color = "white")
  ) + 
  transition_time(iter)

