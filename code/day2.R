#
# DAY 2 CHALLENGE: Use Dithering
#
# by Courtney B. Hilton, January 2nd, 2022

# libraries ---------------------------------------------------------------

library(pacman)
p_load(
  here,
  tidyverse,
  imager
)

source(here("code", "misc_functions.R"))

# stuff -------------------------------------------------------------------

image <- load.image(here("viz", "day1", "day1_1.png"))
image_gray <- grayscale(rm.alpha(image))
image_threshold <- image_gray > 0.5

final_data <- image_threshold %>% as.data.frame
colors_used <- list()

for (i in 1:10) {
  colors_used[[i]] <- rand_color(1)
  
  ggplot(final_data, aes(x,y)) + 
    geom_raster(aes(fill=cc %>% as.factor)) + 
    scale_fill_manual(values = c("white", colors_used[[i]])) +
    theme_void() + 
    theme(legend.position = "none",
          plot.background = element_rect(color = "black", fill = "white"))
  
  ggsave(filename = paste0("day2_", i, ".png"), path = here("viz", "day2"),
         device = "png", width = 8, height = 8, dpi = 300)
}

# saving the used colors so that it is easier to recreate specific plots later
save(colors_used, file = here("viz", "day2", "colors_used.RData"))
