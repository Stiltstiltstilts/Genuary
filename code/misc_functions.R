
# General helper functions

# returns random colors
# by default it returns a single color
# specifying {n_cols} enables returning a vector of random colors
rand_color <- function(n_cols = 1) {
  values <- c(letters[1:6] %>% str_to_upper, # letters in hex colors
              0:9)                             # numbers in hex colors
  replicate(n_cols, {
    c("#", sample(values, 6, replace = TRUE)) %>% str_c(collapse="")
  })
}
