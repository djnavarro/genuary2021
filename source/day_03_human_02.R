library(tidyverse)
library(asciify)
library(here)

print_grid <- function(x) {
  x %>%
    as.data.frame() %>%
    transpose() %>%
    map(~ cat(unlist(.x), "\n"))
}

here("other", "human1.jpg") %>%
  ascii_map(alphabet = 0:9, rescale = .03) %>%
  ascii_grid() %>%
  print_grid()

