prefix <- "day_02_rule30"
version <- "02"

library(here)
library(tidyverse)
library(paletteer)
set.seed(2)

pixels_wide <- 1000
pixels_high <- 1000
iterations <- 50
fade <- .025
rate <- .0025

x_wrap <- function(x) x %% pixels_wide
y_wrap <- function(y) y %% pixels_high

connect <- function(graph, ...) {
  graph %>%
    mutate(map = row_number()) %>%
    mutate(...) %>%
    mutate(x0 = x_wrap(x0), y0 = y_wrap(y0)) %>%
    arrange(x0, y0) %>%
    pull(map)
}

rule_30 <- function(a, b, c) {
  case_when(                         # rule 30:
    a == 1 & b == 1 & c == 1  ~  0,  #   111 --> 0
    a == 1 & b == 1 & c == 0  ~  0,  #   110 --> 0
    a == 1 & b == 0 & c == 1  ~  0,  #   101 --> 0
    a == 1 & b == 0 & c == 0  ~  1,  #   100 --> 1
    a == 0 & b == 1 & c == 1  ~  1,  #   011 --> 1
    a == 0 & b == 1 & c == 0  ~  1,  #   010 --> 1
    a == 0 & b == 0 & c == 1  ~  1,  #   001 --> 1
    a == 0 & b == 0 & c == 0  ~  0   #   000 --> 0
  )
}

initial_colour <- function(x, y) {
  as.numeric(runif(length(x)) < rate)
}

graph0 <- expand_grid(
  x0 = 0:(pixels_wide - 1),
  y0 = 0:(pixels_high - 1)
)

graph <- graph0 %>%
  transmute(
    x = x0,
    y = y0,
    colour = initial_colour(x0, y0)
  ) %>%
  mutate(
    edge_1 = graph0 %>% connect(x0 = x0 - 1, y0 = y0 - 1),
    edge_2 = graph0 %>% connect(x0 = x0,     y0 = y0 - 1),
    edge_3 = graph0 %>% connect(x0 = x0, y0 = y0 - 2),
    feed_1 = NA_integer_,
    feed_2 = NA_integer_,
    feed_3 = NA_integer_,
    shades = colour
  )

iterate <- function(graph) {
  graph %>%
    mutate(
      feed_1 = colour[edge_1],
      feed_2 = colour[edge_2],
      feed_3 = colour[edge_3],
      colour = rule_30(feed_1, feed_2, feed_3),
      shades = (1 - fade) * shades + fade * colour
    )
}

for(i in 1:iterations) {
  cat(".")
  if(i %% 10 == 0) cat(" ", i, "\n")
  graph <- iterate(graph)
}

image <- ggplot(graph, aes(x = x, y = y, fill = shades)) +
  geom_raster(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_paletteer_c(palette = "viridis::magma") +
  theme_void()

ggsave(
  filename = here("image", paste0(prefix, "_", version, ".png")),
  plot = image,
  width = pixels_wide / 300,
  height = pixels_high / 300,
  dpi = 300
)
