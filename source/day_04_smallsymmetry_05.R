library(here)
library(jasmines)
library(dplyr)

n <- 8

dat <- use_seed(8) %>%
  scene_grid(
    xpos = (1:n)*1.2,
    ypos = (1:n)*1.2,
    entity = "circle",
    grain = 400,
    size = 4
  ) %>%
  filter(id %in% sample(n^2, n^2/2)) %>%
  group_by(id) %>%
  filter(ind < sample(max(ind), 1)) %>%
  filter(ind > sample(max(ind), 1)) %>%
  ungroup() %>%
  unfold_breeze(
    iterations = 200,
    scale = .0004,
    octaves = 5
  ) %>%
  unfold_inside() %>%
  mutate(
    x = x + (runif(n^2))[id] * (inside == 0),
    y = y + (runif(n^2))[id] * (inside == 0),
    val = ind +(sample(n^2))[id]
  )

dat %>%
  style_ribbon(
    background = "ghostwhite", "#222326",
    colour = "val",
    palette = function(n) {gameofthrones::got(n, option = "Targaryen")},
    alpha = c(.5, .02),
    size = .2,
    type = "point"
  ) %>%
  export_image(
    xlim = c(.025, .975),
    ylim = c(.025, .975),
    filename = here(
      "image", "day_04_smallsymmetry_05.png"),
    width = 10,
    height = 10
  )
