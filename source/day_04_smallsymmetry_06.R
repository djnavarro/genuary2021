library(here)
library(jasmines)
library(dplyr)

n <- 20

dat <- use_seed(11) %>%
  scene_grid(
    xpos = (1:n)*1.2,
    ypos = (1:n)*1.2,
    entity = "circle",
    grain = 400,
    size = 5
  ) %>%
  filter(id %in% sample(n^2, n^2/5)) %>%
  group_by(id) %>%
  filter(ind < sample(max(ind), 1)) %>%
  filter(ind > sample(max(ind), 1)) %>%
  ungroup() %>%
  unfold_breeze(
    iterations = 200,
    scale = .006,
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
    background = "#000930",
    colour = "val",
    palette = function(n) {scico::scico(n, palette = "vik")},
    alpha = c(.5, .03),
    size = .2,
    type = "point"
  ) %>%
  export_image(
    xlim = c(.2, .8),
    ylim = c(.2, .8),
    filename = here(
      "image", "day_04_smallsymmetry_06.png"),
    width = 10,
    height = 10
  )
