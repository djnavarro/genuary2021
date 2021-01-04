library(here)
library(jasmines)
library(dplyr)

n <- 12

dat <- use_seed(19) %>%
  scene_grid(
    xpos = (1:n)*1.2,
    ypos = (1:n)*1.2,
    entity = "droplet",
    grain = 800,
    size = 8,
    shape = 10,
    angle = sample(80:100, n^2, replace = TRUE)
  ) %>%
  filter(id %in% sample(n^2, n^2/5)) %>%
  group_by(id) %>%
  filter(ind < sample(max(ind), 1)) %>%
  filter(ind > sample(max(ind), 1)) %>%
  ungroup() %>%
  unfold_breeze(
    iterations = 300,
    scale = .008,
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
    background = "grey10",
    colour = "val",
    palette = function(n) {scico::scico(n, palette = "bilbao")},
    alpha = c(.5, .06),
    size = .1,
    type = "point"
  ) %>%
  export_image(
    xlim = c(.2, .8),
    ylim = c(.2, .8),
    filename = here(
      "image", "day_04_smallsymmetry_08.jpg"),
    width = 10,
    height = 10
  )
