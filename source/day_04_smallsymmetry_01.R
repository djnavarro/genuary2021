library(here)
library(jasmines)
library(dplyr)

set.seed(1)
n <- 20

use_seed(1) %>%
  scene_grid(
    xpos = (1:n)*1.2,
    ypos = (1:n)*1.2,
    entity = "circle",
    grain = 200,
    size = 1
  ) %>%
  filter(id %in% sample(n^2, n^2/2)) %>%
  unfold_breeze(
    iterations = 300,
    scale = .0002,
    octaves = 3
  ) %>%
  unfold_inside() %>%
  mutate(
    val = 1 + (sample(n^2))[id] * (inside == 0)
  ) %>%
  style_ribbon(
    background = "#2E3644",
    colour = "val",
    palette = colorRampPalette(c(
      "#5D6C89",
      "#36D6E7",
      "#FEB06A",
      "#FBF6F3"
    )),
    alpha = c(.5, .0025),
    size = .1,
    type = "segment"
  ) %>%
  export_image(
    xlim = c(.025, .975),
    ylim = c(.025, .975),
    filename = here(
      "image", "day_04_smallsymmetry_01.png"),
    width = 10,
    height = 10
  )
