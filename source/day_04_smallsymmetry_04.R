library(here)
library(jasmines)
library(dplyr)

n <- 16

dat <- use_seed(6) %>%
  scene_grid(
    xpos = (1:n)*1.2,
    ypos = (1:n)*1.2,
    entity = "circle",
    grain = 400,
    size = 2
  ) %>%
  filter(id %in% sample(n^2, n^2/3)) %>%
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
    val = 1 +
      (sample(n^2))[id] * (inside == 0)
  )

dat %>%
  style_ribbon(
    background = "#101418",
    colour = "val",
    palette = colorRampPalette(c(
      "#101418", # dark charcoal
      "#FFAEBC", # hot pink
      "#A0E7E5", # tiffany blue
      "#B4F8C8", # mint
      "#FBE7C6"  # yellow
    )),
    alpha = c(.99, .02),
    size = .2,
    type = "segment"
  ) %>%
  export_image(
    xlim = c(.025, .975),
    ylim = c(.025, .975),
    filename = here(
      "image", "day_04_smallsymmetry_04.png"),
    width = 10,
    height = 10
  )
