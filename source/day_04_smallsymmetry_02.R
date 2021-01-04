library(here)
library(jasmines)
library(dplyr)

n <- 20

dat <- use_seed(2) %>%
  scene_grid(
    xpos = (1:n)*1.2,
    ypos = (1:n)*1.2,
    entity = "circle",
    grain = 200,
    size = 1
  ) %>%
  filter(id %in% sample(n^2, n^2/2)) %>%
  unfold_breeze(
    iterations = 150,
    scale = .0006,
    octaves = 3
  ) %>%
  unfold_inside() %>%
  mutate(
    val = 1 +
      (sample(n^2))[id] * (inside == 0)
  )

dat %>%
  style_ribbon(
    background = "#424651",
    colour = "val",
    palette = colorRampPalette(c(
      "#424651", # charcoal
      "#FFAEBC", # hot pink
      "#A0E7E5", # tiffany blue
      "#B4F8C8", # mint
      "#FBE7C6"  # yellow
    )),
    alpha = c(.7, .002),
    size = .1,
    type = "segment"
  ) %>%
  export_image(
    xlim = c(.025, .975),
    ylim = c(.025, .975),
    filename = here(
      "image", "day_04_smallsymmetry_02.png"),
    width = 10,
    height = 10
  )
