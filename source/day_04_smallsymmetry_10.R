library(here)
library(jasmines)
library(dplyr)

seed <- 100

crescent_moon <- function(seed) {

n <- 20
bg <- paste0("#", paste(sample(10:50, 3),collapse=""))
shader <- colorRampPalette(sample(colours(distinct = TRUE), 4))

dat <- use_seed(seed) %>%
  scene_grid(
    xpos = (1:n)*1.2,
    ypos = (1:n)*1.2,
    entity = "circle",
    grain = 400,
    size = 5
  ) %>%
  filter(id %in% sample(n^2, n^2/6)) %>%
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
    x = x + (runif(n^2)/2)[id] * (inside == 0),
    y = y + (runif(n^2)/2)[id] * (inside == 0),
    val = ind +(sample(n^2))[id]
  )

dat %>%
  style_ribbon(
    background = bg,
    colour = "val",
    palette = shader,
    alpha = c(.8, .03),
    size = .2,
    type = "point"
  ) %>%
  export_image(
    xlim = c(.2, .8),
    ylim = c(.2, .8),
    filename = here(
      "image", paste0("day_04_smallsymmetry_10_", seed, ".jpg")),
    width = 10,
    height = 10
  )

}
