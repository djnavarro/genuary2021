library(here)
library(tidyverse)
library(flametree)
library(ggforce)
library(jasmines)

seed <- 2

genuary_tree <- function(seed) {

  set.seed(seed)
  version <- "02"
  fname <- paste0("day_10_tree_", version, "_", seed, ".jpg")

  shades <- sample(colours(distinct = TRUE), 10)
  #shades <- map_chr(shades, ~blend(.x, "black"))

  cat("making tree...\n")

  tree <- flametree_grow(
    seed = seed,
    time = 12,
    scale = c(0.6, 0.9, 0.9)
  ) %>%
    mutate(
      coord_x = coord_x * 3,
      coord_y = coord_y * 3
    )

  leaf <- tree %>%
    filter(id_leaf == TRUE) %>%
    sample_frac(1)

  cat("making background...\n")

  set.seed(seed)
  background <- scene_rows(n = 200, grain = 200, vertical = TRUE) %>%
    mutate(x = x * 18 - 10, y = y * 18) %>%
    mutate(ind = 1:n()) %>%
    mutate(seed = use_seed(seed)) %>%
    unfold_warp(iterations = 80, scale = .01) %>%
    prepare_data(colour = "time") %>%
    sample_frac(.1)

  cat("making leaves...\n")

  set.seed(seed)
  grass <-  leaf %>%
    mutate(
      x = coord_x + rnorm(n())/2,
      y = coord_y + rnorm(n())/2
    ) %>%
    mutate(ind = 1:n()) %>%
    mutate(seed = use_seed(seed)) %>%
    unfold_warp(iterations = 80, scale = .01) %>%
    prepare_data(colour = "time") %>%
    sample_frac(.4)

  cat("making image...\n")

  pic <- tree %>%
    ggplot() +
    geom_segment(
      data = background,
      mapping = aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
      ),
      show.legend = FALSE,
      size = .1,
      alpha = 1,
      colour = "gray90"
    ) +
    geom_segment(
      data = grass,
      mapping = aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        colour = time
      ),
      show.legend = FALSE,
      size = .3,
      alpha = .8
    ) +
    geom_bezier(
      mapping = aes(
        x = coord_x,
        y = coord_y,
        size = 0 + seg_wid * 15,
        group = id_path
      ),
      colour = "gray90",
      show.legend = FALSE,
      lineend = "round",
      alpha = 1
    ) +
    jasmines:::theme_mono("grey10") +
    scale_size_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    #scale_color_gradientn(colours = shades) +
    scico::scale_color_scico(palette = "grayC") +
    coord_equal(
      xlim = c(-10, 8),
      ylim = c(0, 18)
    )

  ggsave(
    filename = here("image", fname),
    plot = pic,
    width = 5000/300,
    height = 5000/300,
    dpi = 300
  )
}


prepare_data <- function(
  data,
  palette = "viridis",
  colour = "order",
  alpha = c(0.3, 0)
){

  ribbon <- data
  ribbon$order <- ribbon[[colour]]
  if (is.character(palette)) {
    palette <- palette_named(palette)
  }
  alpha_init <- alpha[1]
  if (length(alpha) > 1) {alpha_decay <- alpha[2]}
  else {alpha_decay <- 0}
  if (!("order" %in% names(ribbon))) {
    ribbon$order <- 1:nrow(ribbon)
  }

  ribbon2 <- ribbon %>%
    rename(xend = x, yend = y) %>%
    mutate(time = time - 1) %>%
    filter(time > 0)
  ribbon <- ribbon %>%
    filter(time < max(time))
  ribbon$xend <- ribbon2$xend
  ribbon$yend <- ribbon2$yend
  ribbon$order <- ribbon2$order

  return(ribbon)
}


blend <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255,
           green = z[2, ]/255,
           blue = z[3, ]/255)
  return(z)
}


for(s in seed) {
  cat("\nusing seed ", s, "...\n", sep = "")
  genuary_tree(s)
}
