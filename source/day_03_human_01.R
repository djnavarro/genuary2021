# #genuary2021 #rstats make something human
# https://unsplash.com/photos/mFadZWL9UiI
library(ggplot2)
here::here(
  "other", "human1.jpg") %>%
asciify::ascii_map(
  alphabet = 0:9, rescale = 1) %>%
ggplot(aes(x, -y, label = label)) +
  geom_text(size = .5) +
  theme_void()
ggsave(here::here("image", "day_03_human_01.png"), width = 6, height = 6)
