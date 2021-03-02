

library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(scales)
library(readr)
library(htmltools)

current_fire <- read_csv("data/modernFiresAcres.csv")

animateCurr <-
  current_fire %>%
  ggplot(aes(x=Year, y=Acres)) +
  geom_line() +
  geom_point(size = 4) +
  ggtitle("Annual acres of fire in the lower 48 states, 1984-2020") +
  theme_ipsum() +
  ylab("") +
  shadow_wake(wake_length = 0.2,
              alpha = FALSE,
              size = NULL,
              colour = "grey",
              falloff = "linear") +
  scale_y_continuous(label=comma, limits = c(0, 11000000)) +
  scale_x_continuous(limits = c(1980, 2020)) +

  transition_reveal(Year)

animateCurr

anim_save("animated.gif", animateCurr)
