




library(hrbrthemes)
library(waffle)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)



# with fire data


GvFire<- read_csv("data/histFireGVGroupedBar.csv")
View(GvFire)

# try to fix y axis

ks <- function (x) { number_format(accuracy = 1,
                                   suffix = "M",
                                   big.mark = ",")(x) }






gvWaffle <- ggplot(GvFire, aes(fill = fire, values = acreHndThou)) +
  geom_waffle(color = "white", size = .9, n_rows = 10, flip = TRUE) +
  facet_wrap(~groupveg, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_fill_manual(values = c("#FFC107", "#1E88E5", "#008870"), # mixed, replacement, surface
                    name = "Fire Severity", 
                    labels = c("Mixed (25-75% top kill)",
                               "Replacement (>75%)",              
                               "Surface (< 25%)")) + 
  coord_equal() +
  labs(
    title = "Historical fire severity by broad vegetation groups",
    subtitle = "One square = 100K acres",
    caption = "Data from landfire.gov",
    x = "Vegetation group",
    y = "Acres burned per year (~1300s- ~1600s)" ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels=ks) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) + 
  theme(legend.position = c(0.8, 0.8)) + theme(panel.spacing.x = unit(1, "lines")) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

gvWaffle 


ggsave(filename="test.png",width=10,height=8,units='in',dpi=300)
