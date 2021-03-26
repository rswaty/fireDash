
## non-chord alternatives to grouped bar chart

library(pacman)

p_load(ggplot2, scales)


df <- read_csv("data/histFireGVGroupedBar.csv")
View(df)

gg <- ggplot(data=df, aes(x=fire, y=acres))
# use points, colored by Quarter
gg <- gg + geom_point(aes(color=fire), size=5)
# make strips by nosql db factor
gg <- gg + facet_grid(groupveg~.)
# rotate the plot
gg <- gg + coord_flip()
# get rid of most of the junk
gg <- gg + theme_bw()
# add a title
gg <- gg + labs(x="", title="Fire by Groupveg")
# get rid of the legend
gg <- gg + theme(legend.position = "none")
# ensure the strip is gone
gg <- gg + theme(strip.text.x = element_blank())

gg <- gg +  scale_y_continuous(label=comma)




gg