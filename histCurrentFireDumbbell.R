
# dumbbell chart

library(tidyverse)
library(scales)
library(extrafont)
library(ggplot2) 
library(ggalt)   
library(plotly)


currentFireAcresStates <- read_csv("data/currentFireAcresStates.csv")
View(currentFireAcresStates)

histAreaBurnedbyState <- read_csv("data/histAreaBurnedbyState.csv")
View(histAreaBurnedbyState)

historicalCurrentFire <- merge(currentFireAcresStates, histAreaBurnedbyState, by = "state")

historicalCurrentFire <- historicalCurrentFire %>%
  mutate(difference = historic_acres_burned - currentAverageAcres)

write.csv(historicalCurrentFire, file = "histCurStates.csv")


historicalCurrentFire$state <- factor(historicalCurrentFire$state , levels = rev(unique(historicalCurrentFire$state)))

write.csv(historicalCurrentFire, file = "historicalVScurrentDraft.csv")



ggplot(historicalCurrentFire, aes(x=currentAverageAcres, xend=historic_acres_burned, y=state)) + 
  #create a thick line between x and xend instead of using defaut 
  #provided by geom_dubbell
  geom_segment(aes(x=currentAverageAcres, 
                   xend=historic_acres_burned, 
                   y=state, 
                   yend=state), 
               color="#b2b2b2", size=1.5)+
  geom_dumbbell(color="light blue", 
                size_x=4.0, 
                size_xend = 4.0,
                colour_x="#edae52", 
                colour_xend = "#9fb059")+
  labs(x=NULL, y=NULL) +
  theme_bw(base_size = 14, base_family = "Calisto MT") +
  scale_x_continuous(label=comma)

##  take 2
# add mean lines

avgCur <- mean(historicalCurrentFire$currentAverageAcres)
avgHist <- mean(historicalCurrentFire$historic_acres_burned)


db <-
  ggplot(historicalCurrentFire, aes(x=currentAverageAcres, xend=historic_acres_burned, y=state)) + 
  #create a thick line between x and xend instead of using defaut 
  #provided by geom_dubbell
  geom_segment(aes(x=currentAverageAcres, 
                   xend=historic_acres_burned, 
                   y=state, 
                   yend=state), 
               color="#b2b2b2", size=1.5)+
  geom_dumbbell(color="light blue", 
                size_x=4.0, 
                size_xend = 4.0,
                colour_x="#edae52", 
                colour_xend = "#9fb059")+
  labs(x=NULL, y=NULL) +
  theme_bw(base_size = 14, base_family = "Calisto MT") +
  scale_x_continuous(label=comma) + 
  geom_vline(xintercept = avgCur) +
  geom_vline(xintercept = avgHist)

db




vlineCUR <- function(x = avgCur, color = "grey") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}

vlineHIST <- function(x = avgHist, color = "grey") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}








fig <- plot_ly(historicalCurrentFire, color = I("gray80"))
fig <- fig %>% add_segments(x = ~currentAverageAcres, xend = ~historic_acres_burned, y = ~state, yend = ~state, showlegend = FALSE)
fig <- fig %>% add_markers(x = ~currentAverageAcres, y = ~state, name = "Current", color = I("#edae52"), showlegend = FALSE)
fig <- fig %>% add_markers(x = ~historic_acres_burned, y = ~state, name = "Historical", color = I("#9fb059"), showlegend = FALSE)
fig <- fig %>% layout(
  xaxis = list(title = "Acres (in millions)"),
  yaxis = list(title = "", dtick = 1),
  margin = list(l = 65),
  shapes = list(vlineCUR(), vlineHIST())
)

fig