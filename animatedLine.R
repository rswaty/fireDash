

############ animated line #############


library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(scales)
library(readr)
library(htmltools)
library(plotly)

current_fire <- read_csv("data/modernFiresAcres.csv")

histModernFiresAcres <- read_csv("histModernFiresAcres.csv")

# plotly



accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


df <- histModernFiresAcres 

fig <- df %>% accumulate_by(~Year)


fig <- fig %>%
  plot_ly(
    x = ~Year, 
    y = ~Acres,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  )
fig <- fig %>% layout(
  xaxis = list(
    title = "Year",
    zeroline = F
  ),
  yaxis = list(
    title = "Acres",
    zeroline = F
  )
) 
fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  hide = T
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)

fig

# export using R-Studio tool in Viewer tab



