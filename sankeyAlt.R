

# ag = 2
# conifer = 1
# Conifer-Hardwood = 3
# Developed = 4
# Exotic Herbaceous = 5
# grassland =  6
# hardwood = 7
# riparian = 8
# shrubland = 9 
#https://www.hvitfeldt.me/blog/recreate-sankey-flow-chart/
library(tidyverse)
library(gganimate)

library(readr)
data <- read_csv("coniferChangereps.csv")
View(data)

sigmoid <- function(x_from, x_to, y_from, y_to, scale = 5, n = 100) {
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
         y = y * (y_to - y_from) + y_from)
}

n_points <- 400
data <- tibble(from = rep(4, n_points),
               to = sample(1:4, n_points, TRUE),
               color = sample(c("A", "B"), n_points, TRUE)) 

sigmoid(0, 1, as.numeric(data[2, 1]), as.numeric(data[2, 2]), 
        n = 100, scale = 10) %>%
  ggplot(aes(x, y)) +
  geom_point()

# single point
p <- sigmoid(0, 1, as.numeric(data[2, 1]), as.numeric(data[2, 2]),
             n = 100, scale = 10) %>%
  mutate(time = row_number()) %>%
  ggplot(aes(x, y, frame = time)) +
  geom_point()+

transition_time(time)

animate(p, nframes = 50)

# many points
p <- map_df(seq_len(nrow(data)), 
            ~ sigmoid(0, 1, as.numeric(data[.x, 1]), as.numeric(data[.x, 2])) %>%
              mutate(time = row_number() + .x,
                     y = y + runif(1, -0.25, 0.25))) %>%
  ggplot(aes(x, y, frame = time)) +
  geom_point() +

transition_time(time) 

animate(p, nframes = 50)

# add colors
point_data <- map_df(seq_len(nrow(data)), 
                     ~ sigmoid(0, 1, as.numeric(data[.x, 1]), as.numeric(data[.x, 2])) %>%
                       mutate(time = row_number() + .x,
                              y = y + runif(1, -0.25, 0.25),
                              id = .x) %>%
                       bind_cols(bind_rows(replicate(100, data[.x, -(1:2)], simplify = FALSE))))

p <- ggplot(point_data, aes(x, y, color = color, frame = time)) +
  geom_point(shape = 15) +
  theme_void() +
  guides(color = "none")+
  
  transition_time(time) 

animate(p, nframes = 50)


################  add counter
start_data_no_end <- point_data %>%
  group_by(id) %>%
  summarize(time = min(time)) %>%
  count(time) %>%
  arrange(time) %>%
  mutate(n = cumsum(n),
         x = 0.125, 
         y = 2,
         n = str_c("Follow the lives of ", n, " squares"))



# duplicating last number to fill gif
start_data <- start_data_no_end %>%
  bind_rows(
    map_df(unique(point_data$time[point_data$time > max(start_data_no_end$time)]),
           ~ slice(start_data_no_end, nrow(start_data_no_end)) %>%
             mutate(time = .x))
  )

p <- ggplot(point_data, aes(x, y, color = color, frame = time)) +
  geom_point(shape = 15) +
  geom_text(data = start_data, hjust = 0,
            aes(label = n, frame = time, x = x, y = y), color = "black") +
  theme_void() +
  guides(color = "none") +

transition_time(time) 

animate(p, nframes = 50)

### ending boxes
ending_box <- data %>%
  pull(to) %>%
  unique() %>%
  map_df(~ data.frame(x = c(1.01, 1.01, 1.1, 1.1, 1.01),
                      y = c(-0.3, 0.3, 0.3, -0.3, -0.3) + .x,
                      id = .x))


p <- ggplot(point_data, aes(x, y, color = color, frame = time)) +
  geom_point() +
  geom_text(data = start_data, 
            aes(label = n, frame = time, x = x, y = y), color = "black") +
  geom_path(data = ending_box,
            aes(x, y, group = id, frame = min(point_data$time),
                cumulative = TRUE), color = "grey70") +
  theme_void() +
  coord_cartesian(xlim = c(-0.05, 1.15)) +
  guides(color = "none") +
  
  transition_time(time) 

animate(p, nframes = 50)

## fill the boxes
end_points <- point_data %>% 
  group_by(id) %>%
  filter(time == max(time)) %>%
  ungroup()

end_lines <- map_df(end_points$id,
                    ~ data.frame(x = c(1.01, 1.01, 1.1, 1.1, 1.01),
                                 y = c(-0.01, 0.01, 0.01, -0.01, -0.01) + 
                                  as.numeric(end_points[.x, 2]),
                                 id = .x) %>%
                      bind_cols(bind_rows(replicate(5, end_points[.x, -(1:2)], simplify = FALSE)))
)

end_lines$id <- end_lines$id...3

suppressWarnings(p <- ggplot(point_data, aes(x, y, color = color, frame = time)) +
  geom_point() +
  geom_text(data = start_data, 
            aes(label = n, frame = time, x = x, y = y), color = "black") +
  geom_path(data = ending_box,
            aes(x, y, group = id, frame = min(point_data$time),
                cumulative = TRUE), color = "grey70") +
 geom_polygon(data = end_lines,
              aes(x, y, fill = color, frame = time, group = id,
                  cumulative = TRUE, color = color)) +
  theme_void() +
  coord_cartesian(xlim = c(-0.05, 1.15)) +
  guides(color = "none",
         fill = "none") +

  transition_time(time) )

animate(p, nframes = 50, fps = 10, end_pause = 2)
  
anim_save("ants.gif", animation = last_animation())
