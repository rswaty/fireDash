

library(readr)
coniferChange <- read_csv("coniferChange.csv")
View(coniferChange)


library(tidyr)
library(dplyr)



df <- with(coniferChange, data.frame(to=rep(to, percent), Total=sequence(percent)))

write.csv(df, file = "coniferChangereps.csv")
