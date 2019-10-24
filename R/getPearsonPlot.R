library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ComplexHeatmap)
library(circlize)


setwd("/home/diana/Workspace/PhD/chuchocentro/R/")
data <- read.csv("../data/crimen_latin.csv", sep = ",",
                 stringsAsFactors = FALSE, encoding = "latin-1")
data$date <- as.Date(data$date)
data <- data %>% filter(mun_name != "")

data_by_week <- data %>% group_by(mun_name, week_id = week(date), year = year(date)) %>% 
  summarise(events =n()) %>% arrange(year, week_id) %>% 
  mutate(week_str = str_pad(week_id, 2, pad = "0")) %>%
  unite(week_year, c( "year","week_str"))

data_by_week$week_year = as.factor(data_by_week$week_year)
data_by_week$week_year_n = as.numeric(data_by_week$week_year)

dbyw = data_by_week %>% ungroup() %>%
  select(mun_name, week_year_n, events)
dbyw <- dbyw %>% arrange(mun_name, week_year_n)
dbyw <- as.data.frame(dbyw %>% spread(week_year_n, events, fill = 0))
rownames(dbyw) <- dbyw$mun_name
dbyw <- dbyw[, 2:ncol(dbyw)]

csum <- t(apply(dbyw, MARGIN = 1, FUN = cumsum))
corrsum <- cor(t(csum), method = "pearson")

smatrix <- apply(dbyw, 1, FUN = function(x){
  cumsum(sample(x, length(x)))
})
scorr <- cor(smatrix, method = "pearson")




csum <- as.data.frame(csum)
csum["mun_name"] <- rownames(csum)

csump <- pivot_longer(csum, cols = colnames(csum)[1:ncol(csum) -1],
                      names_to = "week_id", values_to = "casualties")
csump$week_id <- as.integer(csump$week_id)
break.vec <- c(seq(from = as.Date("2011-1-1"), to = as.Date("2018-3-1"),
                   by = "month"))
break.vec <- break.vec[seq(1,length(break.vec),3)]

csump_min <- csump %>% 
  filter(mun_name %in% list("GARCÍA", "SALINAS VICTORIA"))

ggplot(data = csump_min, 
       aes(x = week_id, 
                         y = casualties, 
                         colour = mun_name,
                         group = mun_name)) +
  geom_line()


csum <- csum[,1:ncol(csum)-1]
Heatmap(t(corrsum),  col = colorRamp2(c(0, 0.2, 0.3, 0.7,  0.95, 1),
      c("white", "floralwhite", "blanchedalmond", "lightpink1",  "deeppink3", "deeppink4")),
)
