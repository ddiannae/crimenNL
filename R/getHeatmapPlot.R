library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(hrbrthemes)
library(scales)
library(lubridate)
library(ComplexHeatmap)
library(circlize)

hrbrthemes::import_roboto_condensed()

setwd("/home/diana/Workspace/PhD/chuchocentro/R/")
data <- read.csv("../data/crimen_latin.csv", sep = ",",
                 stringsAsFactors = FALSE, encoding = "latin-1")
data$date <- as.Date(data$date)
data <- data %>% filter(mun_name != "")

data_by_week <- data %>% group_by(object_id, week_id = week(date), year = year(date)) %>% 
  summarise(events =n()) %>% arrange(year, week_id) %>% 
  mutate(week_str = str_pad(week_id, 2, pad = "0")) %>%
  unite(week_year, c( "year","week_str"))

data_by_week$week_year = as.factor(data_by_week$week_year)
data_by_week$week_year = as.numeric(data_by_week$week_year)

data_by_week = data_by_week %>% ungroup() %>%
  select(object_id, week_year, events)
data_by_week <- data_by_week %>% arrange(object_id, week_year)
dbyw <- as.data.frame(data_by_week %>% spread(week_year, events, fill = 0))
rownames(dbyw) <- dbyw$object_id
dbyw <- dbyw[, 2:ncol(dbyw)]
dbyw <- data.matrix(dbyw)
dbyw <- dbyw[rowSums(dbyw) > 5, ]

neigh_names <- data %>% select(object_id, neighborhood_name) %>% unique()
rownames(neigh_names) <- neigh_names$object_id
neigh_names <- neigh_names[rownames(dbyw), "neighborhood_name"]
rownames(dbyw) <- neigh_names
h1 = Heatmap(dbyw, cluster_columns = FALSE, 
        col = colorRamp2(c(0, 2, max(dbyw)), c("white", "red3", "red4")),
        show_column_names = FALSE, row_names_gp = gpar(fontsize = 12),
        heatmap_legend_param = list(title = "Events", 
                                    title_gp = gpar(fontsize = 20),
                                    title_gp = gpar(fontsize = 16),
                                    legend_height = unit(5, "cm"),
                                    title_position = "lefttop-rot"))
draw(h1, heatmap_legend_side = "left")
dim(dbyw)
