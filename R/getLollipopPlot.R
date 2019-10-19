library(ggplot2)
library(readr)
library(dplyr)
library(hrbrthemes)
library(scales)
hrbrthemes::import_roboto_condensed()

setwd("/home/diana/Workspace/PhD/chuchocentro/R/")
data <- read.csv("../data/crimen_latin.csv", sep = ",",
                 stringsAsFactors = FALSE, encoding = "latin-1")
data$date <- as.Date(data$date)
data %>% filter(cassualties == max(cassualties))
data <- data %>% filter(mun_name != "")
min <- as.Date("2011-1-1")
break.vec <- c(seq(from = as.Date("2011-1-1"), to = as.Date("2018-3-1"),
                   by = "month"))
break.vec <- break.vec[seq(1,length(break.vec),3)]
ggplot(data) +
  geom_segment(aes(x=date, xend=date, y=0, yend=cassualties), color = "grey") +
  geom_point( aes(x=date, y=cassualties, color=mun_name), size=0.2) +
  scale_color_viridis_d() +
  facet_wrap(~mun_name, ncol = 1) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 90, size = 6),
    axis.text.y = element_text(size = 6)
  ) + xlab("Date") +
  ylab("Cassualties") +
  scale_x_date(breaks = break.vec, labels = date_format("%b-%Y"), 
               expand = expand_scale(0, 0))

ggsave("../figures/lollipop_mty.png", 
       width = 1, height = 5,
       scale = 5,
       units = "in",
       dpi = 300)  
