library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(hrbrthemes)
library(scales)
hrbrthemes::import_roboto_condensed()

municipios <- read_csv("../data/municipios.txt", col_names = FALSE)
municipios<- sort(unlist(municipios$X1))

poblacion <- read_csv("../data/iter_19_cpv2010/iter_19_cpv2010.csv") 
poblacion <- poblacion %>% filter(loc == "0000" & mun != "000") %>% select(mun, nom_mun, pobtot) %>%
  mutate(nom_mun = str_replace(nom_mun, "Gral.", "General"), nom_mun = str_replace(nom_mun, "Dr.", "Doctor"), 
         nom_mun_may = toupper(nom_mun))

poblacion <- poblacion %>% filter(nom_mun_may %in% municipios) 
poblacion <- poblacion %>% mutate("pob_mil" = pobtot/1000, pob_diezmil = pobtot/10000, 
                                  pob_cienmil = pobtot/100000)

data <- read.csv("../data/crimen_latin.csv", sep = ",",
                 stringsAsFactors = FALSE, encoding = "latin-1")
#data$mun_name <- factor(data$mun_name, levels = municipios)
data$date <- as.Date(data$date)
data %>% filter(cassualties == max(cassualties))
data <- data %>% filter(mun_name != "")
min <- as.Date("2011-1-1")
break.vec <- c(seq(from = as.Date("2011-1-1"), to = as.Date("2018-3-1"),
                   by = "month"))
break.vec <- break.vec[seq(1,length(break.vec),6)]

data_pob <- data %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% 
  group_by(mun_name, month, year) %>% summarise(total_casualties = sum(cassualties)) %>% 
  inner_join(poblacion, by = c("mun_name" = "nom_mun_may" ))  %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-01")), 
         label = paste0(mun_name, " - ", round(pob_mil, digits = 2), "K"))

p <- ggplot(data_pob) +
  geom_segment(aes(x=date, xend=date, y=0, yend=total_casualties/pob_cienmil, size = 1),
               color = "grey") +
  geom_point( aes(x=date, y=total_casualties/pob_cienmil, color=mun_name,  size=2)) +
  scale_color_viridis_d() +
  facet_wrap(~label, ncol = 2, dir = "v") +
  theme_ipsum() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing.x = unit(0.8, "lines"),
    panel.spacing.y = unit(0.3, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.x = element_text(size = 24),
    axis.text.x = element_text(angle = 90, size = 16),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40)
  ) + xlab("Date") +
  ylab("Casualties") +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_date(breaks = break.vec, labels = date_format("%b-%Y"), 
               expand = expand_scale(0, 0))

png(paste0("../figures/lollipop_mty.png"), width = 1300, height = 2000)
p
dev.off()
