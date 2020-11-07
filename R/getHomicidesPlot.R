library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggthemes)

mma_pop <- read_tsv("../data/INEGI_poblacion_NL.txt", col_names = c("mun", "pob"))
pob <- read_tsv("../data/INEGI_poblacion.txt", col_names = c("place", "pob"), skip = 1)

h_estados <- read_tsv("../data/INEGI_homicidios.txt", col_names = c("year", "month", "mexico", "chih", "nl", "yuc"),
                      skip = 1)
h_municipios <- read_tsv("../data/INEGI_homicidios_NL.txt")
mma <- read_tsv("../data/municipios.txt", col_names = F) %>% unlist(use.names = F)
mma  <- iconv(mma,from="UTF-8",to="ASCII//TRANSLIT")

mma_pop$mun  <- iconv(toupper(mma_pop$mun),from="UTF-8",to="ASCII//TRANSLIT")
mma[!mma %in% mma_pop$mun]

mma_pop <- mma_pop %>% mutate(mun = str_replace(mun, "DR\\.", "DOCTOR"))
mma[!mma %in% mma_pop$mun]
# [1] "SAN PEDRO GARZA GARCIA" "GENERAL ESCOBEDO"       "GENERAL ZUAZUA"         "EL CARMEN"  
mma_pop <- mma_pop %>% mutate(mun = str_replace(mun, "GRAL\\.", "GENERAL"))
mma[!mma %in% mma_pop$mun]
# [1] "EL CARMEN"
mma[mma == "EL CARMEN"] = "CARMEN"
mma[!mma %in% mma_pop$mun]
# character(0)
mma_pop <- mma_pop %>% filter(mun %in% mma) %>% select(pob) %>% sum()

pob$pob <- as.integer(str_replace_all(pob$pob, " ", ""))
poblacion <- c(pob$pob, mma_pop)
names(poblacion) <- c("chih", "nl", "yuc", "mexico", "mma")
pob_cien <- poblacion /100000

h_estados <- h_estados %>% filter(!month %in% c("Total", "No especificado"))
colnames(h_municipios) <- toupper(colnames(h_municipios))

mma[!mma %in% colnames(h_municipios)]
# [1] "SAN PEDRO GARZA GARCÍA" "JUÁREZ"                 "CADEREYTA JIMÉNEZ"      "GARCÍA"                
# [5] "CIÉNEGA DE FLORES"      "MARÍN"                  "CARMEN"                 "DOCTOR GONZÁLEZ" 
colnames(h_municipios)  <- iconv(colnames(h_municipios),from="UTF-8",to="ASCII//TRANSLIT")
mma[!mma %in% colnames(h_municipios)]
# [1] "CARMEN"
mma[mma == "CARMEN"] <- "EL CARMEN"
mma[!mma %in% colnames(h_municipios)]
# character(0)

colnames(h_municipios) [1:2] <- c("year", "month")
h_municipios <- h_municipios %>% select(year, month, all_of(mma))
h_municipios[is.na(h_municipios)] = 0
h_municipios <- h_municipios %>% filter(!month %in% c("Total", "No especificado"))
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
           "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
h_municipios$month <- factor(h_municipios$month, levels = meses)
h_estados$month <- factor(h_estados$month, levels = meses)
h_municipios$monthnum <- as.integer(h_municipios$month)
h_estados$monthnum <- as.integer(h_estados$month)
h_municipios <- h_municipios %>% mutate(fecha = as.Date(paste(year, monthnum, "1",sep = "-"), format = "%Y-%m-%d")) %>% 
  select(fecha, MONTERREY:`DOCTOR GONZALEZ`)
h_municipios$mma <- rowSums(h_municipios%>% select(-fecha))
h_estados <- h_estados %>% mutate(fecha = as.Date(paste(year, monthnum, "1",sep = "-"), format = "%Y-%m-%d")) %>%
  select(fecha, mexico:yuc)
homicides <- h_estados %>% inner_join(h_municipios %>% select(fecha, mma), by = "fecha")

homicides$mexico_cien <- homicides$mexico/pob_cien["mexico"] 
homicides$nl_cien <- homicides$nl/pob_cien["nl"] 
homicides$yuc_cien <- homicides$yuc/pob_cien["yuc"]
homicides$chih_cien <- homicides$chih/pob_cien["chih"]
homicides$mma_cien <- homicides$mma/pob_cien["mma"]

homicides[is.na(homicides)] = 0
homicides <- homicides %>% filter(year(fecha) >= 2000)

homicides <- homicides %>% select(fecha, mexico_cien:mma_cien) %>%
  pivot_longer(mexico_cien:mma_cien, names_to = "place", values_to = "cassualties")


p <- ggplot(homicides, aes(x = fecha, y = cassualties, color = place)) +
  geom_line() +
  ylab("Casualties per 100,000 hab.") +
  xlab("Year") +
  theme_base(base_size = 20) +
 # facet_wrap(~place, ncol = 1) +
  theme(legend.title = element_blank(),
        axis.title.y =  element_text(size = 15),
        legend.spacing.y = unit(1, "cm")) +
  scale_color_brewer(palette="Dark2", labels = c("Chihuahua", "Mexico", "MMA", "Nuevo León", "Yucatán"))

png(paste0("../figures/plot_homicides.png"), width = 1000, height = 300)
p
dev.off()
