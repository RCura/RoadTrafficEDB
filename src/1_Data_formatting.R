library(tidyverse)
library(sf)
library(lubridate)

data_zip <- "../data/paris_capteurs_donnees_2015.zip"

routes <- st_read("../data/paris_capteurs_referentiel.shp") %>%
  rename(UID = ID_ARC_TRA) %>%
  mutate(UID = as.character(UID)) %>%
  select(-ID_ARC) %>%
  group_by(UID) %>%
  summarise()

library(rio)
batch_data <- import_list(data_zip)

trafic_data <-  bind_rows(batch_data) %>%
  rename(UID = V1, # id_arc_trafics : en jointure avec l’attribut « id_arc_trafic » du référentiel.
         Time = V2, # horodate (format ISO : YYYY-MM-DD HH:MM:SS)
         Debit = V3, # débit (nombre de véhicules comptés pendant l’heure)
         TxOccupation = V4) %>% # taux d’occupation (en pourcentage de temps d'occupation
                                # de la station de mesure par des véhicules sur l’heure)
  mutate(UID = as.character(UID),
         Time = ymd_hms(Time),
         Debit = as.numeric(Debit),
         TxOccupation = as.numeric(gsub(TxOccupation, pattern = ",", replacement = ".")))

trafic_data <- trafic_data %>%
  filter(UID %in% routes$UID)

trafic_grouped <- trafic_data %>%
  group_by(UID) %>%
  summarise(N = n())


routes <- routes %>%
  filter(UID %in% trafic_grouped$UID)

trafic_data <- trafic_data %>%
  mutate(DHour = hour(Time) + minute(Time) / 60)

trafic_data <- trafic_data %>%
  mutate(WDay = wday(Time, label = TRUE))

trafic_data <- trafic_data %>%  
  mutate(WDay = factor(WDay,levels(WDay)[c(2:7, 1)]))

trafic_data <- trafic_data %>%
    mutate(Month = month(Time, label = TRUE))

trafic_data <- trafic_data %>%
  mutate(Debit = ifelse(Debit < 0, yes =  NA, no = Debit))

routes <- routes %>%
  st_cast() %>%
  st_transform(4326)

save(trafic_data, routes, file = "../data/roadtraffic_data.RData", compress = TRUE)

