library(tidyverse)
library(lubridate)
# Downloading traffic data

baseURL <- "https://opendata.paris.fr/api/datasets/1.0/comptages-routiers-permanents/attachments/"
yearToDownload <- 2016
fileName <- sprintf("%i_paris_donnees_trafic_capteurs_zip", yearToDownload)
fileURL <- paste0(baseURL, fileName)

outputName <- paste0("data/", fileName, ".zip")
download.file(url = fileURL, destfile = outputName)
unzip(zipfile = outputName, exdir = "data/")

# Read data into R

trafic_data <- dir(path = "data/", pattern = "*.txt", full.names = TRUE) %>%
  map_df(read_delim, delim = "\t",
         col_names = c("UID", "Time", "Debit", "TxOccupation"),
         col_types = "cTid",
         locale = locale(decimal_mark = ","))

# Removing lines with no Debit / TxOccupation :Some lines are missing both TxOccup and Debit
trafic_data <- trafic_data %>%
  filter(!is.na(TxOccupation) & !is.na(Debit)) # Note that for 2016, there is no line with NA in either Debit or TxOccupation


# Downloading geographic data
gisDataURL <- "https://opendata.paris.fr/explore/dataset/referentiel-comptages-routiers/download/?format=shp&timezone=Europe/Berlin"

download.file(gisDataURL, destfile = "data/routes.zip")
routes <- st_read(unzip("data/routes.zip"), stringsAsFactors = FALSE) %>%
  mutate(UID = id_arc_tra) %>%
  group_by(UID) %>% #  Some road are composed of multiple segments, for ex. Place de l'étoile
  summarise() %>%
  st_cast() %>% # Converting all polylines to multipolylines for consistency
  st_transform(4326) # Was in Lambert93, to map easily

# Cleaning trafic_data again to remove non-referenced data
trafic_data <- trafic_data %>%
  filter(UID %in% routes$UID)

# Removing routes that have no value
grouped_trafic <- trafic_data %>% group_by(UID) %>% summarise(N = n())
routes <- routes %>%
  filter(UID %in% grouped_trafic$UID)

# Trafic_data preparation for exploration

trafic_data <- trafic_data %>%
  mutate(Date = date(Time)) %>%
  mutate(Hour = hour(Time)) %>%
  mutate(WDay = wday(Date, label = TRUE)) %>%
  mutate(WDay = factor(WDay,levels(WDay)[c(2:7, 1)])) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  mutate(Debit = ifelse(Debit < 0, yes =  NA, no = Debit))


save(trafic_data, routes, file = "data/roadtraffic_data.RData", compress = TRUE)
