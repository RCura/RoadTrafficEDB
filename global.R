library(shiny)
library(scales)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets) #https://github.com/dreamRs/shinyWidgets

if (!exists("trafic_data")) {
  load("data/roadtraffic_data.RData")
}

meanTrafic <- trafic_data %>%
  group_by(UID) %>%
  summarise(Debit = mean(Debit, na.rm = TRUE),
            Occup = mean(TxOccupation, na.rm = TRUE))

routesMean <- routes %>%
  merge(meanTrafic,  by = "UID")
