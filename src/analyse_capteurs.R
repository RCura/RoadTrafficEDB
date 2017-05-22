library(tidyverse)
library(scales)
library(sf)
library(leaflet)
library(leaflet.extras)
library(lubridate)

trafic <- read_delim(unz("paris_capteurs_donnees_2015.zip",  filename = "donnees_trafic_capteurs_201501.txt"),
                     delim = "\t",locale = locale(decimal_mark = ","),
                     col_names = c("UID", "Date", "Nb",  "Tx"),
                     col_types = cols(
                       UID = "c",
                       Date = "T",
                       Nb = "d",
                       Tx = "d"
                     ),
                     guess_max = 1)

capteurs <- st_read("paris_capteurs_referentiel.shp", stringsAsFactors = FALSE) %>%
  mutate(ID_ARC = as.character(ID_ARC))

ptsCapteurs <- capteurs %>%
  mutate(geometry = st_line_sample(x = geometry, density = 1/5, type = "regular")) %>%
  st_cast(to = "POINT",group_or_split = TRUE) %>%
  group_by(ID_ARC) %>%
  mutate(NbPoints = n()) %>%
  ungroup() %>%
  st_sf() %>%
  st_transform(crs = 4326)

# uneDate <- sample_n(trafic, size = 1)
# traficData <- trafic %>%
#   filter(Date == uneDate$Date)
# 
# mapData <- ptsCapteurs %>%
#   group_by(ID_ARC) %>%
#   ungroup() %>%
#   left_join(traficData, by = c("ID_ARC" = "UID")) %>%
#   mutate(intensity = (Nb / NbPoints)^2) %>%
#   mutate(intensity = scale(intensity)) %>%
#   st_sf()

# leaflet(mapData) %>%
#   addProviderTiles('CartoDB.DarkMatter',
#                    group = "DarkMatter",
#                    options = providerTileOptions(opacity = 1)) %>%
#   addHeatmap(
#     data = mapData,
#     layerId = "heatmap",
#     intensity = ~ intensity,
#     minOpacity = .6,
#     radius = 5,
#     blur = 5
#   )

# plotData <- trafic %>%
#   mutate(hour = hour(Date)) %>%
#   group_by(hour) %>%
#   summarise(
#     SumNb = sum(Nb, na.rm = TRUE), 
#     MeanNb = mean(Nb, na.rm = TRUE),
#     MedNb = median(Nb, na.rm = TRUE)
#     ) %>%
#   mutate(SumNb = rescale(SumNb),
#          MeanNb = rescale(MeanNb),
#          MedNb = rescale(MedNb))

# ggplot(plotData) +
#   stat_smooth(mapping = aes(x = hour, y = SumNb), geom = "area", span = .4, fill = "red", alpha = .3)
# 
# capteurs <- st_read("paris_capteurs_referentiel.shp", stringsAsFactors = FALSE) %>%
#   mutate(ID_ARC = as.character(ID_ARC))
# 
# ptsCapteurs <- capteurs %>%
#   mutate(geometry = st_line_sample(x = geometry, density = 1/5, type = "regular")) %>%
#   st_cast(to = "POINT",group_or_split = TRUE) %>%
#   group_by(ID_ARC) %>%
#   mutate(NbPoints = n()) %>%
#   ungroup() %>%
#   st_sf() %>%
#   st_transform(crs = 4326)



bufferCapteurs = rgeos::gBuffer(as(capteurs, "Spatial"), width = 100)
HexPts <- spsample(bufferCapteurs, type="hexagonal", cellsize=500)
HexBins <- HexPoints2SpatialPolygons(hex = HexPts)
plot(HexBins)
