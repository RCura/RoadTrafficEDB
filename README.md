# RoadTrafficEDB
## Interactive exploration of Paris roads traffic

### Dependencies

Those packages are needed for the data handling : `tidyverse`, `lubridate`, and `sf`.

### Data

#### Download

The data can be downloaded on the official Paris Open-Data repository : https://opendata.paris.fr/page/home/
It's based on two datasets :

1. The geographic reference data : shapefile of all the road segments for which the counts are given : https://opendata.paris.fr/explore/dataset/referentiel-comptages-routiers/information/
2. The trafic data, that can be merged to the first dataset :  https://opendata.paris.fr/explore/dataset/comptages-routiers-permanents/information/

This app is designed to analyse the data of a whole year, and it was based on 2016 data.

Run the `src/1_Data_preparation.R` script for downloading data, after setting the parameters to the year that you want to explore (2013 to 2017 currently).

It will also clean the data, which consists of :
  - Removing all empty data (both Debit and TxOccupation are NA)
  - Transforming the geographic data so that each feature is unique and can be mapped in WGS84
  - Remove all trafic data that does not have a correspondance in the geographic data
  - Remove all geographic data that does not censor values
  - Save both those datasets as a compressed RData file
  
And then, prepare the data for exploration :
  - `Date`: contains the Date, without timestamp
  - `Month` : a factor of the english name of each month
  - `WDay` : a factor of the english name of each day of the week, starting on Monday
  - `Day` : the number of the day
  - `Hour` : The plain hour (eg. 5h45 is 5, athough all dates are hour-based on this dataset)

### Shiny Application

Once all of this is done, you can run the Shiny App with `shiny::runApp()`
You'll need those packages :
`shiny`, `scales`, `tidyverse`, `leaflet` ([Development version](http://rstudio.github.io/leaflet/)), `leaflet.extras` ([GitHub version](https://github.com/bhaskarvk/leaflet.extras)) and `shinyWidgets` ([GitHub version](https://github.com/dreamRs/shinyWidgets))

The running app can also be used here : http://shiny.parisgeo.cnrs.fr/RoadTrafficEDB/


### Licencing

Those scripts are under `GPLv3` licence, and the shiny app is licensed under the `GNU-AGPLv3` licence.
