
library(shiny)

ui <- fluidPage(
  titlePanel("CarCensorEDB"),
  fluidRow(
    column(6, plotOutput("hourPlot", brush = brushOpts(id = "hourfreq_brush", direction = "x"))),
    column(6, leafletOutput("map"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  mapData <- ptsCapteurs
  
  csData <- reactiveValues(traficData = trafic %>%
                             mutate(hour = as.integer(hour(Date))),
                           traficSel = NA)

  output$hourPlot <- renderPlot({
    req(csData$traficData)
    hourPlotData <- csData$traficData %>%
      group_by(hour) %>%
      summarise(Nb = mean(Nb, na.rm = TRUE)) %>%
      mutate(Nb = rescale(Nb))

    ggplot(hourPlotData, aes(x = hour, y = Nb))  +
      geom_line() +
      # stat_smooth(geom = "area", col = "#053144", fill = "#43a2ca", alpha = 0.3, span = .3) +
      # scale_x_continuous("Heure", breaks = c(0, 6, 12, 18, 24),
      #                    minor_breaks = c(3, 9, 15, 21),
      #                    labels = function(x){paste(x, "h")}) +
      scale_y_continuous("Densité", labels = scales::percent)
  })
  
  output$map <- renderLeaflet({
     req(mapData)
    # 
    mapData <- mapData %>%
      group_by(ID_ARC) %>%
      ungroup() %>%
      left_join(csData$traficData %>%
                group_by(UID) %>%
                  summarise(Nb = sum(Nb, na.rm = TRUE)),
                by = c("ID_ARC" = "UID")) %>%
      mutate(intensity = (Nb / NbPoints)^2) %>%
      mutate(intensity = scale(intensity)) %>%
      st_sf()

    leaflet() %>%
      addProviderTiles('CartoDB.DarkMatter',
                       group = "DarkMatter",
                       options = providerTileOptions(opacity = 1)) %>%
      addHeatmap(
        data = mapData,
        layerId = "heatmap",
        intensity = ~ intensity,
        minOpacity = 1E-1,
        radius = 2,
        blur = 3,
        cellSize = .1
      )
    })
  
  observe({
    req(csData$traficSel)
    
    selData <- mapData %>%
      group_by(ID_ARC) %>%
      ungroup() %>%
      left_join(csData$traficSel %>%
                  group_by(UID) %>%
                  summarise(Nb = sum(Nb, na.rm = TRUE)),
                by = c("ID_ARC" = "UID")) %>%
      mutate(intensity = (Nb / NbPoints)) %>%
      mutate(intensity = scale(intensity)) %>%
      st_sf()
    
    mapProxy <- leafletProxy("map")
    mapProxy %>%
      clearHeatmap() %>%
      addHeatmap(
        data = selData,
        layerId = "heatmap",
        intensity = ~ intensity,
        minOpacity = 1E-1,
        radius = 2,
        blur = 3
      )
  })
  
  
  observe({
    req(input$hourfreq_brush)
    timeSel <- input$hourfreq_brush
     csData$traficSel <- csData$traficData %>%
       filter(hour >= timeSel$xmin,
              hour <= timeSel$xmax)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
