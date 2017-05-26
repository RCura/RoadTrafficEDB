
library(shiny)

shinyServer(function(input, output, session) {
  output$map <- renderLeaflet({
      pal <- colorNumeric(
        palette = "magma",
        domain = routesMean$Debit,
        na.color = "#dedede",
        reverse = TRUE
      )
      
      leaflet(data = routesMean) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(opacity = 0.3)) %>%
        addPolylines(
          color = ~ pal(Debit), weight = 5
        ) %>%
        addLegend("bottomright", pal = pal, values = ~Debit,
                  title = "Débit moyen",
                  opacity = 1
        ) %>%
        addDrawToolbar(polylineOptions = FALSE, polygonOptions = FALSE,
                       circleOptions = FALSE, markerOptions = FALSE,
                       singleFeature = TRUE, editOptions = editToolbarOptions(
                         edit = FALSE, remove = TRUE)
                       )
  })
  
  observe({
    myLabelFormat <- labelFormat(digits = 0)
    
    if (length(filtredTemporalData()) > 1) {
      selectionData <- filtredTemporalData() %>%
        group_by(UID) %>%
        summarise(sMeanDebit = mean(Debit, na.rm = TRUE),
                  sMeanOccup = mean(TxOccupation, na.rm = TRUE))
      
      
      mapData <- routesMean %>%
        merge(selectionData, by = "UID") %>%
        mutate(diffDebit = (sMeanDebit - Debit) / Debit * 100,
               diffOccup = (sMeanOccup - Occup) / Occup * 100)
      if (input$typevar == "Brut"){
        mapData <- mapData %>%
          mutate(Debit = sMeanDebit,
                 Occup = sMeanOccup)
      } else {
        mapData <- mapData %>%
          mutate(Debit = diffDebit,
                 Occup = diffOccup)
        myLabelFormat <- labelFormat(suffix = "%")
      }
    } else {
      mapData <- routesMean
    }
    
    
    mapProxy <- leafletProxy("map", data = mapData, session = session) %>%
      clearControls() %>%
      clearShapes() %>%
      addDrawToolbar(polylineOptions = FALSE, polygonOptions = FALSE,
                     circleOptions = FALSE, markerOptions = FALSE,
                     singleFeature = TRUE, editOptions = editToolbarOptions(
                       edit = FALSE, remove = TRUE)
      )
    
    if (input$mapvar == "Debit"){
      pal <- colorNumeric(
        palette = "magma",
        domain = mapData$Debit,
        na.color = "#dedede",
        reverse = TRUE
      )
      
      mapProxy %>%
        addPolylines(
          color = ~ pal(Debit), weight = 5
        ) %>%
        addLegend("bottomright", pal = pal, values = ~Debit,
                  title = "Débit moyen",
                  opacity = 1, labFormat = myLabelFormat
        )
      
    } else {
      pal <- colorNumeric(
        palette = "magma",
        domain = mapData$Occup,
        na.color = "#dedede",
        reverse = TRUE
      )
        
      mapProxy %>%
        addPolylines(
          color = ~ pal(Occup), weight = 5
        ) %>%
        addLegend("bottomright", pal = pal, values = ~Occup,
                  title = "Occupation moyenne",
                  labFormat = labelFormat(suffix =  "%"),
                  opacity = 1
        )
    }
  })
  
  filtredSpatialData <- reactive({
    if (length(input$map_draw_all_features$features) > 0) {
      selectedPolygon <- list(
        matrix(
          unlist(input$map_draw_all_features$features[[1]]$geometry$coordinates),
          ncol = 2,
          byrow = TRUE
        )
      ) %>%
        st_polygon()
      intersectedRoutes <- st_intersects(x = selectedPolygon, y = routes) %>%
        unlist()
      
      selectedRoutes <- routes[intersectedRoutes,]
      
      
      tmpFiltredSpatialData <- trafic_data %>%
        filter(UID %in% selectedRoutes$UID)
      
      return(tmpFiltredSpatialData)
    }
  })
  
  filtredTemporalData <- reactive({
    noSelection <- TRUE
    currentlyFiltred <- trafic_data
    
    if (!is.null(input$hourPlot_brush)) {
      thisSelection <- input$hourPlot_brush
      currentlyFiltred <- currentlyFiltred %>%
        filter(Hour >= thisSelection$xmin, Hour <= thisSelection$xmax)
      noSelection <- FALSE
    }
    
    if (!is.null(input$wdayPlot_brush)) {
      thisSelection <- input$wdayPlot_brush
      currentlyFiltred <- currentlyFiltred %>%
        filter(as.numeric(WDay) >= round(thisSelection$xmin),
               as.numeric(WDay) <= round(thisSelection$xmax))
      noSelection <- FALSE
    }
    
    if (!is.null(input$monthPlot_brush)) {
      thisSelection <- input$monthPlot_brush
      currentlyFiltred <- currentlyFiltred %>%
        filter(as.numeric(Month) >= round(thisSelection$xmin),
               as.numeric(Month) <= round(thisSelection$xmax))
      noSelection <- FALSE
    }
    
    if (!is.null(input$timePlot_brush)) {
      thisSelection <- input$timePlot_brush
      currentlyFiltred <- currentlyFiltred %>%
        filter(Time >= thisSelection$xmin, Time <= thisSelection$xmax)
      noSelection <- FALSE
    }
    
    if (!noSelection) {
      return(currentlyFiltred)
    }
  })
  
  output$monthPlot_debit <- renderPlot({
    monthPlot_data <- trafic_data %>%
      group_by(Month) %>%
      summarise(meanDebit = mean(Debit, na.rm = TRUE))
    
    monthPlot <- ggplot(monthPlot_data, aes(Month, meanDebit)) +
      geom_col(na.rm = TRUE, fill = "blue", alpha = .35) +
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1){
      selData <- filtredSpatialData() %>%
        group_by(Month) %>%
        summarise(meanDebit = mean(Debit, na.rm = TRUE))
      
      monthPlot <- monthPlot +
        geom_col(data = selData, na.rm = TRUE, fill = "red", alpha = .35)
    }
    
    return(monthPlot)
  })
  
  output$monthPlot_occup <- renderPlot({
    monthPlot_data <- trafic_data %>%
      group_by(Month) %>%
      summarise(meanOccup = mean(TxOccupation, na.rm = TRUE))
    
    monthPlot <- ggplot(monthPlot_data, aes(Month, meanOccup)) +
      geom_col(na.rm = TRUE, fill = "blue", alpha = .35) +
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1){
      selData <- filtredSpatialData() %>%
        group_by(Month) %>%
        summarise(meanOccup = mean(TxOccupation, na.rm = TRUE))
      
      monthPlot <- monthPlot +
        geom_col(data = selData,na.rm = TRUE, fill = "red", alpha = .35)
    }
    return(monthPlot)
  })
  
  output$wdayPlot_debit <- renderPlot({
    wdayPlot_data <- trafic_data %>%
      group_by(WDay) %>%
      summarise(meanDebit = mean(Debit, na.rm = TRUE))
    
    wdayPlot <- ggplot(wdayPlot_data, aes(WDay, meanDebit)) +
      geom_col(na.rm = TRUE, fill = "blue", alpha = .35) +
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1){
      selData <- filtredSpatialData() %>%
        group_by(WDay) %>%
        summarise(meanDebit = mean(Debit, na.rm = TRUE))
      
      wdayPlot <- wdayPlot +
        geom_col(data = selData,na.rm = TRUE, fill = "red", alpha = .35)
    }
    return(wdayPlot)
  })
  
  output$wdayPlot_occup <- renderPlot({
    wdayPlot_data <- trafic_data %>%
      group_by(WDay) %>%
      summarise(meanOccup = mean(TxOccupation, na.rm = TRUE))
    
    wdayPlot <- ggplot(wdayPlot_data, aes(WDay, meanOccup)) +
      geom_col(na.rm = TRUE, fill = "blue", alpha = .35) +
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1){
      selData <- filtredSpatialData() %>%
        group_by(WDay) %>%
        summarise(meanOccup = mean(TxOccupation, na.rm = TRUE))
      
      wdayPlot <- wdayPlot +
        geom_col(data = selData,na.rm = TRUE, fill = "red", alpha = .35)
    }
    return(wdayPlot)
  })
  
  output$hourPlot_debit <- renderPlot({
    hourPlot_data <- trafic_data %>%
      group_by(Hour) %>%
      summarise(meanDebit = mean(Debit, na.rm = TRUE))
    
    hourPlot <- ggplot(hourPlot_data, aes(Hour, meanDebit)) +
      stat_smooth(se = FALSE, geom = "area", fill = "blue", alpha = .35, na.rm = TRUE) +
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1){
      selData <- filtredSpatialData() %>%
        group_by(Hour) %>%
        summarise(meanDebit = mean(Debit, na.rm = TRUE))
      
      hourPlot <- hourPlot +
        stat_smooth(data = selData, se = FALSE, geom = "area", fill = "red", alpha = .35, na.rm = TRUE)
    }
    return(hourPlot)
  })
  
  output$hourPlot_occup <- renderPlot({
    hourPlot_data <- trafic_data %>%
      group_by(Hour) %>%
      summarise(meanOccup = mean(TxOccupation, na.rm = TRUE))
    
    hourPlot <- ggplot(hourPlot_data, aes(Hour, meanOccup)) +
      stat_smooth(se = FALSE, geom = "area", fill = "blue", alpha = .35, na.rm = TRUE) +
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1){
      selData <- filtredSpatialData() %>%
        group_by(Hour) %>%
        summarise(meanOccup = mean(TxOccupation, na.rm = TRUE))
      
      hourPlot <- hourPlot +
        stat_smooth(data = selData, se = FALSE, geom = "area", fill = "red", alpha = .35, na.rm = TRUE)
    }
    return(hourPlot)
  })
  
  output$timePlot <- renderPlot({
    timePlot_data <- trafic_data %>%
      select(Time, Debit, TxOccupation) %>%
      group_by(Time) %>%
      summarise(meanDebit = mean(Debit, na.rm = TRUE),
                meanOccup = mean(TxOccupation, na.rm = TRUE)) %>%
      gather(key = Variable,
             value = Value,
             meanDebit, meanOccup)
    
    
    timePlot <- ggplot(timePlot_data, aes(Time, Value)) +
      geom_smooth(se = FALSE, col = "blue", alpha = .35, na.rm = TRUE) +
      facet_wrap(~Variable, ncol = 1, scales = "free_y") +
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1){
      selData <- filtredSpatialData() %>%
        select(Time, Debit, TxOccupation) %>%
        group_by(Time) %>%
        summarise(meanDebit = mean(Debit, na.rm = TRUE),
                  meanOccup = mean(TxOccupation, na.rm = TRUE)) %>%
        gather(key = Variable,
               value = Value,
               meanDebit, meanOccup)
      
      timePlot <- timePlot +
        geom_smooth(data = selData, se = FALSE, col = "red", alpha = .35, na.rm = TRUE)
    }
    return(timePlot)
  })
  
  
})
