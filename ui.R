library(shiny)

shinyUI(fluidPage(
  fluidRow(style = "height: 50px;text-align:center;",
           h1("RoadTrafficEDB")),
  fluidRow(
    column(4,
           fluidRow(style = "height: 50px;text-align:center;", h4("Débit de voitures / heure")),
           fluidRow(plotOutput("monthPlot_debit", height = "250px",
                               brush = brushOpts(id = "monthPlot_brush", direction = "x", resetOnNew = FALSE))),
           fluidRow(plotOutput("wdayPlot_debit", height = "250px",
                               brush = brushOpts(id = "wdayPlot_brush", direction = "x", resetOnNew = FALSE)))
           ),
    column(4,
           fluidRow(style = "height: 50px;",
                    column(6, radioGroupButtons(inputId = "mapvar", size = "xs",
                                      label = "Type de données",
                                      choices = c("Débit" = "Debit",
                                                  "Taux d'occupation" = "TxOccupation"),
                                      selected = "Debit",
                                      justified = TRUE,
                                      status = "primary")),
                    column(6,radioGroupButtons(inputId = "typevar", size = "xs",
                                      label = "Type de représentation",
                                      choices = c("Brut", "Relatif"),
                                      selected = "Brut",
                                      justified = TRUE,
                                      status = "primary"))),
           fluidRow(
             leafletOutput("map", height = "500px"))
           ),
    column(4,
           fluidRow(style = "height: 50px;text-align:center;", h4("Taux d'occupation de la route")),
           fluidRow(plotOutput("monthPlot_occup", height = "250px",
                               brush = brushOpts(id = "monthPlot_brush", direction = "x", resetOnNew = FALSE))),
           fluidRow(plotOutput("wdayPlot_occup", height = "250px",
                               brush = brushOpts(id = "wdayPlot_brush", direction = "x", resetOnNew = FALSE)))
    )
  ),
  fluidRow(
    column(3, plotOutput("hourPlot_debit", height = "250px",
                         brush = brushOpts(id = "hourPlot_brush", direction = "x", resetOnNew = FALSE))),
    column(6, plotOutput("timePlot", height = "250px",
                         brush = brushOpts(id = "timePlot_brush", direction = "x", resetOnNew = FALSE))),
    column(3, plotOutput("hourPlot_occup", height = "250px",
                         brush = brushOpts(id = "hourPlot_brush", direction = "x", resetOnNew = FALSE)))
  )
))
