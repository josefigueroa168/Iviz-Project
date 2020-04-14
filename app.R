source("modules/libs.R")

ui <- fluidPage(
  fluidRow(tags$h1("Filler Input")),
  #fluidRow(
  #  column(4, sliderInput()), #Todo: Add dates in order for input
  #  column(8, leafletOutput()) #Todo: Print da map
  #),
  hr()
) 

server <- function(input, output, session) {
  #Todo: Make da map
}

shinyApp(ui = ui, server = server)