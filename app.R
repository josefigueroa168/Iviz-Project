source("modules/libs.R")
#TODO: Cleanup date format in column names
#TODO: Move data imports into separate file
country.shapes <- here("Data", "countries.geojson") %>%
  geojson_read(what = "sp")
cases.df <- readRDS(here("Data", "cases.rds"))
cases.ncol <- ncol(cases.df)
date.choices <- colnames(cases.df)

ui <- fluidPage(
  fluidRow(tags$h1("H1N1 Stuff")),
  fluidRow(
    column(3, 
           radioButtons(inputId = "date",
                       label = "Dates",
                       choices = date.choices,
                       selected = date.choices[1],
                       inline = FALSE # Might make horizontal later
                       )
           ), 
    column(9, leafletOutput("h1n1.map", height = "600px")) #Todo: Print da map
  ),
  hr()
) 

server <- function(input, output, session) {
  output$h1n1.map <- renderLeaflet({
    CASES <- as.data.frame(country.shapes$ADMIN)
    colnames(CASES) <- c("Country")
    rownames(CASES) <- CASES$Country
    CASES$Cases <- 0
    CASES[rownames(cases.df), "Cases"] <- cases.df[,input$date]
    CASES <- drop_na(CASES)
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = country.shapes$CASES
    )
    country.shapes$CASES <- CASES$Cases
    
    country.shapes %>%
      leaflet(options=leafletOptions(minZoom=2, maxZoom=18)) %>%
      setView(0, 39, 2) %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      #addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light")) %>%
      addPolygons(
        stroke=FALSE,
        smoothFactor = 0.2,
        fillOpacity = 0.7,
        color = ~pal(CASES))
    
  })
}

shinyApp(ui = ui, server = server)