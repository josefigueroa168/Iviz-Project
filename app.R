source("modules/libs.R")
#TODO: Cleanup date format in column names
#TODO: Move data imports into separate file
#TODO: Use population information to normalize data (i.e. Per cases 100k)

country.shapes <- here("Data", "countries.geojson") %>%
  geojson_read(what = "sp")
cases.df <- readRDS(here("Data", "cases_norm.rds"))
cases.ncol <- ncol(cases.df)
date.choices <- colnames(cases.df)

ui <- fluidPage(
  theme = "style.css",
  fluidRow(tags$h1("H1N1 Stuff")),
  fluidRow(
    column(3, 
           HTML("SIDEBAR")
           ), 
    column(9, 
           radioButtons(inputId = "date",
                        label = "Dates",
                        choices = date.choices,
                        #choiceNames = date.choices,
                        #choicesValues = date.choices,
                        selected = date.choices[1], # Currently first index represents total cases
                        inline = TRUE # Might make horizontal later
                        
           ),
           leafletOutput("h1n1.map", height = "600px"))
  ),
  hr(),
  #includeScript(here("www","style.js")),
  fluidRow(verbatimTextOutput("Click_text")) # Display text based on click location
  
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
    min.range <- round(min(cases.df[,input$date]))
    max.range <- round(max(cases.df[,input$date]))
    
    country.shapes %>%
      leaflet(options=leafletOptions(minZoom=2, maxZoom=18)) %>%
      setView(0, 39, 2) %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      #addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light")) %>%
      addPolygons(
        stroke=FALSE,
        smoothFactor = 0.2,
        fillOpacity = 0.5,
        color = ~pal(CASES),
        highlight = highlightOptions(weight=50,
                                     color="blue",
                                     fillOpacity=0.9,
                                     bringToFront=TRUE),
        label=paste(country.shapes$ADMIN, ": ", round(country.shapes$CASES), " cases per million.", sep='')) %>%
      addLegend(position = "bottomright",
              pal = pal, values = range(min.range, max.range), # Currently legend has a fixed scale, would be better to base it on highest value in table 
              title = "Cases per million",
              opacity = 1)
    
    
  })
  # Observer to look for clicks on shapes(countries) on the map
  observe({
    click<-input$h1n1.map_shape_click
    if(is.null(click))
      return()
    else
      leafletProxy("h1n1.map") %>%
      setView(lng=click$lng, lat = click$lat, zoom = 3) # Adjust zoom and position on click to center the country clicked on
    text2<-paste("You've selected point ", click$id)
    output$Click_text<-renderText({
      text2
    })
  })
}


shinyApp(ui = ui, server = server)