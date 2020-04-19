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
  fluidRow(column(12, class="title", tags$h1("2009 H1N1 Rates"))),
  fluidRow(
    column(3,class = "sidebar",
           fluidRow(class = "content",
           HTML("&emsp;In the spring of 2009, a novel influenza A (H1N1) virus emerged. 
                It was detected first in the United States and spread quickly across 
                the United States and the world. This new H1N1 virus contained a unique 
                combination of influenza genes not previously identified in animals or 
                people. This virus was designated as influenza A (H1N1)pdm09 virus. Ten 
                years later work continues to better understand influenza, prevent disease
                , and prepare for the next pandemic.<sup>1</sup></br>
                &emsp;The H1N1 dataset provides reported global cases for H1N1 throughout the summer of 
                2009, provided by the World Health Organization (WHO)<sup>2</sup>. The cases
                were normalized with correlating 2009 population estimates provided by The World 
                Bank.<sup>3</sup> Our app color palette was selected with Paletton<sup>4</sup> and 
                RColorBrewer.<sup>5</sup>")
           ),
           fluidRow(class = "footer",
                    HTML(
                      "<a href='https://www.cdc.gov/'>1 Centers for Disease Control and Prevention</a></br>",
                      "<a href='https://www.who.int/csr/en/'>2 World Health Organization</a></br>",
                      "<a href='https://data.worldbank.org/indicator/sp.pop.totl'>3 The World Bank</a></br>",
                      "<a href='http://paletton.com/'>4 Paletton</a></br>",
                      "<a href='https://www.rdocumentation.org/packages/heatmaply/versions/1.0.0/topics/RColorBrewer_colors'>5 RColorBrewer</a>"
                    )
           )), 
    column(8, 
           radioButtons(inputId = "date",
                        label = "",
                        choices = date.choices,
                        #choiceNames = date.choices,
                        #choicesValues = date.choices,
                        selected = date.choices[1], # Currently first index represents total cases
                        inline = TRUE # Might make horizontal later
                        
           ),
           leafletOutput("h1n1.map", height = "600px"))
  ),
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