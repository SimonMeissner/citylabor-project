# https://shiny.rstudio.com/articles/reactivity-overview.html


# install R packages



# load R packages
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)



plant_data <- read.csv("src/20211214-plants-scraped.csv", header=TRUE) # Load test plant data




  # UI -------------------------------------------------------------------------------------------
  ui <- fluidPage(theme = shinytheme("cerulean"),
                  tags$head(includeHTML(("www/geolocation.html"))),
                  
                  
    navbarPage(
      
      

      title = "Urban Garden App",
      id = "navbar",
      
      # Page1, used as a welcome landing page with descriptions and redirecting features for the user 
      tabPanel( title = "Welcome", value = "tab1",
               mainPanel(
                 
                  h1("Under Construction!",style = "font-weight: 500; color: red"), #Under Construction sign
                  h1("Welcome"),
                  p("This is the landing page of our webapp build in R-Shiny!
                    It could be used to describe our app and suggests the user
                    to use one of the two options."),
                  
                  actionButton("redirect1", "What to plant"),
                  
                  actionButton("redirect2", "When to plant"),
                  p("By clicking one of the options the user gets redirected to the different pages"),
                  
                  
                  h1("Testing Location api",style = "font-weight: 500; color: red"),
                  
                  
                  
                  verbatimTextOutput("location")
               )
               
      ), # end Page1
      
      # Page2, used for providing the "When to plant" service
      tabPanel( title = "What to plant!", value = "tab2",
               sidebarLayout(
                sidebarPanel(
                 tags$h3("What can I plant?"),
                 
                 textInput("space", "How much space is available: (square inch)", ""),
                 # Select growth range
                 dateRangeInput("growthrange", strong("Select timeframe from planting to harvesting"), start = NULL, end = NULL,
                                min = "2021-01-01", max = "2030-01-01", startview =  "year", weekstart = "1"),
                 
                 leafletOutput("map1"), #displaying map
                 
                 #submit button
                 actionButton(inputId = "data1", label = "Submit"),
                 
                 
                 
               ), 
               
               # sidebarPanel
               
               mainPanel( # mainPanel used for outputting results
                 
                 h1("Under Construction!",style = "font-weight: 500; color: red"), #Under Construction sign
                 
                 h2("Results:"),
                 h4("lists possible plants ordered by success rate!"),
                 
                 verbatimTextOutput("plants"),
                 
                 
                 
               ) # mainPanel
              ) # sidebarLayout
      ), # end Page2
      
      # Page3, used for providing the "What to plant" service
      tabPanel( title = "When to plant!", value = "tab3",
               sidebarLayout(
                sidebarPanel(
                 tags$h3("When can I plant?"),
                
                 
                 # Select type of plant
                 selectInput(inputId = "plant", label = strong("Select Plant:"), choices = unique(plant_data$plant_name)),
                 
                 leafletOutput("map2"), #displaying map
                 # submit button
                 actionButton(inputId = "data2",label = "Submit"),
                 
                 
               ), # sidebarPanel
               
               mainPanel( # mainPanel used for outputting results
                 
                 h1("Under Construction!",style = "font-weight: 500; color: red"), #Under Construction sign
                 h2("Results:"),
                 h4("returns possibly planting and harvest times for your plant in addition to the required space"),
                 tableOutput("timesAndSpace"),
                 
                 
                 
                
               ) # mainPanel
              ) # sidebarLayout
      ) # end Page3
    ) # navbarPage
  ) # fluidPage

  
  # Server -------------------------------------------------------------------------------- 
  server <- function(input, output, session) {
    
    
    
    #only submit data when submit button is pressed
    data1 <- eventReactive(input$data1, { c(input$space,input$growthrange)}) 
    
    data2 <- eventReactive(input$data2, { subset(plant_data, plant_name == input$plant) }) 
    #
    
     
    
    # outputs of the two pages
    output$plants <- renderText( data1())
    
    output$timesAndSpace <- renderTable( data2(), rownames = TRUE)
    #
    
    #location api request
    latLongStatus <- reactive({c(input$lat, input$long, input$geolocation)})
    output$location <- renderText( latLongStatus())
    #
    
    
    
    #map section
    #middleware to avoid duplicate code
    mapdata <- reactive({
      leaflet() %>%
      addTiles(options = tileOptions(opacity = 0.8)) %>%
        addMarkers(lng= input$long, lat= input$lat, popup="Your Location") -> p
      p <- p %>%
        addDrawToolbar(
          polylineOptions = FALSE,
          polygonOptions = FALSE,
          rectangleOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = TRUE,
          circleMarkerOptions = FALSE,
          singleFeature = FALSE,
          editOptions = editToolbarOptions()
        )
    })
    
    
    #map on page "what to plant"
    output$map1 <- renderLeaflet({ mapdata() })
    #map on page "when to plant"
    output$map2 <- renderLeaflet({ mapdata() })
    #
    
    
    
    #handle redirect
    observeEvent(input$redirect1,
                 {
                   updateNavbarPage(session, "navbar",
                                    selected = "tab2")
                 })
    observeEvent(input$redirect2,
                 {
                   updateNavbarPage(session, "navbar",
                                    selected = "tab3")
                 })
    #
    
    
    
  }
    
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
