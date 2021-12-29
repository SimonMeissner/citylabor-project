# https://shiny.rstudio.com/articles/reactivity-overview.html


# install R packages



# load R packages
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))
suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(ranger))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(raster))



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
                 
                 tags$h5(tags$b("Location: ")),
                 leafletOutput("map1"), #displaying map
                 
                 textInput("space", "How much space is available:", ""),
                 # Select growth range
                 dateRangeInput("growthrange", "Select timeframe from planting to harvesting", start = NULL, end = NULL,
                                min = "2021-01-01", max = "2030-01-01", startview =  "year", weekstart = "1"),
                 
                 
                 
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
                 
                  tags$h5(tags$b("Location: ")),
                  leafletOutput("map2"), #displaying map
                
                 
                  # Select type of plant
                  selectInput(inputId = "plant", label = strong("Select Plant:"), choices = unique(plant_data$plant_name)),
                 
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
    
    coordinates <- reactiveValues(lat = NULL, long = NULL)
    
    #only submit and compute data when submit button is pressed
    
    data1 <- eventReactive(input$data1, { c(input$space,input$growthrange)}) 
    
    data2 <- eventReactive(input$data2, {
      
      #get climate from user based on coordinates
      climate <- climate("src/climate.tif", coordinates$lat, coordinates$long)
      
      #filter_wtp
      filter_wtp(plant= input$plant, space=9000, clim=climate) 
      
    }) 
    #
    
     
    
    # outputs of the two pages
    
    
    # page what to plant
    output$plants <- renderText( data1())
    
    
    
    # page when to plant
    
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
      
        setView(7.633763,51.97587, zoom = 4) %>%
        addDrawToolbar(
          polylineOptions = FALSE,
          polygonOptions = FALSE,
          rectangleOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = TRUE,
          circleMarkerOptions = FALSE,
          singleFeature = TRUE,
          editOptions = editToolbarOptions()
        )
    })
    #observers for marker actions on map1
    #observe new marker and save coordinates
    observeEvent(input$map1_draw_new_feature, {
      draw1  <- input$map1_draw_new_feature
      coordinates$lat <- draw1$geometry$coordinates[[1]]
      coordinates$long <- draw1$geometry$coordinates[[2]]
      print(coordinates$lat)
      print(coordinates$long)
    })
    
    #observe marker edit and update coordinates
    observeEvent(input$map1_draw_edited_features, {
      draw1  <- input$map1_draw_edited_features
      coordinates$lat <- draw1$features[[1]]$geometry$coordinates[[1]]
      coordinates$long <- draw1$features[[1]]$geometry$coordinates[[2]]
      print(coordinates$lat)
      print(coordinates$long)
    })
    
    
    #observers for marker actions on map2
    #observe new marker and save coordinates
    observeEvent(input$map2_draw_new_feature, {
      draw2  <- input$map2_draw_new_feature
      coordinates$lat <- draw2$geometry$coordinates[[1]]
      coordinates$long <- draw2$geometry$coordinates[[2]]
      print(coordinates$lat)
      print(coordinates$long)
    })
    
    #ovserve marker edit and update coordinates
    observeEvent(input$map2_draw_edited_features, {
      draw2  <- input$map2_draw_edited_features
      coordinates$lat <- draw2$features[[1]]$geometry$coordinates[[1]]
      coordinates$long <- draw2$features[[1]]$geometry$coordinates[[2]]
      print(coordinates$lat)
      print(coordinates$long)
    })
    
    
    
    #map on page "what to plant"
    output$map1 <- renderLeaflet({ mapdata() })
    #map on page "when to plant"
    output$map2 <- renderLeaflet({ mapdata() })
    #
    
    
    #map section end
    
    
    
    #handle redirect
    observeEvent(input$redirect1,{
      updateNavbarPage(session,"navbar",selected = "tab2") #redirect to what to plant
    })
    observeEvent(input$redirect2, {
      updateNavbarPage(session,"navbar",selected = "tab3") #redirect to when to plant
    })
    #
    
    
    
    
    
    # get climate from user depending on location 
    climate <- function(rst, x, y) {
      
      xy <- data.frame(x = c(x), y = c(y))
      kg <- raster(rst)
      n_clim <- extract(kg, xy)
      
      classif <- c('Af', 'Am', 'Aw', 'BWh', 'BWk', 'BSh', 'BSk', 'Csa', 'Csb', 'Cwa', 'Cwb', 'Cwc',
                   'Cfa', 'Cfb', 'Cfc', 'Dsa', 'Dsb', 'Dsc', 'Dsd', 'Dwa', 'Dwb', 'Dwc', 'Dwd', 'Dfa',
                   'Dfb', 'Dfc', 'Dfd', 'ET', 'EF')
      
      user_clim <- classif[n_clim]
      print("in climate()")
      print(user_clim)
      return (user_clim)
      
    }
    
    ########### FILTER WHEN TO PLANT ##########
    
    number_of_rows_in_dataset <- nrow(plant_data)
    range <- 1:number_of_rows_in_dataset
    Months<-c("January","February","March","April","May","June","July","August","September","October","November","December")
    
    filter_wtp <- function(plant, space, clim){
      output_array <- vector()
      for(n in range){
        if(plant == plant_data$plant_name[n]){
          if(plant_data$required_space[n]>space){
            return("You do not have enough space")
          }
          if(plant_data$required_space[n]<=space && plant_data$climate[n] == clim) {
            when_to_plant <- plant_data$when_to_plant[n]
            when_to_plant <- unlist(strsplit(when_to_plant, ","))
            when_to_plant <- as.numeric(when_to_plant)
            for(i in when_to_plant){
              x <- c("You can plant in ", Months[i]) 
              output_array <- append(output_array, x) 
              cat("You can plant in", Months[i], "\n\n")
              
            }
          }
        }
      }
      print("in filter_wtp()")
      print(output_array)
      return(output_array)
    }
    
  }
    
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
