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
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(htmlwidgets))
suppressPackageStartupMessages(library(rgdal))



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
                 
                 numericInput("space1", "How much space is available:", 0, min = 0),
                 # Select growth range
                 dateRangeInput("growthrange", "Select timeframe from planting to harvesting", start = NULL, end = NULL,
                                min = "2021-01-01", max = "2030-01-01", startview =  "year", weekstart = "1"),
                 
                 
                 
                 #submit button
                 actionButton(inputId = "data1", label = "Submit"),
                 
                 
                 
               ), 
               
               # sidebarPanel
               
               mainPanel( # mainPanel used for outputting results
                 
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
                  # available space
                  numericInput("space2", "How much space is available:", 0, min = 0),
                  
                  # submit button
                  actionButton(inputId = "data2",label = "Submit"),
                 
                 
               ), # sidebarPanel
               
               mainPanel( # mainPanel used for outputting results
                 
                  h2("Results:"),
                  h4("returns possibly planting and harvest times for your plant in addition to the required space"),
                  
                  tableOutput("timesAndSpace"),
                 
                 
                 
                
               ) # mainPanel
              ) # sidebarLayout
      ), # end Page3
      
      # Page4, used for an impressum
      tabPanel( title = "Impressum", value = "tab4"
               #TODO: Impressum @Merel
                 
                
      )
    ) # navbarPage
  ) # fluidPage

  
  # Server -------------------------------------------------------------------------------- 
  server <- function(input, output, session) {
    
    #coordinates of the users location on whattoplant(map1) and whentoplant(map2)
    coordinates1 <- reactiveValues(lat = NULL, long = NULL)
    coordinates2 <- reactiveValues(lat = NULL, long = NULL)
    
    #only submit and compute data when submit button is pressed
    
    data1 <- eventReactive(input$data1, {
    
      
      if(!is.null(coordinates1$lat) && !is.null(coordinates1$long) && input$space1 > 0 ) { #add check if timeframe isset
        
        print("submit succesfull!")
        
        climate <- climate("src/climate.tif", coordinates1$lat, coordinates1$long)
        return(what_to_plant(climate, input$growthrange[1], input$growthrange[2], input$space1, plot= FALSE))
        
      }
      #if no marker is on the map coordinates are NULL and computation does not start
      else if(is.null(coordinates1$lat) || is.null(coordinates1$long)) {
        
        print("Pls provide a location!")
        return("Pls provide a location!")
      }
      #no space greater than 0 is provided
      else if(input$space1 <= 0) {
        
        print("pls provide your available space")
        return("Pls provide your available space")
      }
      
    
    }) 
    
    data2 <- eventReactive(input$data2, {
      
      #if marker is on the map coordinates2 are not NULL and computation starts
      if(!is.null(coordinates2$lat) && !is.null(coordinates2$long) && (input$space2 > 0) && !is.null(input$plant)) {
        
        print("submit succesfull!")
        
        #get climate from user based on coordinates
        climate <- climate("src/climate.tif", coordinates2$lat, coordinates2$long)
      
        
        #filter_wtp
        return(filter_wtp(plant= input$plant, space=input$space2, clim=climate))
        
      }
      #if no marker is on the map coordinates are NULL and computation does not start
      else if(is.null(coordinates2$lat) || is.null(coordinates2$long)) {
        
        print("Pls provide a location!")
        return("Pls provide a location!")
      }
      #no space greater than 0 is provided
      else if(input$space2 <= 0) {
        
        print("pls provide your available space")
        return("Pls provide your available space")
      }
      
    }) 
    #
    
     
    
    # outputs of the two pages
    
    
    # page what to plant
    output$plants <- renderText( data1())
    
    
    # page when to plant
    output$timesAndSpace <- renderTable( data2(), rownames = FALSE)
    
    
    #
    
    
    
    #location api request
    latLongStatus <- reactive({c(input$lat, input$long, input$geolocation)})
    #geolocation <- reactiveValues(lat = input$lat, long = input$long, status = input$geolocation)
    output$location <- renderText( latLongStatus())
    #
    
    
    
    #map section
    #middleware to avoid duplicate code
    mapdata <- reactive({
      leaflet() %>%
      addTiles(options = tileOptions(opacity = 0.8)) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ 
                      map.locate({setView: true})
                      .on('locationfound', function(e){
                          var marker = map.addmarker([e.latitude, e.longitude]).bindPopup('Your are here :)');
                      })
                      
                      
                      
                    }")
      )) %>%
      
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
      coordinates1$lat <- draw1$geometry$coordinates[[1]]
      coordinates1$long <- draw1$geometry$coordinates[[2]]
      print(coordinates1$lat)
      print(coordinates1$long)
    })
    
    #observe marker edit and update coordinates
    observeEvent(input$map1_draw_edited_features, {
      draw1  <- input$map1_draw_edited_features
      coordinates1$lat <- draw1$features[[1]]$geometry$coordinates[[1]]
      coordinates1$long <- draw1$features[[1]]$geometry$coordinates[[2]]
      print(coordinates1$lat)
      print(coordinates1$long)
    })
    #observe marker delete and set coordinates to null
    observeEvent(input$map1_draw_deleted_features, {
      coordinates1$lat <- NULL
      coordinates1$long <- NULL
      print(coordinates1$lat)
      print(coordinates1$long)
    })
    
    
    #observers for marker actions on map2
    #observe new marker and save coordinates
    observeEvent(input$map2_draw_new_feature, {
      draw2  <- input$map2_draw_new_feature
      coordinates2$lat <- draw2$geometry$coordinates[[1]]
      coordinates2$long <- draw2$geometry$coordinates[[2]]
      print(coordinates2$lat)
      print(coordinates2$long)
    })
    
    #observe marker edit and update coordinates
    observeEvent(input$map2_draw_edited_features, {
      draw2  <- input$map2_draw_edited_features
      coordinates2$lat <- draw2$features[[1]]$geometry$coordinates[[1]]
      coordinates2$long <- draw2$features[[1]]$geometry$coordinates[[2]]
      print(coordinates2$lat)
      print(coordinates2$long)
    })
    #observe marker delete and set coordinates to null
    observeEvent(input$map2_draw_deleted_features, {
      coordinates2$lat <- NULL
      coordinates2$long <- NULL
      print(coordinates2$lat)
      print(coordinates2$long)
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
    
    ########### FILTER WHEN TO PLANT ###########
    
    number_of_rows_in_dataset <- nrow(plant_data)
    range <- 1:number_of_rows_in_dataset
    Months<-c("January","February","March","April","May","June","July","August","September","October","November","December")
    
    filter_wtp <- function(plant, space, clim){
      
      print("in filter_wtp()")
      output_array <- vector()
      for(n in range){
        if(plant == plant_data$plant_name[n]){
          if(plant_data$required_space[n]>space){
            return("You do not have enough space")
          }
          if(plant_data$required_space[n]<=space && substr(plant_data$climate[n], 1, 2) == substr(clim, 1, 2)) {
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
      
      print(output_array)
      return(output_array)
    }
    
  }
  
  ######### WHAT TO PLANT SURVIVAL ANALYSIS ##########

  what_to_plant <- function(climate, date_to_plant, date_to_harvest, size, plot = FALSE ) {
  
    # read jointly created vegetables table
    plants <- read.table("src/20211214-plants-scraped.csv", sep = ',', header = T)
  
    #excluding non-usable data
    plants <- plants[,3:10]
  
    colnames(plants) <- c('name', 'links', 'space', 'when_to_plant', 'min_grow_t', 'max_grow_t', 'when_to_harvest', 'climate')
  
    # mean survival time (discussion required)
    plants$time <- as.numeric(( plants$max_grow_t + plants$min_grow_t ) / 2)
  
    plants_mut <- mutate(plants, space = ifelse((space < 1760), "LT1760", "OV1760"), #1760 is mean space
                       space = factor(space))
  
    # Random Forest Ensemble Model for Prob. of Survival
    r_fit <- ranger(Surv(time) ~ space + min_grow_t + max_grow_t + climate,
                  data = plants_mut,
                  mtry = 2,
                  importance = "permutation",
                  splitrule = "extratrees",
                  verbose = TRUE)
  
    death_times <- r_fit$unique.death.times 
    surv_prob <- data.frame(r_fit$survival)
    avg_prob <- sapply(surv_prob,mean)
  
    if (plot == TRUE){
    
      plot(r_fit$unique.death.times,r_fit$survival[1,], 
         type = "l", 
         ylim = c(0,1),
         col = "red",
         xlab = "Days",
         ylab = "survival",
         main = "Patient Survival Curves")
    
      cols <- colors()
      for (n in sample(c(2:dim(plants_mut)[1]), 20)){
      
        lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
      }
    
      lines(death_times, avg_prob, lwd = 2)
      legend(500, 0.7, legend = c('Average = black'))
    
      vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
      names(vi) <- "importance"
      head(vi)
    
    }
  
    # Area under ROC
    #  pe <- cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)
  
    #Filtering
  
    if (grepl( ".", date_to_plant, fixed = TRUE) == TRUE | grepl( ".", date_to_harvest, fixed = TRUE) == TRUE) {
    
      wtp <- as.Date(date_to_plant, '%d.%m.%Y')
      wth <- as.Date(date_to_harvest, '%d.%m.%Y')
    
    }else if (grepl( "/", date_to_plant, fixed = TRUE) == TRUE | grepl( "/", date_to_harvest, fixed = TRUE) == TRUE) {
    
      wtp <- as.Date(date_to_plant, '%d/%m/%Y')
      wth <- as.Date(date_to_harvest, '%d/%m/%Y')
    
    }else{
    
      wtp <- as.Date(date_to_plant, '%d-%m-%Y')
      wth <- as.Date(date_to_harvest, '%d-%m-%Y')
    
    }
  
    gt = as.integer(wth - wtp)
  
    dt <- c()
    for (i in 1:length(death_times)){
    
      dt[i] <- abs(death_times[i] - gt)
    
    }
  
    min_dt <- min(dt)
    for (i in 1:length(death_times)){
    
      if (dt[i] == min(dt)){
      
        item = i
      
      }
    }
  
    wtp = as.integer(format(wtp, "%m"))
    wth = as.integer(format(wth, "%m"))    
  
    #final filtering
    f <- surv_prob[,item]
  
    if (length(f) > 1700){
    
    
      ls <- c()
      for ( i in 1:length(f)){
      
        if (f[i] > 0.8){
        
          ls <- c(ls, i)
        
        }
      }
    }else {
    
      ls <- c()
      for ( i in 1:length(f)){
      
        if (f[i] > 0.7){
        
          ls <- c(ls, i)
        
        }
      }
    }
  
    nls <- c()
    for ( i in ls){
    
      if (grepl(wtp, plants[i, 4]) == 1 & grepl(wth, plants[i, 7]) == 1) {
      
        nls <- c(nls, i)
      
      }
    }
  
    prd <- plants[nls,]
    prd$surv_p <- round(surv_prob[nls, item], 2)*100
    prd <- prd[which(prd$min_grow_t <= gt + 15 & prd$max_grow_t >= gt - 15), ]
    prd <- prd[which(prd$space <= size + 50 ),] 
    prd <- prd[which(substr(prd$climate, 1, 2) == substr(climate, 1, 2)),]
  
    if ( nrow(prd) == 0){
    
      r = "Sorry, no recommendations for you. \n Please try dates with different gap."
    
    }else{
      
      if ( nrow(prd) >= 10){
        
        prd <- prd[which(prd$surv_p > 99),]
      }
      
      prd <- prd[order(-prd$surv_p), ]
    
      r = c()
      for (i in 1:nrow(prd)){
        r[i] = paste(
          prd[i, 1], ":",
          "Visit ", prd[i, 2], "for more information", 
          "\n\n"
        ) 
      }
    }
  return(r)
  }
    
  # Create Shiny object
  shinyApp(ui = ui, server = server)
