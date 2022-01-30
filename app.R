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
suppressPackageStartupMessages(library(DT))


plant_data <- read.csv("src/20211214-plants-scraped.csv", header=TRUE) # Load test plant data



# UI -------------------------------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("flatly"),
              
                navbarPage(
                  
                  
                  
                  title = "Urban Garden App",
                  id = "navbar",
                  
                  # Page1, used as a welcome landing page with descriptions and redirecting features for the user 
                  tabPanel( title = "Home", value = "tab1",
                            mainPanel(
                              
                              h1("Welcome"),
                              h4("Welcome to our Urban Garden plant planner! As an urban gardener, it's not always easy to decide on which plants you'd like to have in your garden. 
                    Plant survival depends on many things, and the amount of information can be overwhelming at times.
                    That's why our website aims to assist enthousiastic gardeners by proposing some suggested plant types that are suitable for your garden.
                    Additionally, if you wonder what's the best time to plant something, you can consult the 'When to plant' page. 
                    Our suggestions are based on the climate type of your location, as well as the available space in your garden. Have fun!"),
                              
                              actionButton("redirect1", "What to plant"),
                              
                              actionButton("redirect2", "When to plant"),
                              h4("Click on one of the buttons above to get started planting. \n 
                    Please keep in mind that our suggestions only work if you water your plants properly."),
                              
                            )
                            
                  ), # end Page1
                  
                  # Page2, used for providing the "When to plant" service
                  tabPanel( title = "What to plant", value = "tab2",
                            sidebarLayout(
                              sidebarPanel(
                                tags$h3("What can I plant?"),
                                
                                tags$h5(tags$b("Location: ")),
                                leafletOutput("map1"), #displaying map
                                
                                numericInput("space1", "How much space is available? (in square centimeters)", 0, min = 0, step = 100),
                                # Select growth range
                                dateRangeInput("growthrange", "Select timeframe from planting to harvesting", start = NULL, end = NULL,
                                               min = "2021-01-01", max = "2030-01-01", startview =  "year", weekstart = "1"),
                                
                                
                                
                                #submit button
                                actionButton(inputId = "data1", label = "Submit"),
                                
                                
                                
                              ), 
                              
                              # sidebarPanel
                              
                              mainPanel( # mainPanel used for outputting results
                                
                                h2("About this page"),
                                h4("This page is designed to help (urban) gardeners find plants they can plant in their garden. 
                                   To recieve some suggestions please fill out the form according to your needs. 
                                   You have to provide the location of your garden, the reserved space (in square centimeters) 
                                   for your plant and also your desired planting and harvesting times."),
                                h3("Attention"),
                                h4("Please take note, that the service is currently only available european-wide. 
                                Also take in consideration that extreme input values probably won't result in many suggested plants. 
                                For the best results provide spaces around a few squaremeters and harvest-to-plant ranges of around 4-6 months."),
                                
                                tableOutput("plants"),
                                
                                
                                
                              ) # mainPanel
                            ) # sidebarLayout
                  ), # end Page2
                  
                  # Page3, used for providing the "What to plant" service
                  tabPanel( title = "When to plant", value = "tab3",
                            sidebarLayout(
                              sidebarPanel(
                                tags$h3("When can I plant?"),
                                
                                tags$h5(tags$b("Location: ")),
                                leafletOutput("map2"), #displaying map
                                
                                
                                # Select type of plant
                                selectInput(inputId = "plant", label = strong("Select Plant:"), choices = unique(plant_data$plant_name)),
                                # available space
                                numericInput("space2", "How much space is available? (in square centimeters)", 0, min = 0, step = 100),
                                
                                # submit button
                                actionButton(inputId = "data2",label = "Submit"),
                                
                                
                              ), # sidebarPanel
                              
                              mainPanel( # mainPanel used for outputting results
                                
                                h2("About this page"),
                                h4("This page is designed to help (urban) gardeners find planting times for a desired plant.
                                   To recieve some suggestions please fill out the form according to your needs. 
                                   You have to provide the location of your garden and the available space for the plant"),
                                h3("Attention"),
                                h4("Please take note, that the service is currently only available european-wide. 
                                Also take in consideration that extreme input values probably won't result in many suggested plants. 
                                The algorithm for example only suggests planting times if your provided space is sufficient enough for the plant to grow"),
                                
                                
                                tableOutput("timesAndSpace"),
                                tableOutput("datalink")
                                
                                
                                
                              ) # mainPanel
                            ) # sidebarLayout
                  ), # end Page3
                  
                  # Page4, used for contact page
                  tabPanel( title = "About", value = "tab4",
                            
                            mainPanel(
                              h1("Who are we?"),
                              p("This app was developed by five students from the University of Muenster, Germany.
                    The development was completed using RShiny. \n
                    If you have any questions, please don't hesitate to contact us:"),
                              a(actionButton(inputId = "email1", label = " merel.vogel@uni-muenster.de", 
                                             icon = icon("envelope", lib = "font-awesome")),
                                href="merel.vogel@uni-muenster.de"),
                              h1("Disclaimer"),
                              p("Please use the information on this website carefully, and make your own risk assessments when planting and harvesting.
                    We do not accept any liability or responsibility as a result of using the information we provide on this website.
                    The five of us can not be held responsible for any possible damage to gardens or other forms of consequences."),
                              h1("Data sources"),
                              p("The plant information we used in our analyses is retrieved from the following website:"),
                              a(actionButton(inputId = "website", label = " https://gardenplanner.almanac.com/", 
                                             icon = icon("fas fa-seedling", lib = "font-awesome")),
                                href="https://gardenplanner.almanac.com/plants/us-and-canada/"),
                            )
                            
                            
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
    
    #computation starts if marker is on the map and space is set
    if(!is.null(coordinates1$lat) && !is.null(coordinates1$long) && input$space1 > 0 ) { #add check if timeframe isset
      
      print("Submit successful!")
      
      #get climate from user based on coordinates
      climate <- climate("src/climate.tif", coordinates1$long, coordinates1$lat)
      #what_to_plant
      return(what_to_plant(climate, input$growthrange[1], input$growthrange[2], input$space1, plot= FALSE))
      
    }
    #if no marker is on the map coordinates are NULL and computation does not start
    else if(is.null(coordinates1$lat) || is.null(coordinates1$long)) {
      
      print("Please provide a location!")
      return("Please provide a location!")
    }
    #no space greater than 0 is provided
    else if(input$space1 <= 0) {
      
      print("Please provide your available space")
      return("Please provide your available space")
    }
    
    
  }) 
  
  data2 <- eventReactive(input$data2, {
    
    #computation starts if marker is on the map and a plant and space are set
    if(!is.null(coordinates2$lat) && !is.null(coordinates2$long) && (input$space2 > 0) && !is.null(input$plant)) {
      
      print("Submit successful!")
      
      #get climate from user based on coordinates
      climate <- climate("src/climate.tif", coordinates2$long, coordinates2$lat)
      #when_to_plant
      return(when_to_plant(plant= input$plant, space=input$space2, clim=climate))
      
    }
    #if no marker is on the map coordinates are NULL and computation does not start
    else if(is.null(coordinates2$lat) || is.null(coordinates2$long)) {
      
      print("Please provide a location!")
      return("Please provide a location!")
    }
    #no space greater than 0 is provided
    else if(input$space2 <= 0) {
      
      print("Please provide your available space")
      return("Please provide your available space")
    }
    
  }) 
  
   selected_plant <- eventReactive(input$plant, {
    plantname <- as.character(input$plant)
    #plantname <- tolower(plantname)
    print(plantname)
    links <- vector()
    for(i in 1:nrow(plant_data)){
      if(plant_data$plant_name[i] == plantname){
        links <- append(links, plant_data$links[i])
      }
    }
    #print(links)
    link <- links[1]
    print(link)
    #link <- paste("https://gardenplanner.almanac.com/plants/us-and-canada/how-to-grow-", plantname, '/', sep = "", collapse = NULL)
    url <- a("Find more information here", href=link)
    url <- HTML(paste(url))
    return (url)
  })
  #
  
  
  
  # outputs of the two pages
  
  
  # page what to plant
  output$plants <- renderUI({
    DT::renderDataTable({datatable(data1(), options= list(paging= TRUE, searching= TRUE), colnames=c("recommended plants", "links for more information"), escape = FALSE)})
  })
  
  
  # page when to plant
  #output$timesAndSpace <-renderUI({
   # renderTable(data2(), bordered = TRUE, width = '60%',  colnames=TRUE, 
    #            caption = "<b> <span style='color:#000000; font-size: 24px;'> Results: </b>",
     #           caption.placement = getOption("xtable.caption.placement", "top"),
      #          caption.width = getOption("xtable.caption.width", NULL))
    
  #})
  
  output$timesAndSpace <-renderUI({
    DT::renderDataTable({datatable(data2(), options= list(paging= FALSE, searching= FALSE), colnames=c("You can plant during the following months", "Days until your plant is fully grown"))})
  })
  
  output$datalink <- renderText(selected_plant())
  
  #
  
  
  #map section
  
  toolbar1 <- TRUE
  toolbar2 <- TRUE
  
  
  #render map1 for what to plant
  init1 <- function() renderLeaflet({
    leaf <- leaflet() %>%
      addTiles(options = tileOptions(opacity = 0.8)) %>%
      setView(7.633763,51.97587, zoom = 4) %>%
        addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ 
                      map.locate({setView: true})
                      .on('locationfound', function(e){
                          Shiny.setInputValue('lat1', e.latitude); //send latlong coordinates to Shiny
                          Shiny.setInputValue('long1', e.longitude);
                          Shiny.setInputValue('send1', Date.now()) //create unique value so that ObserveEvent gets triggered
                      }) 
                    }")
      ))
      if(toolbar1)
        leaf <- leaf %>%
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
      leaf
  })
  
  #render map2 for when to plant
  init2 <- function() renderLeaflet({
    leaf <- leaflet() %>%
      addTiles(options = tileOptions(opacity = 0.8)) %>%
      setView(7.633763,51.97587, zoom = 4) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ 
                      map.locate({setView: true})
                      .on('locationfound', function(e){  
                          Shiny.setInputValue('lat2', e.latitude); //send latlong coordinates to Shiny
                          Shiny.setInputValue('long2', e.longitude);
                          Shiny.setInputValue('send2', Date.now()) //create unique value so that ObserveEvent gets triggered
                      })
                    }")
      )) 
      if(toolbar2)
        leaf <- leaf %>%
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
      leaf
  })
  

  #observers for marker actions on map1
  #observe LocateMe button press. Remove existing marker from map, adds the new one and saves coordinates
  observeEvent(input$send1, {
    lat <- input$lat1
    long <- input$long1
    
    #workaround to delete existing marker
    toolbar1 <<- !toolbar1
    output$map1 <- init1()
    clearMarkers(leafletProxy('map1'))
    toolbar1 <<- !toolbar1
    output$map1 <- init1()
    
    leafletProxy("map1") %>% addMarkers(long,lat, label = paste0("I am here: ",long,",",lat,sep = '')) %>% setView(long,lat,zoom = 4)
    print("Marker at Geolocation set")
    coordinates1$long <- long
    coordinates1$lat <- lat
    
  })
  #observe new marker and save coordinates. Also deletes existing marker
  observeEvent(input$map1_draw_new_feature, {
    clearMarkers(leafletProxy("map1"))
    draw1  <- input$map1_draw_new_feature
    coordinates1$long <- draw1$geometry$coordinates[[1]]
    coordinates1$lat <- draw1$geometry$coordinates[[2]]
  })
  
  #observe marker edit and update coordinates
  observeEvent(input$map1_draw_edited_features, {
    draw1  <- input$map1_draw_edited_features
    coordinates1$long <- draw1$features[[1]]$geometry$coordinates[[1]]
    coordinates1$lat <- draw1$features[[1]]$geometry$coordinates[[2]]
  })
  #observe marker delete and set coordinates to null
  observeEvent(input$map1_draw_deleted_features, {
    coordinates1$long <- NULL
    coordinates1$lat <- NULL
  })
  
  

  #observers for marker actions on map2
  #observe LocateMe button press. Remove existing marker from map, adds the new one and saves coordinates
  observeEvent(input$send2, {
    lat <- input$lat2
    long <- input$long2
    
    #workaround to delete existing marker
    toolbar2 <<- !toolbar2
    output$map2 <- init2()
    clearMarkers(leafletProxy('map2'))
    toolbar2 <<- !toolbar2
    output$map2 <- init2()
      
    leafletProxy("map2") %>% addMarkers(long,lat, label = paste0("I am here: ",long,",",lat,sep = '')) %>% setView(long,lat,zoom = 4)
    print("Marker at Geolocation set")
    coordinates2$long <- long
    coordinates2$lat <- lat  
  })
  #observe new marker and save coordinates. Also deletes existing marker
  observeEvent(input$map2_draw_new_feature, {
    clearMarkers(leafletProxy("map2"))
    draw2  <- input$map2_draw_new_feature
    coordinates2$long <- draw2$geometry$coordinates[[1]]
    coordinates2$lat <- draw2$geometry$coordinates[[2]]
  })
  
  #observe marker edit and update coordinates
  observeEvent(input$map2_draw_edited_features, {
    draw2  <- input$map2_draw_edited_features
    coordinates2$long <- draw2$features[[1]]$geometry$coordinates[[1]]
    coordinates2$lat <- draw2$features[[1]]$geometry$coordinates[[2]]
  })
  #observe marker delete and set coordinates to null
  observeEvent(input$map2_draw_deleted_features, {
    coordinates2$long <- NULL
    coordinates2$lat <- NULL
  })
  
  output$map1 <- init1()
  output$map2 <- init2()
  
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
    print("The user climate is: ")
    print(user_clim)
    return (user_clim)
    
  }
  
  ########### FILTER WHEN TO PLANT ###########
  
  number_of_rows_in_dataset <- nrow(plant_data)
  range <- 1:number_of_rows_in_dataset
  Months<-c("January","February","March","April","May","June","July","August","September","October","November","December")
  
  when_to_plant <- function(plant, space, clim){
    
    print("in when_to_plant")
    output_array <- vector()
    output_array_days <- vector()
    for(n in range){
      if(plant == plant_data$plant_name[n]){
        if(plant_data$required_space[n]>space){
          return("You do not have enough space")
        }
        if(plant_data$required_space[n]<=space && substr(plant_data$climate[n], 1, 2) == substr(clim, 1, 2)) {
          when_to_plant <- plant_data$when_to_plant[n]
          when_to_plant <- unlist(strsplit(when_to_plant, ","))
          when_to_plant <- as.numeric(when_to_plant)
          min_days <- plant_data$min_growing_time[n]
          max_days <- plant_data$max_growing_time[n]
          days <- paste(min_days, "-", max_days)
          output_array_days <- append(output_array_days, days)
          for(i in when_to_plant){
            x <- paste(Months[i]) 
            output_array <- append(output_array, x) 
            cat("You can plant in", Months[i], "\n\n")
            
          }
        }
      }
    }
    print(output_array_days)
    output_array<- unique(output_array)
    print(output_array)
    output_df <- data.frame(output_array, output_array_days[1])
    return(output_df)
  }
  
}

######### WHAT TO PLANT SURVIVAL ANALYSIS ##########

what_to_plant <- function(climate, date_to_plant, date_to_harvest, size, plot = FALSE ) {
  
  print("in what_to_plant")
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
    
    r = "Sorry, no recommendations for you. \n Please try plant and harvest dates that are either closer or further away from each other."
    
  }else{
    
    if ( nrow(prd) >= 10){
      
      prd <- prd[which(prd$surv_p > 99),]
    }
    
    prd <- prd[order(-prd$surv_p), ]
    
    r = c()
    p = vector()
    l = vector()
    for (i in 1:nrow(prd)){
      r[i] = paste(
        prd[i, 1], ":",
        "Visit ", prd[i, 2], "for more information", 
        "\n\n"
      )
      p = append(p, prd[i,1])
      url = a("Find more information here", href=prd[i,2])
      l = append(l, HTML(paste(url)))
    }
  }
  df<-data.frame(p,l)
  return(df)
  #return(r)
}

# Create Shiny object
shinyApp(ui = ui, server = server)
