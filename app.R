# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)

plant_data <- read.csv("test/test_data.csv", header=TRUE)


  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
      # theme = "cerulean",  #To use a theme, uncomment this
      "Urban Garden App",
      
      # Page1, used as a welcome landing page with descriptions and redirecting features for the user 
      tabPanel("Welcome",
               mainPanel(
                 
                  h1("Under Construction!",style = "font-weight: 500; color: red"), #Under Construction sign
                  h1("Welcome"),
                  p("This is the landing page of our webapp build in R-Shiny!
                    It could be used to describe our app and suggests the user
                    to use one of the two options."),
                  
                  radioButtons("redirect","Choose one of the Options:",
                               choices = list("What to plant" = 1, "When to plant" = 2)),
                  p("By clicking one of the options the user gets redirected to the different pages")
               )
               
      ), # end Page1
      
      # Page2, used for providing the "When to plant" service
      tabPanel("What to plant!",
               sidebarPanel(
                 tags$h3("What can i plant?"),
                 textInput("space", "How much space is available:", ""),
                 # Select growth range
                 dateRangeInput("growthrange", strong("Select timeframe from planting to harvesting"), start = NULL, end = NULL,
                                min = "2021-01-01", max = "2030-01-01", startview =  "year", weekstart = "1"),
                 # dateInput("timep", "When to plant:", ""),
                 # dateInput("timeh", "When to harvest:", ""),
                 submitButton(text = "Submit")
                 
               ), # sidebarPanel
               
               mainPanel( # mainPanel used for outputting results
                 
                 h1("Under Construction!",style = "font-weight: 500; color: red"), #Under Construction sign
                 
                 h2("Results:"),
                 h4("lists possible plants ordered by success rate!"),
                 
                 dataTableOutput("plants"),
                 
               ) # mainPanel
               
      ), # end Page2
      
      # Page3, used for providing the "What to plant" service
      tabPanel("When to plant!",
               sidebarPanel(
                 tags$h3("When can i plant?"),
                 
                 # Select type of plant
                 selectInput(inputId = "plant", label = strong("Select Plant:"),
                             choices = unique(plant_data$plant_name),
                  ),
                 submitButton(text = "Submit")
                 
               ), # sidebarPanel
               
               mainPanel( # mainPanel used for outputting results
                 
                 h1("Under Construction!",style = "font-weight: 500; color: red"), #Under Construction sign
                 h2("Results:"),
                 h4("returns possibly planting and harvest times for your plant in addition to the required space"),
                 verbatimTextOutput("timesAndSpace"),
                 
               ) # mainPanel
               
      ) # end Page3
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    output$plants <- renderDataTable( plant_data,
      options = list(
        pageLength = 5
      )
      
    )
    
      
  }
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
