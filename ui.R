library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
    
  # Application title
  titlePanel("Tracking the Ebola Outbreak 2014"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
#  sidebarLayout(
 #   sidebarPanel(
  #    radioButtons("dist", "Distribution type:",
   #                c("Normal" = "norm",
    #                 "Uniform" = "unif",
     ##                "Log-normal" = "lnorm",
      #               "Exponential" = "exp")),
      #br(),
      
     # sliderInput("n", 
     #             "Number of observations:", 
     #              value = 500,
     #              min = 1, 
     #              max = 1000)
    #),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Plot", plotOutput("basic1"), hr(), plotOutput("facet")),
        tabPanel("Data",  downloadButton('downloadData', 'Download'), hr(), verbatimTextOutput("summary"),dataTableOutput("table")  ),
        tabPanel("Model", h3("Exponential Growth"), verbatimTextOutput("regression"),hr(),
                 h3("Linear Growth"), verbatimTextOutput("linear"), hr(), h3("Saturation Model"), h4("Model does not fit."), verbatimTextOutput("saturation")),
        
        tabPanel("Prediction", 
                 plotOutput("prediction")
                 
                 
                 ),
      #  tabPanel("Map", htmlOutput("map")),
        tabPanel("About", tags$div(class="well", HTML("Data is Scraped from http://en.wikipedia.org/wiki/Ebola_virus_epidemic_in_West_Africa 
      <br> inspired by  http://www.econometricsbysimulation.com/2014/09/12-millions-deaths-by-ebola-projected.html"
      )))
      
      )
    )
#  )
))