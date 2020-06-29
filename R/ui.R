library(shinythemes)
library("shinyjs")

fluidPage(theme = shinytheme("superhero"),
  useShinyjs(),
  pageWithSidebar(
    # Application title
    titlePanel("Hotel Review System"),
    
    sidebarPanel(
      
      selectInput('city', 'Select City', c(Choose='', cities), selectize=TRUE),
      
      conditionalPanel(condition="input.city!=''",
                       uiOutput("choose_dataset")
                       ),
      
      checkboxInput("Sentiment_WC", label = "Sentiment WC", value = TRUE),
      checkboxInput("Bar_Graph", label = "Bar Graph", value = TRUE),
      checkboxInput("Sentiment_Analysis", label = "Sentiment Analysis", value = TRUE),
      checkboxInput("Word_Cloud", label = "Word Cloud", value = TRUE),
      hr(),
      helpText("Data from TripAdvisor")

    ),
    
    mainPanel(
      htmlOutput('ServiceCriticReview'),
      htmlOutput('CleanlinessCriticReview'),
      htmlOutput('RoomsCriticReview'),
      imageOutput("image2"),
      br(),
      br(),
      plotOutput("bar"),
      br(),
      plotOutput("sentiment"),
      br(),
      plotOutput("cloud"),
      br()
      
    )
  )
)
