library(shiny)

# Define UI climate app
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Simplistic climate prediction model for your neighborhood sceptic"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("years",
                   "Select year range:",
                   min = 1880,
                   max = 2016,
                   value = c(1880,2016),
                   step = 1,
                   sep = ""),
       checkboxGroupInput("predictor", "Select predictors:",
                          c("CO2" = "co2",
                            "Sun spot cycle" = "sunspots",
                            "Volcanic eruptions" = "eruptions"),
                          selected = c(FALSE, FALSE, FALSE))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("fitPlot"),
       h3("Documentation:"),
       helpText("First, select a time window of interest (in years) with slider.",
                "Second, select temperature predictors with checkboxes.",
                "The graph will update according to selections made.")
    )
  )
))
