library(ggvis)
library(shiny)


ui <- shinyUI({
  
  fluidPage(
    sidebarPanel(
      selectInput("year", label = "Select Year:", choices = c(1990, 1995, 2000, 2005), selected = 1990)  
    ),
  
    mainPanel(
      ggvisOutput("population_flows")
    )
  )
  
})


