
library(ggcircos)
library(shiny)





ui <- shinyUI({
  
  fluidPage(
    sidebarPanel(
      selectInput("donor", label = "Select Donor:", choices = c("DO32875", "DO32819", "DO32835", "DO32837"), selected = "DO32875")  
    ),
    
    mainPanel(
      ggvisOutput("circos")
    )
  )
  
  
})

