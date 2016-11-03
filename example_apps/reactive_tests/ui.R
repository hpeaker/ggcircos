library(ggcircos)
library(shiny)
library(shinydashboard)




header <- dashboardHeader(title = "Circos")


sidebar <- dashboardSidebar(
  sliderInput("scale_factor", "Scaling size:", min = 2, max = 20, value = 3),
  sliderInput("rotate", "Rotation:", min = 0, max = 359, value = 0),
  selectInput("points", "Select data:", choices = c(1, 2), selected = 1)
)



body <- dashboardBody(ggvisOutput("plot"))


ui <- shinyUI(
  dashboardPage(header, sidebar, body)
)
