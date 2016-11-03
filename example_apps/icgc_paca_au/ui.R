
library(shinydashboard)
library(ggvis)


use_donors <- c("DO32875", "DO32878", "DO32900", "DO33091", "DO33256", "DO33336", "DO33344", "DO33368",
                "DO33376", "DO33392", "DO33400", "DO33408", "DO33480", "DO33512", "DO33528", "DO33544",
                "DO33552", "DO33600", "DO33632", "DO33656", "DO33984", "DO34240", "DO34264", "DO34288",
                "DO34312", "DO34368", "DO34376", "DO34432", "DO34448", "DO34600", "DO34608", "DO34656",
                "DO34696", "DO34736", "DO34785", "DO34793", "DO34801", "DO34809", "DO34817", "DO34849",
                "DO34905", "DO34961")




header <- dashboardHeader()

sidebar <- dashboardSidebar(
  
  selectInput("donor", "Select Donor:", choices = use_donors, selected = "DO32875"),
  sliderInput("scale", label = "Scaling Factor: ", min = 2, max = 10, value = 3, step = 1)
  
)

body <- dashboardBody(

  ggvisOutput("plot")
  
)



ui <- shinyUI(
  dashboardPage(header, sidebar, body)
)



