library(shiny)
shinyUI(fluidPage(
     titlePanel("Air quality measurements"),
     sidebarLayout(
          sidebarPanel(
               sliderInput("sliderNoise", "Amount of noise to add?", 0, 50, value = 0),
               checkboxInput("showModel1", "Linear Model", value = FALSE),
               checkboxInput("showModel2", "Quadratic Model", value = FALSE),
 
               
               
               
          ),
          mainPanel(
               plotOutput("plot1"),
               
               h4("Linear Model:"),
               textOutput("pred1"),
               tableOutput('tbl1'),               
               h4("Quadratic Model:"),
               textOutput("pred2"),
               tableOutput('tbl2'),

          )
     )
))
