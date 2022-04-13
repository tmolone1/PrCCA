#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

well_fxn <- function(u, nterms) {
  i <- seq(2,nterms, 2)
  p <- seq(3,nterms, 2)
  ui<- -1*(u^i)/(i*factorial(i))
  up<- (u^p)/(p*factorial(p))       
  wu <- -0.57721566 - log(u) + u + sum(ui) + sum(up)
  return(wu)
}

list_well_fxn <- function (listu, nterms) {
  wu<-vector(mode="numeric",length=length(listu))
  j <-1
  for (i in listu) {
    wu[j]<-well_fxn(i,nterms)
    j=j+1
  }
  return(wu)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Drawdown Estimation Assistant"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("b",
                   "estimated saturated thickness (ft):",
                   min = 1,
                   max = 1000,
                   value = 280),
      numericInput("k",
                   "estimated hydraulic conductivity (ft/d):",
                   min = 0,
                   max = 100000,
                   value = 4.775),
      sliderInput("t",
                   "time since pumping began (days):",
                   min = 0,
                   max = 365,
                   value = 40),
      sliderInput("S",
                  "Aquifer Specific Yield (dimensionless):",
                  min = 0,
                  max = 1,
                  value = 0.15),
      sliderInput("Q",
                  "Pumping Rate (gpm):",
                  min = 0,
                  max = 5000,
                  value = 900),
      numericInput("r",
                  "User specified distance of interest (ft):",
                  min = 0,
                  max = 20000,
                  value = 500)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      textOutput("text"),
      textOutput("dd")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    S<-input$S
    t<-input$t
    r<-seq(200,10000,200)
    T<-input$k*input$b
    u<-(r^2*S)/(4*T*t)
    Wu<-list_well_fxn(u,200)
    Q<-input$Q*192.5
    s<-(Q/(4*pi*T)) * Wu
    df<-data.frame(cbind(r,s))
    ggplot(df %>% filter(!is.na(s)), aes(x=r, y=-s)) +
      geom_line(size=2) + 
      theme_classic()
  
  })
  
  output$dd<-renderText({
    S<-input$S
    t<-input$t
    r<-input$r
    T<-input$k*input$b
    u<-(r^2*S)/(4*T*t)
    Wu<-list_well_fxn(u,200)
    Q<-input$Q*192.5
    s<-(Q/(4*pi*T)) * Wu
    paste0("Drawdown ", input$r, " feet from the well is ", round(s,1), " feet below static water level")
  })
  
  output$text<-renderText({
    Tprime<-input$k*input$b*7.28
    paste0("Transmissivity = ", Tprime, " gpd/ft or ", input$k*input$b, " ft2/d")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
