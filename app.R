#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(lubridate)
library(tidyverse)
library(scales)
library(ggthemes)
library(RColorBrewer)

setwd("~/PRTRAKR")
prs <- read.csv("data/pRdata.csv")

# plot theme
My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 17),
  axis.text.y = element_text(size = 17),
  axis.title.y = element_text(size = 18),
  plot.title = element_text(color = "#340fea", size = 35, face = "bold"),
  plot.subtitle = element_text(color = "#c8941b", size = 20, face = "bold"),
  strip.text = element_text(color = "#3BB44A", size = 35, face = "bold"))

ui <- fluidPage(
  
  

  checkboxGroupInput("lift", "Show:",
                c("Bench Press" = "bp",
                  "Press"       = "press",
                  "Squat"       = "squat",
                  "Dead Lift"   = "dl")),
  plotOutput("plot"),
  # tableOutput("y"),
  # textOutput("z"),
  DT::dataTableOutput('data')
)

server <- function(input, output, session) {

  rv <- reactiveVal(prs)
  
  output$plot <- renderPlot(
 
    ggplot(data=rv(), aes(x=Date, y=Value)) + geom_line(aes(group=interaction(Name, Metric), color=Name, linetype=Metric), size=2.5) +
      scale_color_brewer(palette = 'Dark2')
  )



  output$data <- DT::renderDataTable ({
    DT::datatable(rv(), editable = TRUE)
  })
  
  observeEvent(input$data_cell_edit, {
    info <- input$data_cell_edit
    newdf <- rv()
    newdf[info$row, info$col] <- info$value
    rv(newdf)
    prs <<- rv()
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
