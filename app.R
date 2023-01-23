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
prs$Date=mdy(prs$Date)
str(prs)
# plot theme
My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 17),
  axis.text.y = element_text(size = 17),
  legend.text = element_text(size=14),
  axis.title.y = element_text(size = 18),
  plot.title = element_text(color = "#340fea", size = 35, face = "bold"),
  plot.subtitle = element_text(color = "#c8941b", size = 20, face = "bold"),
  strip.text = element_text(color = "#3BB44A", size = 35, face = "bold"))

ui <- fluidPage(
  
  checkboxGroupInput("lift", "Show:",
                c("Weight"      = "Weight",
                  "Bench_Press" = "Bench_Press",
                  "Press"       = "Press",
                  "Squat"       = "Squat",
                  "Deadlift"   = "Deadlift"), selected = c("Bench_Press","Weight")),
  plotOutput("plot"),
  DT::dataTableOutput('data')
)

server <- function(input, output, session) {

 
  
  # rv <- reactiveVal(prs %>% filter(Metric %in% c("Bench_Press")) )
  
  # rv <- reactive({
  #   prs %>% filter(Metric %in% c("Bench_Press"))
  # })
  # 
  
  # rv <- reactive({
  #   prs=prs %>% filter(Metric %in% input$lift)
  # })
  # 
  rv <- reactiveValues(prz=prs)
  
  n <- reactiveVal() # create this value to store s in observe() below
  observe({
    s<-rv$prz %>% filter(Metric %in% input$lift)
    n(s)
  })
  
  
  observe({print(input$lift)})
  
  output$plot <- renderPlot(
 
    ggplot(data=n(), aes(x=Date, y=Value)) + geom_point(color="black", size=3.5) +
      geom_line(aes(group=interaction(Name, Metric), color=Name, linetype=Metric), size=2.5) +
      scale_color_brewer(palette = 'Dark2') + 
      scale_x_date(date_breaks = "1 month", 
                   limits = as.Date (c (floor_date(min(prs$Date), 'month'),
                                        ceiling_date(max(prs$Date), 'month')))) + 
      theme_minimal() +
      My_Theme
  )



  output$data <- DT::renderDataTable ({
    # DT::datatable(rv(), editable = TRUE)
    DT::datatable(n(), editable = TRUE)
    
        # n()
  })
  
  observeEvent(input$data_cell_edit, {
    info <- input$data_cell_edit
    newdf <- n()
    newdf[info$row, info$col] <- info$value
    n(newdf)
    prs <<- n()
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
