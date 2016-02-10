#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(mosaic)

load(url("https://stat.duke.edu/~mc301/data/ames.RData"))

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(

         selectInput("selected_var",
                     "Variable:",
                     choices = names(ames),
                     selected = "area"),         
        
         numericInput("n_samp",
                     "Sample size:",
                     min = 1,
                     max = nrow(ames),
                     value = 30),
        
         numericInput("n_sim",
                      "Number of samples:",
                      min = 1,
                      max = 20000,
                      value = 150) 
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("sampling_plot"),
         verbatimTextOutput("sampling_mean"),
         verbatimTextOutput("sampling_se")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   # create sampling distribution
   sampling_dist <- reactive({
     do(input$n_sim) * ames %>%
       sample_n(input$n_samp, replace = TRUE) %>%
       summarise(x_bar = mean(area))     
   })

   # plot sampling distribution
   output$sampling_plot <- renderPlot({
      ggplot(sampling_dist(), aes(x = x_bar)) +
        geom_histogram() +
        ggtitle(paste0("Sampling distribution of mean ", 
                      input$selected_var, " (n = ", input$n_samp, ")")) +
        xlab(paste("mean", input$selected_var)) 
   })
   
   # mean of sampling distribution
   output$sampling_mean <- renderText({
     paste0("mean = ", round(mean(sampling_dist()$x_bar), 2))
   })
   
   # mean of sampling distribution
   output$sampling_se <- renderText({
     paste0("SE = ", round(sd(sampling_dist()$x_bar), 2))
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

