#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
   
   # Application title
   titlePanel("Visualizations of Investing Modalities"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(column(4, sliderInput("initial",
                     "Initial Amount",
                     min = 0,
                     max = 100000,
                     value = 1000,
                     step = 500)) ,
             
         column(4, sliderInput("annual_contrib",
                     "Annual Contribution",
                     min = 0,
                     max = 50000,
                     value = 2000,
                     step = 500)) ,    
         column(4, sliderInput("return_rate",
                    "Return Rate (in %)",
                    min = 0,
                    max = 20,
                    value = 5,
                    step = 0.2))) , 
   fluidRow(column(4, sliderInput("growth_rate",
                    "Growth Rate (in %)",
                    min = 0,
                    max = 20,
                    value = 2,
                    step = 0.2)) , 
            column(4, sliderInput("years",
                    "Years",
                    min = 0,
                    max = 50,
                    value = 20,
                    step = 1)) ,
            column(4, selectInput(inputId = "facet",
                    label = "Facet?",
                    choices = c("No", "Yes"))
                    )) ,

      fluidRow(
         h4("Timelines"), 
         column(12, plotOutput("timelinePlot")), 
         h4("Balances"),
         column(8, verbatimTextOutput("balances"))
      )
   
   )


server <- function(input, output) {
  
df <- reactive({
  future_value <- function(amount, rate, years){
    return(amount*((1+rate)^years))
  }
  
  annuity <- function(contrib, rate, years){
    return(contrib*((((1 + rate)^years)-1)/rate))
  }
  
  growing_annuity <- function(contrib, rate, growth, years){
    return(
      contrib*((((1 + rate)^years) - ((1 + growth)^years))/(rate - growth))
    )
  }
  
  
  years = seq(0, input$years, 1)
  no_contrib = future_value(input$initial, (input$return_rate / 100), years)
  fixed_contrib = no_contrib + annuity(input$annual_contrib, (input$return_rate / 100), years)
  growing_contrib = no_contrib + growing_annuity(input$annual_contrib, (input$return_rate / 100), (input$growth_rate / 100), years)
  df = as.data.frame(cbind(years, no_contrib, fixed_contrib, growing_contrib))
  
  return(df)
  
  
})

  
  
  
   output$timelinePlot <- renderPlot({

     if(input$facet == "Yes"){
       df3 <- data.frame(
         years = rep(df()$year, 3), 
         balance = c(df()$no_contrib,
                     df()$fixed_contrib,
                     df()$growing_contrib),
         type = c(rep("no_contrib", input$years+1),
                  rep("fixed_contrib", input$years+1),
                  rep("growing_contrib", input$years+1))
                  )
       
       library(ggplot2)
       ggplot(df3) +
         geom_line(aes(x = years, y = balance, color = type)) +
         ggtitle("Three Modes of Investing") +
         xlab("Number of Years Passed") +
         ylab("Future Value of Investment") + 
         facet_grid(. ~ type) +
         geom_area(aes(x = years, y = balance, fill=type))
     }
     else{
     

     library(ggplot2)
     df2 <- df()
     ggplot(df2) +
       geom_line(aes(x = years, y = no_contrib, color = "no_contrib")) +
       geom_line(aes(x = years, y = fixed_contrib, color = "fixed_contrib")) +
       geom_line(aes(x = years, y = growing_contrib, color = "growing_contrib")) +
       ggtitle("Three Modes of Investing") +
       xlab("Number of Years Passed") +
       ylab("Future Value of Investment")
     }
   })
   
   output$balances <- renderPrint({
     
      print(df())
   
})
   
}


shinyApp(ui = ui, server = server)

