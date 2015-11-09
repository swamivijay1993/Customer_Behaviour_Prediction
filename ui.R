library(shiny)
library(Cairo)

#load the data in this file to access the name of various columns
data <- read.csv(file = "data.csv", header=T, sep=",")

shinyUI=navbarPage("Predictive Analysis",
                
#UI for Data Section in UI                   
  tabPanel("Data",
           dataTableOutput('contents'),
           hr(),
           
           list(basicPage(
          renderDataTable('contents')
        ))),
  
#UI for Plots Section in UI
  tabPanel("Plots",
           fluidPage(
             titlePanel("Analysis Graphs"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "no_of_levels",label = "Number of elements",min=1,
                             max = length(levels(data$Product.Name)),value=3),
                 selectInput(inputId = "x_axis",label = "X-Axis",choices = c("Product.Name","Customer.Name")),
                 selectInput(inputId = "y_axis",label =  "Y-Axis",choices = c("Quantity","Unit.Price"))
               ),
               mainPanel(
                        h5("Select and Double click to zoom"),
                        fluidRow(
                          column(width=6,
                                 plotOutput("mainplot",height = 300,
                                    brush = brushOpts(
                                       id = "plotbrush",
                                      resetOnNew = T
                                    )
                                )
                            ),
                          column(width=6,
                                 plotOutput('innerplot'),height=300)
                       )
                    )
                 )
              )
           ),
  
#UI for Most Frequent Item
  tabPanel("MFI"),
  
  tabPanel("Behaviour")
)