# Author : Swaminathan
#purpose : User Interface

library(shiny)
library(Cairo)

#load the data in this file to access the name of various columns
dataset <- read.csv(file = "data.csv", header=T, sep=",")

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
                 selectInput(inputId = "x_axis",label = "X-Axis",choices = c("Product.Name","Customer.Name")),
                 selectInput(inputId = "y_axis",label =  "Y-Axis",choices = c("Quantity","Unit.Price")),
                 
                 width = 3
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

#UI for Apriori Analysis
tabPanel("Frequency",
         dataTableOutput('Itemcontents'),
         hr(),
         
         list(basicPage(
           renderDataTable('Itemcontents')
         ))
         ),
  
#UI for Most Frequent Item
  tabPanel("MFI",
           fluidPage(
             titlePanel("Most Frequent Item"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "top_level",label = "Top Items",min=1,
                             max = 40,value=10), width = 4),
                 mainPanel(plotOutput("ItemGraph"))
               )
             )
           ),
  
  tabPanel("Behaviour",basicPage(plotOutput("svmGraph"))),
  tabPanel("Details",basicPage(
           tabsetPanel(
             tabPanel("Products Relative to Frankfurter",plotOutput("avg")),
             tabPanel("Accuracy",verbatimTextOutput("accuracy_text"))
           )))
)