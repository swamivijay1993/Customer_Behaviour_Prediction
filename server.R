library(shiny)
library(ggplot2)
library(arules)
library(gdata)
library(e1071)

server=function(input, output) {
  
dataset <- read.csv(file="data.csv", header = T, sep=",")
  
#service for data navigation tab  
  output$contents = renderDataTable({
    dataset
  })
  
#service for plot navigation tab
  range <- reactiveValues(x=NULL,y=NULL)
  

  output$mainplot <- renderPlot({
        ggplot(data=dataset,aes_string(input$x_axis,input$y_axis)) +
          geom_point()
      })
  
  output$innerplot <- renderPlot({
    ggplot(data=dataset, aes_string(input$x_axis,input$y_axis)) +
      geom_point() +
      coord_cartesian(xlim=range$x,ylim=range$y)
  })
  
  observe({
    brush <- input$plotbrush
    if(!is.null(brush))
    {
      range$x <- c(brush$xmin,brush$xmax)
      range$y <- c(brush$ymin,brush$ymax)
    }
    else
    {
      range$x <- NULL
      range$y <- NULL
    }
  })
  
  #service for Frequency
  subset<-dataset[c(2,5)]
  #############AggPosData<-split(RetailPosData$ProductName,RetailPosData$Trans_Id)
  subset<-subset[!duplicated(subset[c(1,2)]),]
  
  aggSubSet<-split(subset$Product.Name,subset$Customer.Name)
  
  trns<-as(aggSubSet,"transactions")
  
  Rules<-apriori(trns,parameter=list(supp=0.05,conf=0.6,target="rules",minlen=2))
  ItemSet<-inspect(Rules[1:100])
  
  output$Itemcontents<- renderDataTable({
    ItemSet[c(1,2,3,5)]
  })
  
  #service for MFI
  fetchTopLevel <- reactive({
    topLevel <- input$top_level
    itemFrequencyPlot(trns, topN = topLevel, type="absolute",
                      xlab="Most Frequent Item (MFI)",ylab="Frequency",main="Top 10 Frequent Items")
  })
  
  output$ItemGraph <- renderPlot({
    topLevel <- fetchTopLevel()
    topLevel
  })
  
}

