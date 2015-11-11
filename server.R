library(shiny)
library(ggplot2)

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
  
  
}

