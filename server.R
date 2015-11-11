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
    itemFrequencyPlot(trns, topN = topLevel, type="absolute", popCol = "black",
                      xlab="Most Frequent Item (MFI)",ylab="Frequency",
                      main=paste("Top",topLevel,"Frequent Items",sep=" "))
  })
  
  output$ItemGraph <- renderPlot({
    topLevel <- fetchTopLevel()
    topLevel
  })
  
  #service for Behaviour 
  
  svmSubset<-dataset[c(3,5,6,7)]
  
  svmSubset <- svmSubset[svmSubset$Product.Name =="frankfurter",]
  
  svmSubset<-svmSubset[c(1,3,4)]
  
  ##svmSubset$Quantity <- svmSubset$Unit.Price * svmSubset$Quantity
  svmSubset$Total.Cost <- svmSubset$Unit.Price * svmSubset$Quantity
  ####names(svmSubset)[3] <- "Total.Cost"
  
  svmSubset$Order.Year <- as.numeric(format(as.Date(svmSubset$Order.Date,"%m/%d/%Y"),"%Y"))
  svmSubset$Order.Date <- as.numeric(format(as.Date(svmSubset$Order.Date,"%m/%d/%Y"),"%m"))
  
  names(svmSubset)[1] <- "Order.Month"
  
  for(i in 1:length(svmSubset$Total.Cost))
  {
    med <- median(svmSubset$Total.Cost)
    if((svmSubset$Total.Cost[i])>med)
      svmSubset$Order.Val[i]="High"
    else
      svmSubset$Order.Val[i]="Low"
  }
  
  svmSubset2 <- svmSubset[c(6,1,3)]
  
  processData <- write.csv(svmSubset2, file= "newdata.csv")
  #processData<-read.csv(file="newdata.csv",header=T,sep="," )
  cleanData <- read.csv(file="newdata.csv",header=T,sep=",")
  cleanData <- cleanData[c(2,3,4)]
  #model<-svm(Order.Val ~ .,data=cleanData)
  model<-svm(Order.Val ~ .,data=cleanData,kernel="polynomial",degree=3,coef0=0.045,cost=1.3,tolerance=0.008,epsilon=1)
  
  plot(model,cleanData)
  
  #Now, next step is to do the prediction.
  #So we classify 70% of dataset as training dataset and 30% as testing dataset
  
  #Create an index
  index<-1:nrow(cleanData)
  
  #Create testindex to sample out the 30% of the dataset
  testindex<-sample(index,trunc(length(index)*25/100))
  
  #Segregate the testdataset and trainingdataset using the testindex
  testset<-cleanData[testindex,]
  trainingset<-cleanData[-testindex,]
  
  output$svmGraph<- renderPlot({
    plot(model,trainingset)
  })
  
}

