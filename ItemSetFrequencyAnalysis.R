#Predicting items with highest sales
#Algorithm: Apriori
#Packages Required:
#					1. gdata  - To read CSV files
#					2. arules - For manipulating and analyzing transaction data and patterns
#							  - Also provides interfaces to C implementations of the association mining algorithms Apriori and Eclat
#About Dataset:
#				Total Training Sets = 983
#				Total Products		= 98

#Load the libraries
library("gdata")
library("arules")
library(e1071)

#Load 
dataset<-read.csv(file="Retailer Transaction Data.csv",header=T,sep=",")

subset<-dataset[c(2,5)]

#############AggPosData<-split(RetailPosData$ProductName,RetailPosData$Trans_Id)

subset<-subset[!duplicated(subset[c(1,2)]),]

aggSubSet<-split(subset$Product.Name,subset$Customer.Name)

trns<-as(aggSubSet,"transactions")

Rules<-apriori(trns,parameter=list(supp=0.05,conf=0.6,target="rules",minlen=2))
ItemSet<-inspect(Rules[1:100])
write.csv(ItemSet, file= "ItenSet.csv")
#create an item frequency bar plot for inspecting the item frequency distribution of products sold
#Top 10 items being purchased
itemFrequencyPlot(trns, topN = 10, type="absolute",xlab="Most Frequent Item (MFI)",ylab="Frequency",main="Top 10 Frequent Items")

########################################################################################################
########################################################################################################
################ Analyzing purchase of Most frequent Item using SVM ################################
#######################Item Analyzed if Frankfurter ####################################################

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

#Plot model on trainingset
plot(model,trainingset)

#Prediction using the model which has been trained using the trainingset
prediction<-predict(model,testset[,-1])

#Generate the confusion matrix to find the accuracy
tab <-table(pred=prediction,true=testset[,1])
accuracy <- ((tab[1]+tab[4])/sum(tab))*100

#############
#model<-svm(Order.Val ~ .,data=cleanData,kernel="polynomial")
#model<-svm(Order.Val ~ .,data=cleanData,kernel="polynomial",degree=3,coef0=0.045)
#model<-svm(Order.Val ~ .,data=cleanData,kernel="polynomial",degree=3,coef0=0.045,cost=1.3,tolerance=0.008,epsilon=1)
accuracy


