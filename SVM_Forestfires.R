# classify the Size_Categorie using SVM
# month	month of the year: 'jan' to 'dec'
# day	day of the week: 'mon' to 'sun'
# FFMC	FFMC index from the FWI system: 18.7 to 96.20
# DMC	DMC index from the FWI system: 1.1 to 291.3
# DC	DC index from the FWI system: 7.9 to 860.6
# ISI	ISI index from the FWI system: 0.0 to 56.10
# temp	temperature in Celsius degrees: 2.2 to 33.30
# RH	relative humidity in %: 15.0 to 100
# wind	wind speed in km/h: 0.40 to 9.40
# rain	outside rain in mm/m2 : 0.0 to 6.4
# Size_Categorie 	the burned area of the forest ( Small , Large)

dataset <- read.csv(file.choose())
View(dataset)
str(dataset)
summary(dataset)
attach(dataset)

dataset$month<-as.numeric(factor(dataset$month))
dataset$day<-as.numeric(factor(dataset$day))
dataset$size_category<-factor(dataset$size_category,levels = c('small','large'),
                              labels = c(0,1))

dataset_1<- dataset[,-c(12:30)]
View(dataset_1)
summary(dataset_1)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
dataset_norm<-as.data.frame(lapply(dataset_1[,c(1:11)],FUN=normalize))
dataset_norm<-as.data.frame(cbind(dataset_norm,dataset_1[12]))

colnames(dataset_norm)[12]<-"size"
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset_norm$size, SplitRatio = 0.8)
training_set = subset(dataset_norm, split == TRUE)
test_set = subset(dataset_norm, split == FALSE)

attach(dataset_norm)


# Building model 
library(kernlab)

#kernel= vanilladot
model1<-ksvm(training_set$size~., 
             data= training_set, kernel = "vanilladot")

size_prediction <- predict(model1, test_set)
mean(size_prediction==test_set$size)#0.875
table(size_prediction,test_set$size)

#kernel= rbfdot
model_rfdot<-ksvm(training_set$size~., 
                  data= training_set,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_set)
mean(pred_rfdot==test_set$size) #0.8076
table(pred_rfdot,test_set$size)

# kernal = besseldot
model_besseldot<-ksvm(training_set$size ~.,data = training_set,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=test_set)
mean(pred_bessel==test_set$size) #0.8076
table(pred_bessel,test_set$size)


# kernel = polydot

model_poly<-ksvm(training_set$size ~.,data = training_set,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = test_set)
mean(pred_poly==test_set$size) # 0.875
table(pred_poly,test_set$size)
