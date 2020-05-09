# 1) Prepare a classification model using SVM for salary data 
# Data Description:
# age -- age of a person
# workclass	-- A work class is a grouping of work 
# education	-- Education of an individuals	
# maritalstatus -- Marital status of an individulas	
# occupation	 -- occupation of an individuals
# relationship -- 	
#   race --  Race of an Individual
# sex --  Gender of an Individual
# capitalgain --  profit received from the sale of an investment	
# capitalloss	-- A decrease in the value of a capital asset
# hoursperweek -- number of hours work per week	
# native -- Native of an individual
# Salary -- salary of an individual

#importing dataset
training_set<-read.csv(file.choose())
test_set<-read.csv(file.choose())
View(training_set)
summary(training_set)
str(training_set)
table(training_set$Salary)

training_set$educationno <- as.factor(training_set$educationno)
test_set$educationno <- as.factor(test_set$educationno)

# Building model 
library(kernlab)

#kernel= vanilladot
model1<-ksvm(training_set$Salary~., 
             data= training_set, kernel = "vanilladot")

Salary_prediction <- predict(model1, test_set)
mean(Salary_prediction==test_set$Salary)#0.8464
table(Salary_prediction,test_set$Salary)

#kernel= rbfdot
model_rfdot<-ksvm(training_set$Salary~., 
                 data= training_set,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_set)
mean(pred_rfdot==test_set$Salary) #0.8520
table(pred_rfdot,test_set$Salary)

# kernal = besseldot
model_besseldot<-ksvm(training_set$Salary ~.,data = training_set,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=test_set)
mean(pred_bessel==test_set$Salary) #0.7897
table(pred_bessel,test_set$Salary)

# kernel = polydot

model_poly<-ksvm(training_set$Salary ~.,data = training_set,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = test_set)
mean(pred_poly==test_set$Salary) # 0.8461
table(pred_poly,test_set$Salary)
