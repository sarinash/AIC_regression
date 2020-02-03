library(Hmisc)
library(psych)
library(car)
library(MASS)
mydata <- "C:/Users/sarina/Desktop/uuu.csv"
# Multiple Linear Regression Example 
my_data <-  read.csv(mydata, header = T)
my_data$Music <- factor(my_data$Music, levels=c("one", "two", "three", "four", "five"))
# Multiple Linear Regression Example 
#Perform 10 fold cross validation
fit <- lm(Originality ~ Head_Movement_Syncrony_Low_frequency+Head_Movement_Syncrony_High_frequency+Music+IBS_left_channel+IBS_Middle_channel
          ,data=my_data)
summary(fit) # show results
AIC_total <- AIC(fit)
write.table(summary(fit)$r.squared,file="one.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
print(AIC_total)
write.table(AIC_total,file="six.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
print(AIC_total)
#Create 10 equally size folds
folds <- cut(seq(1,nrow(my_data)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  valid.sample <- my_data[testIndexes, ]
  train.sample <- my_data[-testIndexes, ]
  fit1 <- lm(Originality ~ Head_Movement_Syncrony_Low_frequency+Head_Movement_Syncrony_High_frequency+Music+IBS_left_channel+IBS_Middle_channel
             ,data=train.sample)
  
  summary(fit) 
  #### Now evaluate the final linear model
  #     Find all predicted values for both a training set and a validation set
  train.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(train.sample, select=c(Originality,Head_Movement_Syncrony_Low_frequency,Head_Movement_Syncrony_High_frequency,Music,IBS_left_channel,IBS_Middle_channel )))
  valid.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(valid.sample, select=c(Originality,Head_Movement_Syncrony_Low_frequency,Head_Movement_Syncrony_High_frequency,Music,IBS_left_channel,IBS_Middle_channel )))
  # The theoretical model performance is defined here as R-Squared
  print (summary(fit1))
  # Check how good is the model on the training set -  RME and MAE
  print(summary(train.sample$Pred.Originality))
  train.RMSE <- sqrt(mean((train.sample$Pred.Originality - train.sample$Originality)^2))
  train.MAE <- mean(abs(train.sample$Pred.Originality - train.sample$Originality))
  print(c( train.RMSE, train.MAE))
  
  
  # Check how good is the model on the validation set -  RME and MAE
  print(summary(valid.sample$Pred.Originality))
  valid.RMSE <-sqrt(mean((valid.sample$Pred.Originality - valid.sample$Originality)^2))
  valid.MAE <- mean(abs(valid.sample$Pred.Originality - valid.sample$Originality))
  print(c( valid.RMSE, valid.MAE))
  
  # Stepwise Regression
  AIC_total1 <- AIC(fit1)
  write.table(summary(fit1)$r.squared,file="two.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(train.RMSE,file="three.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(valid.RMSE,file="four.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(AIC_total1,file="five.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  print(AIC_total1)
}
########################################################################################
########################################################################################
# without low
fit <- lm(Originality ~ Head_Movement_Syncrony_High_frequency+Music+IBS_left_channel+IBS_Middle_channel
          ,data=my_data)
summary(fit) # show results
AIC_total <- AIC(fit)
write.table(summary(fit)$r.squared,file="one.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
print(AIC_total)
write.table(AIC_total,file="six.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
print(AIC_total)
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  valid.sample <- my_data[testIndexes, ]
  train.sample <- my_data[-testIndexes, ]
  fit1 <- lm(Originality ~ Head_Movement_Syncrony_High_frequency+Music+IBS_left_channel+IBS_Middle_channel
             ,data=train.sample)
  
  summary(fit1) 
  #### Now evaluate the final linear model
  #     Find all predicted values for both a training set and a validation set
  train.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(train.sample, select=c(Originality,Head_Movement_Syncrony_High_frequency,Music,IBS_left_channel,IBS_Middle_channel )))
  valid.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(valid.sample, select=c(Originality,Head_Movement_Syncrony_High_frequency,Music,IBS_left_channel,IBS_Middle_channel )))
  # The theoretical model performance is defined here as R-Squared
  print (summary(fit1))
  # Check how good is the model on the training set -  RME and MAE
  print(summary(train.sample$Pred.Originality))
  train.RMSE <- sqrt(mean((train.sample$Pred.Originality - train.sample$Originality)^2))
  train.MAE <- mean(abs(train.sample$Pred.Originality - train.sample$Originality))
  print(c( train.RMSE, train.MAE))
  
  
  # Check how good is the model on the validation set -  RME and MAE
  print(summary(valid.sample$Pred.Originality))
  valid.RMSE <-sqrt(mean((valid.sample$Pred.Originality - valid.sample$Originality)^2))
  valid.MAE <- mean(abs(valid.sample$Pred.Originality - valid.sample$Originality))
  print(c( valid.RMSE, valid.MAE))
  
  # Stepwise Regression
  
  AIC_total1 <- AIC(fit1)
  write.table(summary(fit1)$r.squared,file="two.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(train.RMSE,file="three.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(valid.RMSE,file="four.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(AIC_total1,file="five.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  
  print(AIC_total1)
}
#####################################################################################################
############################################################################################################################################

##############################################################################################################################################################
##############################################################################################################################################################
#without high
fit <- lm(Originality ~ Head_Movement_Syncrony_Low_frequency+Music+IBS_left_channel+IBS_Middle_channel
          ,data=my_data)
summary(fit) # show results
AIC_total <- AIC(fit)
write.table(summary(fit)$r.squared,file="one.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
print(AIC_total)
write.table(AIC_total,file="six.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
print(AIC_total)
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  valid.sample <- my_data[testIndexes, ]
  train.sample <- my_data[-testIndexes, ]
  fit1 <- lm(Originality ~ Head_Movement_Syncrony_Low_frequency+Music+IBS_left_channel+IBS_Middle_channel
             ,data=train.sample)
  
  summary(fit1) 
  #### Now evaluate the final linear model
  #     Find all predicted values for both a training set and a validation set
  train.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(train.sample, select=c(Originality,Head_Movement_Syncrony_Low_frequency,Music,IBS_left_channel,IBS_Middle_channel )))
  valid.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(valid.sample, select=c(Originality,Head_Movement_Syncrony_Low_frequency,Music,IBS_left_channel,IBS_Middle_channel )))
  # The theoretical model performance is defined here as R-Squared
  print (summary(fit1))
  # Check how good is the model on the training set -  RME and MAE
  print(summary(train.sample$Pred.Originality))
  train.RMSE <- sqrt(mean((train.sample$Pred.Originality - train.sample$Originality)^2))
  train.MAE <- mean(abs(train.sample$Pred.Originality - train.sample$Originality))
  print(c( train.RMSE, train.MAE))
  
  
  # Check how good is the model on the validation set -  RME and MAE
  print(summary(valid.sample$Pred.Originality))
  valid.RMSE <-sqrt(mean((valid.sample$Pred.Originality - valid.sample$Originality)^2))
  valid.MAE <- mean(abs(valid.sample$Pred.Originality - valid.sample$Originality))
  print(c( valid.RMSE, valid.MAE))
  
  # Stepwise Regression
  
  AIC_total1 <- AIC(fit1)
  write.table(summary(fit1)$r.squared,file="two.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(train.RMSE,file="three.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(valid.RMSE,file="four.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(AIC_total1,file="five.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  print(AIC_total1)
}
###############################################################################################################################
########################################################################################################################################
#without music
fit <- lm(Originality ~ Head_Movement_Syncrony_Low_frequency+Head_Movement_Syncrony_High_frequency+IBS_left_channel+IBS_Middle_channel
          ,data=my_data)
summary(fit) # show results
AIC_total <- AIC(fit)
print(AIC_total)
write.table(summary(fit)$r.squared,file="one.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
print(AIC_total)
write.table(AIC_total,file="six.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  valid.sample <- my_data[testIndexes, ]
  train.sample <- my_data[-testIndexes, ]
  fit1 <- lm(Originality ~ Head_Movement_Syncrony_Low_frequency+Head_Movement_Syncrony_High_frequency+IBS_left_channel+IBS_Middle_channel
             ,data=train.sample)
  
  summary(fit1) 
  #### Now evaluate the final linear model
  #     Find all predicted values for both a training set and a validation set
  train.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(train.sample, select=c(Originality,Head_Movement_Syncrony_Low_frequency,Head_Movement_Syncrony_High_frequency,IBS_left_channel,IBS_Middle_channel )))
  valid.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(valid.sample, select=c(Originality,Head_Movement_Syncrony_Low_frequency,Head_Movement_Syncrony_High_frequency,IBS_left_channel,IBS_Middle_channel )))
  # The theoretical model performance is defined here as R-Squared
  print (summary(fit1))
  # Check how good is the model on the training set -  RME and MAE
  print(summary(train.sample$Pred.Originality))
  train.RMSE <- sqrt(mean((train.sample$Pred.Originality - train.sample$Originality)^2))
  train.MAE <- mean(abs(train.sample$Pred.Originality - train.sample$Originality))
  print(c( train.RMSE, train.MAE))
  
  
  # Check how good is the model on the validation set -  RME and MAE
  print(summary(valid.sample$Pred.Originality))
  valid.RMSE <-sqrt(mean((valid.sample$Pred.Originality - valid.sample$Originality)^2))
  valid.MAE <- mean(abs(valid.sample$Pred.Originality - valid.sample$Originality))
  print(c( valid.RMSE, valid.MAE))
  
  # Stepwise Regression
  
  AIC_total1 <- AIC(fit1)
  write.table(summary(fit1)$r.squared,file="two.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(train.RMSE,file="three.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(valid.RMSE,file="four.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(AIC_total1,file="five.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  print(AIC_total1)
}
##########################################################################################################################
#######################################################################################################################################
#without left
fit <- lm(Originality ~ Head_Movement_Syncrony_Low_frequency+Head_Movement_Syncrony_High_frequency+Music+IBS_Middle_channel
          ,data=my_data)
summary(fit) # show results
AIC_total <- AIC(fit)
write.table(summary(fit)$r.squared,file="one.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
print(AIC_total)
write.table(AIC_total,file="six.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
print(AIC_total)
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  valid.sample <- my_data[testIndexes, ]
  train.sample <- my_data[-testIndexes, ]
  fit1 <- lm(Originality ~ Head_Movement_Syncrony_Low_frequency+Head_Movement_Syncrony_High_frequency+Music+IBS_Middle_channel
             ,data=train.sample)
  
  summary(fit1) 
  #### Now evaluate the final linear model
  #     Find all predicted values for both a training set and a validation set
  train.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(train.sample, select=c(Originality,Head_Movement_Syncrony_Low_frequency,Head_Movement_Syncrony_High_frequency,Music,IBS_Middle_channel )))
  valid.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(valid.sample, select=c(Originality,Head_Movement_Syncrony_Low_frequency,Head_Movement_Syncrony_High_frequency,Music,IBS_Middle_channel )))
  # The theoretical model performance is defined here as R-Squared
  print (summary(fit1))
  # Check how good is the model on the training set -  RME and MAE
  print(summary(train.sample$Pred.Originality))
  train.RMSE <- sqrt(mean((train.sample$Pred.Originality - train.sample$Originality)^2))
  train.MAE <- mean(abs(train.sample$Pred.Originality - train.sample$Originality))
  print(c( train.RMSE, train.MAE))
  
  
  # Check how good is the model on the validation set -  RME and MAE
  print(summary(valid.sample$Pred.Originality))
  valid.RMSE <-sqrt(mean((valid.sample$Pred.Originality - valid.sample$Originality)^2))
  valid.MAE <- mean(abs(valid.sample$Pred.Originality - valid.sample$Originality))
  print(c( valid.RMSE, valid.MAE))
  
  # Stepwise Regression
  
  AIC_total1 <- AIC(fit1)
  write.table(summary(fit1)$r.squared,file="two.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(train.RMSE,file="three.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(valid.RMSE,file="four.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(AIC_total1,file="five.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  print(AIC_total1)
}

##################################################################################
##########################################################################################
#without right
fit <- lm(Originality ~ Head_Movement_Syncrony_Low_frequency+Head_Movement_Syncrony_High_frequency+Music+IBS_left_channel
          ,data=my_data)
summary(fit) # show results
write.table(summary(fit)$r.squared,file="one.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
print(AIC_total)
write.table(AIC_total,file="six.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
AIC_total <- AIC(fit)
print(AIC_total)
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  valid.sample <- my_data[testIndexes, ]
  train.sample <- my_data[-testIndexes, ]
  fit1 <- lm(Originality ~ Head_Movement_Syncrony_Low_frequency+Head_Movement_Syncrony_High_frequency+Music+IBS_left_channel
             ,data=train.sample)
  
  summary(fit1) 
  #### Now evaluate the final linear model
  #     Find all predicted values for both a training set and a validation set
  train.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(train.sample, select=c(Originality,Head_Movement_Syncrony_Low_frequency,Head_Movement_Syncrony_High_frequency,Music,IBS_left_channel )))
  valid.sample$Pred.Originality <- predict(fit1, 
                                       newdata = subset(valid.sample, select=c(Originality,Head_Movement_Syncrony_Low_frequency,Head_Movement_Syncrony_High_frequency,Music,IBS_left_channel )))
  # The theoretical model performance is defined here as R-Squared
  print (summary(fit1))
  # Check how good is the model on the training set -  RME and MAE
  print(summary(train.sample$Pred.Originality))
  train.RMSE <- sqrt(mean((train.sample$Pred.Originality - train.sample$Originality)^2))
  train.MAE <- mean(abs(train.sample$Pred.Originality - train.sample$Originality))
  print(c( train.RMSE, train.MAE))
  
  
  # Check how good is the model on the validation set -  RME and MAE
  print(summary(valid.sample$Pred.Originality))
  valid.RMSE <-sqrt(mean((valid.sample$Pred.Originality - valid.sample$Originality)^2))
  valid.MAE <- mean(abs(valid.sample$Pred.Originality - valid.sample$Originality))
  print(c( valid.RMSE, valid.MAE))
  
  # Stepwise Regression
  
  AIC_total1 <- AIC(fit1)
  write.table(summary(fit1)$r.squared,file="two.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(train.RMSE,file="three.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(valid.RMSE,file="four.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  write.table(AIC_total1,file="five.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
  print(AIC_total1)
}