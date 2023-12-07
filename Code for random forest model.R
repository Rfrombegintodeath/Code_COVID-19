##read the datatable
dt_air<-read.csv('dt_air.csv', header=T)
##dt_air contains all the data from June 2021 to November 2022, the random forest models should be developed in two periods 'June 2021-May 2022' and 'June 2022-November 2022', here we take 'June 2021-May 2022' as example
bweek <- filter(dt_air, date %in% c("2021-Jul","2021-Aug","2021-Sep","2021-Oct","2021-Nov","2021-Dec",
                                    "2022-Jan","2022-Feb","2022-Mar","2022-Apr","2022-May"))
bweek <- na.omit(bweek)
set.seed(123)
#Set the training, testing and validation sets
train <- sample(nrow(bweek), nrow(bweek)*0.7)
bweek_train <- bweek[train, ]
bweek_test <- bweek[-train, ]
validation<-sample(nrow(bweek_test), nrow(bweek_test)*0.5)
bweek_val<-bweek_test[validation, ]
bweek_test <- bweek_test[-validation, ]

library(randomForest)
##search for mtry
#replace the target name for follw7 and follw14
#we use follw7 as example here
for(i in 1:15){
  mtry_test <- randomForest(follw7~., data=bweek_train, mtry=i)
  err<-mean(mtry_test$mse)
  print(err)
}
##select mtry
##search for ntree
ntree_fit<-randomForest(follw7~., data=bweek_train, mtry=15, ntree=500)
plot(ntree_fit)
##based on ntree fit, choose the mtry and ntree
bweek_train.forest <- randomForest(follw7~., data = bweek_train, importance = TRUE, mtry=15, ntree=300,  keep.inbag=TRUE)
bweek_train.forest

#check the model performance for trainging data set
bweek_predict <- predict(bweek_train.forest, bweek_train)
cor.test(bweek_train$follw7~, bweek_predict)
bweek_train$predict<-bweek_predict
results<-bweek_train[, c(13,57)]
write.csv(results, "f7_training.csv")

er=0
sae=0
for (i in 1:length(bweek_predict)){
  j=(bweek_predict[i]-bweek_train$follw7[i])^2
  er=er+j
  ae=abs(bweek_predict[i]-bweek_train$follw7[i])
  sae=sae+ae
}
rmse=sqrt(er/length(bweek_predict))
mae=sae/length(bweek_predict)
nrmse=rmse/mean(bweek_train$follw7)
rmse
mae
nrmse

#check the model performance for test data set
bweek_predict <- predict(bweek_train.forest, bweek_test)
cor.test(bweek_test$follw7, bweek_predict,)
plot(bweek_test$follw7, bweek_predict, main = 'training',
     xlab = 'Cases in next 7 days (patient/100k population)', ylab = 'Predict')
bweek_test$predict<-bweek_predict
View(bweek_test)
results<-bweek_test[,c(13,57)]
write.csv(results, "f7_test.csv")
er=0
sae=0
for (i in 1:length(bweek_predict)){
  j=(bweek_predict[i]-bweek_test$follw7[i])^2
  er=er+j
  ae=abs(bweek_predict[i]-bweek_test$follw7[i])
  sae=sae+ae
}
rmse=sqrt(er/length(bweek_predict))
mae=sae/length(bweek_predict)
nrmse=rmse/mean(bweek_test$follw7)
rmse
mae
nrmse

##validation set performance
bweek_predict <- predict(bweek_train.forest, bweek_val)
cor.test(bweek_val$follw7, bweek_predict,)
plot(bweek_val$follw7, bweek_predict, main = 'training',
     xlab = 'Cases in next 7 days (patient/100k population)', ylab = 'Predict')
abline(1, 1)
bweek_val$predict<-bweek_predict
results<-bweek_test[,c(13,57)]
write.csv(results, "f7_validation.csv")
er=0
sae=0
for (i in 1:length(bweek_predict)){
  j=(bweek_predict[i]-bweek_val$follw7[i])^2
  er=er+j
  ae=abs(bweek_predict[i]-bweek_val$follw7[i])
  sae=sae+ae
}
rmse=sqrt(er/length(bweek_predict))
mae=sae/length(bweek_predict)
nrmse=rmse/mean(bweek_val$follw7)
rmse
mae
nrmse

##overall performance
bweek_predict <- predict(bweek_train.forest, bweek)
cor.test(bweek$follw7, bweek_predict)
plot(bweek$follw7, bweek_predict)
bweek$predict<-bweek_predict
results<-bweek[,c(13,57)]
write.csv(results, "f7_all.csv")
er=0
sae=0
for (i in 1:length(bweek_predict)){
  j=(bweek_predict[i]-bweek$follw7[i])^2
  er=er+j
  ae=abs(bweek_predict[i]-bweek$follw7[i])
  sae=sae+ae
}
rmse=sqrt(er/length(bweek_predict))
mae=sae/length(bweek_predict)
nrmse=rmse/mean(bweek$follw7)
rmse
mae
nrmse
