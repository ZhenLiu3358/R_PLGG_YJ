library(performance)
library(Boruta)
library(randomForest)
library(Rcpp)
library(pROC)
library(ROCR)
library(caret)
library(dplyr)
library(irr)
all_features <- read.csv("final_ICC.csv")

TRA1_identify_1<- 0
TRA1_identify_2<- 0
TRA1_identify_3<- 0
TES1_identify_1<- 0
TES1_identify_2<- 0
TES1_identify_3<- 0

n <- 35
{
  
  all_features$group <-"NO"
  
  data <- c(1:nrow(all_features))
  w <- nrow(all_features)
  n_parts <- 3 
  part_size <- ceiling(w / n_parts)
  
  set.seed(n)
  shuffled_data <- sample(data)
  result <- split(shuffled_data, rep(1:n_parts, each = part_size)[1:w])
  
  CV_Data_1<-all_features[result[[1]],]
  CV_Data_2<-all_features[result[[2]],]
  CV_Data_3<-all_features[result[[3]],]
  
  CV_Data_1$group <-"V1"
  CV_Data_2$group <-"V2"
  CV_Data_3$group <-"V3"
  
  CV_Data<-rbind(CV_Data_1,CV_Data_2,CV_Data_3)
  
  write.csv(CV_Data, "CV_Data.csv", row.names = FALSE)
  CV_Data<-read.csv("CV_Data.csv",row.names = 1)
  
  sort_index <- order(rownames(CV_Data))
  CV_Data_Data <- CV_Data[sort_index, ]
  CV_Data_Data_1 <- CV_Data_Data
  
  test_data_all_1<- CV_Data_Data_1[CV_Data_Data_1$group == "V1", ]
  train_data_all_1<- CV_Data_Data_1[CV_Data_Data_1$group != "V1", ] 
  write.csv(train_data_all_1,file='train_data_all_1.csv')
  write.csv(test_data_all_1,file='test_data_all_1.csv')
  train_data_all_1<-read.csv("train_data_all_1.csv")
  test_data_all_1<-read.csv("test_data_all_1.csv")
  
  train_data<-train_data_all_1[,-c(1:3)]
  train_data<-train_data[,-4033]
  test_data<-test_data_all_1[,-c(1:3)]
  test_data<-test_data[,-4033]
  Process_all1 <- preProcess(train_data,method = c("center","scale"))
  featureTrainNorm <- predict(Process_all1,train_data)
  featureTestNorm <- predict(Process_all1,test_data)
  
  numNcol<-ncol(featureTrainNorm)
  correlationMatrix_Radiomics<-cor(featureTrainNorm[,1:(numNcol)])
  highlycorrelated_Radiomics<-findCorrelation(correlationMatrix_Radiomics,cutoff=0.75)
  train_caret_data_1<-featureTrainNorm[,-highlycorrelated_Radiomics]
  
  train_caret<-cbind(train_data_all_1[2],train_caret_data_1)
  test_data<-cbind(test_data_all_1[2],featureTestNorm)
  b <- 123
  set.seed(b)
  Boruta_all<-Boruta(risk~.,data = train_caret)
  plot(Boruta_all, xlab = "", xaxt = "n")
  lz<-lapply(1:ncol(Boruta_all$ImpHistory),function(i)
    Boruta_all$ImpHistory[is.finite(Boruta_all$ImpHistory[,i]),i])
  names(lz) <- colnames(Boruta_all$ImpHistory)  
  Labels <- sort(sapply(lz,median))
  axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(Boruta_all$ImpHistory),cex.axis = 0.7)
  
  Radio_Name <- getSelectedAttributes(Boruta_all, withTentative = F)
  print(Radio_Name)
  Radio_Name=data.frame(Radio_Name)
  write.csv(Radio_Name,file='Radio_Name.csv',row.names = F)
  
  Boruta_data1 <- names(train_caret) %in% Radio_Name$Radio_Name
  train_boruta_selec<-train_caret[Boruta_data1]
  
  Boruta_data2 <- names(test_data) %in% Radio_Name$Radio_Name
  test_boruta_selec<-test_data[Boruta_data2]
  
  TYPE <- train_data_all_1[,2]
  train_boruta_selec <- cbind(TYPE,train_boruta_selec)
  TYPE <- test_data_all_1[,2]
  
  train1=train_boruta_selec
  test1=test_boruta_selec
  train1$TYPE=as.factor(train1$TYPE)
  test1$TYPE=as.factor(test1$TYPE) 
  train1$TYPE=factor(train1$TYPE,levels = c(1,2),labels=c("A","B"))
  test1$TYPE=factor(test1$TYPE,levels = c(1,2),labels=c("A","B"))
  write.csv(train1,file='train1.csv',row.names = F)
  
  library(MLmetrics)
  set.seed(123)
  modelfit1=train(TYPE~.,data=train1, method = "rf", metric="ROC",
                  trControl = trainControl(method = "repeatedcv",number = 10,repeats = 3,
                                           classProbs = TRUE,summaryFunction = twoClassSummary ))
  
  predictions1_prob_train=predict(modelfit1$finalModel,type = "prob")
  
  predictions1_prob_test=predict(modelfit1,newdata = test1,type = "prob")
  
  train_type <- data.frame(train_data_all_1[,c(1:3)],A=train_data_all_1[,2],B=train_data_all_1[,2])
  train_type$A[which(train_type$A ==2)] <- 0
  train_type$B[which(train_type$B ==1)] <- 0
  train_type$B[which(train_type$B ==2)] <- 1
  
  test_type <- data.frame(test_data_all_1[,c(1:3)],B=test_data_all_1[,2],A=test_data_all_1[,2])
  test_type$A[which(test_type$A ==2)] <- 0
  test_type$B[which(test_type$B ==1)] <- 0
  test_type$B[which(test_type$B ==2)] <- 1
  roc_train1_identify_1= roc(train_type$A,predictions1_prob_train[,1])
  roc_train2_identify_1= roc(train_type$B,predictions1_prob_train[,2])
  roc_test1_identify_1= roc(test_type$A,predictions1_prob_test[,1])
  roc_test2_identify_1= roc(test_type$B,predictions1_prob_test[,2])
  TRA1_identify_1 <- roc_train1_identify_1$auc
  TRA2_identify_1 <- roc_train2_identify_1$auc
  TES1_identify_1 <- roc_test1_identify_1$auc
  TES2_identify_1 <- roc_test2_identify_1$auc
  
  roc_train1_1= roc(train_type$A,predictions1_prob_train[,1])
  roc_train1_1
  cutoff.train1_1=coords(roc_train1_1,"best",ret="threshold",transpose=TRUE)
  rets <- c("threshold", "specificity", "sensitivity", "accuracy")
  ci.coords(roc_train1_1, x=cutoff.train1_1, input = "threshold", ret=rets)
  
  roc_test1_1= roc(test_type$A,predictions1_prob_test[,1])
  roc_test1_1
  cutoff.test1_1=cutoff.train1_1
  rets <- c("threshold", "specificity", "sensitivity", "accuracy","recall","precision")
  ci.coords(roc_test1_1, x=cutoff.test1_1, input = "threshold", ret=rets)
  ci.auc(roc_test1_1)
  
  test_data_all_2<- CV_Data_Data_1[CV_Data_Data_1$group == "V2", ]
  train_data_all_2<- CV_Data_Data_1[CV_Data_Data_1$group != "V2", ] 
  write.csv(train_data_all_2,file='train_data_all_2.csv')
  write.csv(test_data_all_2,file='test_data_all_2.csv')
  train_data_all_2<-read.csv("train_data_all_2.csv")
  test_data_all_2<-read.csv("test_data_all_2.csv")
  
  train_data<-train_data_all_2[,-c(1:3)]
  train_data<-train_data[,-4033]
  test_data<-test_data_all_2[,-c(1:3)]
  test_data<-test_data[,-4033]
  Process_all1 <- preProcess(train_data,method = c("center","scale"))
  featureTrainNorm <- predict(Process_all1,train_data)
  featureTestNorm <- predict(Process_all1,test_data)
  
  numNcol<-ncol(featureTrainNorm)
  correlationMatrix_Radiomics<-cor(featureTrainNorm[,1:(numNcol)])
  highlycorrelated_Radiomics<-findCorrelation(correlationMatrix_Radiomics,cutoff=0.75)
  train_caret_data_2<-featureTrainNorm[,-highlycorrelated_Radiomics]
  
  train_caret<-cbind(train_data_all_2[2],train_caret_data_2)
  test_data<-cbind(test_data_all_2[2],featureTestNorm)
  b <- 123
  set.seed(b)
  Boruta_all<-Boruta(risk~.,data = train_caret)
  plot(Boruta_all, xlab = "", xaxt = "n")
  lz<-lapply(1:ncol(Boruta_all$ImpHistory),function(i)
    Boruta_all$ImpHistory[is.finite(Boruta_all$ImpHistory[,i]),i])
  names(lz) <- colnames(Boruta_all$ImpHistory)  
  Labels <- sort(sapply(lz,median))
  axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(Boruta_all$ImpHistory),cex.axis = 0.7)
  
  Radio_Name <- getSelectedAttributes(Boruta_all, withTentative = F)
  print(Radio_Name)
  Radio_Name=data.frame(Radio_Name)
  write.csv(Radio_Name,file='Radio_Name.csv',row.names = F)
  
  ta1 <- names(train_caret) %in% Radio_Name$Radio_Name
  train_boruta_selec<-train_caret[Boruta_data1]
  
  Boruta_data2 <- names(test_data) %in% Radio_Name$Radio_Name
  test_boruta_selec<-test_data[Boruta_data2]
  
  TYPE <- train_data_all_2[,2]
  train_boruta_selec <- cbind(TYPE,train_boruta_selec)
  TYPE <- test_data_all_2[,2]
  
  train1=train_boruta_selec
  test1=test_boruta_selec
  train1$TYPE=as.factor(train1$TYPE)
  test1$TYPE=as.factor(test1$TYPE) 
  train1$TYPE=factor(train1$TYPE,levels = c(1,2),labels=c("A","B"))
  test1$TYPE=factor(test1$TYPE,levels = c(1,2),labels=c("A","B"))
  write.csv(train1,file='train1.csv',row.names = F)
  
  library(MLmetrics)
  set.seed(123)
  modelfit1=train(TYPE~.,data=train1, method = "rf", metric="ROC",
                  trControl = trainControl(method = "repeatedcv",number = 10,repeats = 3,
                                           classProbs = TRUE,summaryFunction = twoClassSummary ))
  
  predictions1_prob_train=predict(modelfit1$finalModel,type = "prob")
  
  predictions1_prob_test=predict(modelfit1,newdata = test1,type = "prob")
  
  train_type <- data.frame(train_data_all_2[,c(1:3)],A=train_data_all_2[,2],B=train_data_all_2[,2])
  train_type$A[which(train_type$A ==2)] <- 0
  train_type$B[which(train_type$B ==1)] <- 0
  
  test_type <- data.frame(test_data_all_2[,c(1:3)],A=test_data_all_2[,2],B=test_data_all_2[,2])
  test_type$A[which(test_type$A ==2)] <- 0
  test_type$B[which(test_type$B ==1)] <- 0
  test_type$B[which(test_type$B ==2)] <- 1
  roc_train1_identify_2= roc(train_type$A,predictions1_prob_train[,1])
  roc_train2_identify_2= roc(train_type$B,predictions1_prob_train[,2])
  roc_test1_identify_2= roc(test_type$A,predictions1_prob_test[,1])
  roc_test2_identify_2= roc(test_type$B,predictions1_prob_test[,2])
  TRA1_identify_2 <- roc_train1_identify_2$auc
  TRA2_identify_2 <- roc_train2_identify_2$auc
  TES1_identify_2 <- roc_test1_identify_2$auc
  TES2_identify_2 <- roc_test2_identify_2$auc
  
  roc_train1_2= roc(train_type$A,predictions1_prob_train[,1])
  roc_train1_2
  cutoff.train1_2=coords(roc_train1_2,"best",ret="threshold",transpose=TRUE)
  rets <- c("threshold", "specificity", "sensitivity", "accuracy")
  ci.coords(roc_train1_2, x=cutoff.train1_2, input = "threshold", ret=rets)
  ci.auc(roc_train1_2)
  
  roc_test1_2= roc(test_type$A,predictions1_prob_test[,1])
  roc_test1_2
  cutoff.test1_2=cutoff.train1_2
  rets <- c("threshold", "specificity", "sensitivity", "accuracy","recall","precision")
  ci.coords(roc_test1_2, x=cutoff.test1_2, input = "threshold", ret=rets)
  ci.auc(roc_test1_2)
  
  test_data_all_3<- CV_Data_Data_1[CV_Data_Data_1$group == "V3", ]
  train_data_all_3<- CV_Data_Data_1[CV_Data_Data_1$group != "V3", ] 
  write.csv(train_data_all_3,file='train_data_all_3.csv')
  write.csv(test_data_all_3,file='test_data_all_3.csv')
  train_data_all_3<-read.csv("train_data_all_3.csv")
  test_data_all_3<-read.csv("test_data_all_3.csv")
  
  train_data<-train_data_all_3[,-c(1:3)]
  train_data<-train_data[,-4033]
  test_data<-test_data_all_3[,-c(1:3)]
  test_data<-test_data[,-4033]
  Process_all1 <- preProcess(train_data,method = c("center","scale"))
  featureTrainNorm <- predict(Process_all1,train_data)
  featureTestNorm <- predict(Process_all1,test_data)
  numNcol<-ncol(featureTrainNorm)
  correlationMatrix_Radiomics<-cor(featureTrainNorm[,1:(numNcol)])
  highlycorrelated_Radiomics<-findCorrelation(correlationMatrix_Radiomics,cutoff=0.75)
  train_caret_data_3<-featureTrainNorm[,-highlycorrelated_Radiomics]
  
  
  train_caret<-cbind(train_data_all_3[2],train_caret_data_3)
  test_data<-cbind(test_data_all_3[2],featureTestNorm)
  b <- 123
  set.seed(b)
  Boruta_all<-Boruta(risk~.,data = train_caret)
  plot(Boruta_all, xlab = "", xaxt = "n")
  lz<-lapply(1:ncol(Boruta_all$ImpHistory),function(i)
    Boruta_all$ImpHistory[is.finite(Boruta_all$ImpHistory[,i]),i])
  names(lz) <- colnames(Boruta_all$ImpHistory)  
  Labels <- sort(sapply(lz,median))
  axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(Boruta_all$ImpHistory),cex.axis = 0.7)
  
  Radio_Name <- getSelectedAttributes(Boruta_all, withTentative = F)
  print(Radio_Name)
  Radio_Name=data.frame(Radio_Name)
  write.csv(Radio_Name,file='Radio_Name.csv',row.names = F)
  
  Boruta_data1 <- names(train_caret) %in% Radio_Name$Radio_Name
  train_boruta_selec<-train_caret[Boruta_data1]
  
  Boruta_data2 <- names(test_data) %in% Radio_Name$Radio_Name
  test_boruta_selec<-test_data[Boruta_data2]
  
  TYPE <- train_data_all_3[,2]
  train_boruta_selec <- cbind(TYPE,train_boruta_selec)
  TYPE <- test_data_all_3[,2]
  test_boruta_selec <- cbind(TYPE,test_boruta_selec)
  
  train1=train_boruta_selec
  test1=test_boruta_selec
  train1$TYPE=as.factor(train1$TYPE)
  test1$TYPE=as.factor(test1$TYPE) 
  train1$TYPE=factor(train1$TYPE,levels = c(1,2),labels=c("A","B"))
  test1$TYPE=factor(test1$TYPE,levels = c(1,2),labels=c("A","B"))
  write.csv(train1,file='train1.csv',row.names = F)
  
  library(MLmetrics)
  set.seed(123)
  modelfit1=train(TYPE~.,data=train1, method = "rf", metric="ROC",
                  trControl = trainControl(method = "repeatedcv",number = 10,repeats = 3,
                                           classProbs = TRUE,summaryFunction = twoClassSummary ))
  
  predictions1_prob_train=predict(modelfit1$finalModel,type = "prob")
  
  predictions1_prob_test=predict(modelfit1,newdata = test1,type = "prob")
  
  train_type <- data.frame(train_data_all_3[,c(1:3)],A=train_data_all_3[,2],B=train_data_all_3[,2])
  train_type$A[which(train_type$A ==2)] <- 0
  train_type$B[which(train_type$B ==1)] <- 0
  train_type$B[which(train_type$B ==2)] <- 1
  
  test_type <- data.frame(test_data_all_3[,c(1:3)],A=test_data_all_3[,2],B=test_data_all_3[,2])
  test_type$A[which(test_type$A ==2)] <- 0
  test_type$B[which(test_type$B ==1)] <- 0
  test_type$B[which(test_type$B ==2)] <- 1
  roc_train1_identify_3= roc(train_type$A,predictions1_prob_train[,1])
  roc_train2_identify_3= roc(train_type$B,predictions1_prob_train[,2])
  roc_test1_identify_3= roc(test_type$A,predictions1_prob_test[,1])
  roc_test2_identify_3= roc(test_type$B,predictions1_prob_test[,2])
  TRA1_identify_3 <- roc_train1_identify_3$auc
  TRA2_identify_3 <- roc_train2_identify_3$auc
  TES1_identify_3 <- roc_test1_identify_3$auc
  TES2_identify_3 <- roc_test2_identify_3$auc
  
  roc_train1_3= roc(train_type$A,predictions1_prob_train[,1])
  roc_train1_3
  cutoff.train1_3=coords(roc_train1_3,"best",ret="threshold",transpose=TRUE)
  rets <- c("threshold", "specificity", "sensitivity", "accuracy")
  ci.coords(roc_train1_3, x=cutoff.train1_3, input = "threshold", ret=rets)
  ci.auc(roc_train1_3)
  
  roc_test1_3= roc(test_type$A,predictions1_prob_test[,1])
  roc_test1_3
  cutoff.test1_3=cutoff.train1_3
  rets <- c("threshold", "specificity", "sensitivity", "accuracy","recall","precision")
  ci.coords(roc_test1_3, x=cutoff.test1_3, input = "threshold", ret=rets)
  ci.auc(roc_test1_3)
  
  DATA1_identify_1_1 <- data.frame(TRA1_identify_1,TRA1_identify_2,TRA1_identify_3)
  DATA2_identify_2_2 <- data.frame(TES1_identify_1,TES1_identify_2,TES1_identify_3)
  print(DATA1_identify_1_1)
  print(DATA2_identify_2_2 )
  print(n)
  n=n+1
}

plot.roc(roc_train1_1,percent = TRUE, lty = 1, lwd = 3, col = "BLUE",
         cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, font.lab = 2, col.main = "Black")
plot.roc(roc_train1_2, percent = TRUE, add = TRUE, type = "S", lty = 1, lwd = 3, col = "Green")
plot.roc(roc_train1_3, percent = TRUE, add = TRUE, type = "S", lty = 1, lwd = 3, col = "Red")
legend("bottomright",legend = c("fold 1      0.9436", "fold 2      0.7609", "fold 3      0.8978"),
       text.font = 2, lty = 1, lwd = 3, cex = 0.8,
       col = c("Blue","Green","Red"))

plot.roc(roc_test1_1,percent = TRUE, lty = 1, lwd = 3, col = "BLUE",
         cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, font.lab = 2, col.main = "Black")
plot.roc(roc_test1_2, percent = TRUE, add = TRUE, type = "S", lty = 1, lwd = 3, col = "Green")
plot.roc(roc_test1_3, percent = TRUE, add = TRUE, type = "S", lty = 1, lwd = 3, col = "Red")
legend("bottomright",legend = c("fold 1      0.6531", "fold 2      0.7500", "fold 3      0.7083"),
       text.font = 2, lty = 1, lwd = 3, cex = 0.8,
       col = c("Blue","Green","Red"))

