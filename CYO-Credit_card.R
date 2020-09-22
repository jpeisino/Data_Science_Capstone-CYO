  ###--------------------------------------------------------------------
  ##1 Install required packages
  ###--------------------------------------------------------------------
  if(!require(gdata)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(tidyverse)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(ggplot2)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(knitr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(gridExtra)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(corrplot)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
  if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(rpart)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(matrixStats)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(gam)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(randomForest)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(dplyr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(dplyr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  library(gbm)
  library(dplyr)
  library(gdata)
  library(tidyverse)
  library(ggplot2)
  library(knitr)
  library(gridExtra)
  library(corrplot)
  library(caret)
  library(data.table)
  library(rpart)
  library(matrixStats)
  library(gam)
  library(randomForest)
  
  ###--------------------------------------------------------------------
  ##2- Data Cleaning
  ###--------------------------------------------------------------------
  
  url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00350/default%20of%20credit%20card%20clients.xls"
  creditcard <- read.xls(url)
  colnames<-creditcard[1,]
  colnames[7]<-"PAY_1"
  colnames[25]<-"def"
  colnames(creditcard)<-colnames
  creditcard<-creditcard[-1,]
  ###--------------------------------------------------------------------
  ### 3- Data Analysis
  ###--------------------------------------------------------------------
  
  ###3.1- Correlation matrix
  
  creditcard<-lapply(creditcard, as.numeric)
  creditcard<-as.data.frame(creditcard)
  r<-cor(as.matrix(creditcard))
  corrplot(r, method = "circle", type="upper",sig.level = 0.01,insig = "blank", 
           tl.col = "black", tl.srt = 45, tl.cex=0.5)
  
  ### 3.2 Adjusting data to make charts easy to understand 
  ##SEX data
  
  creditcard$SEX[creditcard$SEX=="1"]<-"male"
  creditcard$SEX[creditcard$SEX=="2"]<-"female"
  
  ## Education data
  creditcard$EDUCATION[creditcard$EDUCATION=="1"]<-"graduate school"
  creditcard$EDUCATION[creditcard$EDUCATION=="2"]<-"university"
  creditcard$EDUCATION[creditcard$EDUCATION=="3"]<-"high school"
  creditcard$EDUCATION[creditcard$EDUCATION=="4"]<-"others"
  
  ##Marital status
  creditcard$MARRIAGE[creditcard$MARRIAGE=="1"]<-"married"
  creditcard$MARRIAGE[creditcard$MARRIAGE=="2"]<-"single"
  creditcard$MARRIAGE[creditcard$MARRIAGE=="3"]<-"others"
  
  #Default
  creditcard$def[creditcard$def=="1"]<-"yes"
  creditcard$def[creditcard$def=="0"]<-"no"
  
  ## 3.3 Analyisi of database structure
  dim(creditcard)
  str(creditcard)
  
  ## 3.4 Age Effect
  
  range(creditcard$AGE)
  mean(creditcard$AGE)
  median(creditcard$AGE)
  
  ##age frec
  creditcard%>%ggplot(aes(AGE,fill=def))+geom_histogram(bins = 30)+
    geom_vline(xintercept = mean(creditcard$AGE), lty = 2, color= "Red")+
    labs(title = "Client Age Distribution",x = "age range", y = "qty")
  
  ## 3.4 Marital Status
  
  ##Marital Status count
  creditcard%>%ggplot(aes(MARRIAGE))+geom_bar(fill="steelblue")+
    labs(title = "Marital Status", y = "qty")
  
  ##Marital status table 
  creditcard%>%group_by(MARRIAGE)%>%summarize(n=n(),prop=n/30000)%>%arrange(desc(prop))%>%kable()
  
  ## Maritatl status by age range
  creditcard%>%mutate(age_range=paste0(trunc(AGE/10)*10,"'s"),prop=n())%>%ggplot(aes(age_range,prop,fill=MARRIAGE))+geom_bar(position="fill", stat="identity")+labs(title = "Client Age Distribution",x = "age range", y = "%")
  
  ## 3.5 Education
  ###Education by age range
  creditcard%>%mutate(age_range=paste0(trunc(AGE/10)*10,"'s"),prop=n())%>%
    ggplot(aes(age_range,prop,fill=EDUCATION))+
    geom_bar(position="fill", stat="identity")+
    labs(title = "Education",x = "age range", y = "%")
  
  ### Education range table
  creditcard%>%group_by(EDUCATION)%>%summarize(n=n(),prop=n/30000)%>%
    arrange(desc(prop))%>%kable()
  
  ### 3.6 Defaulters
  creditcard%>%ggplot(aes(def))+geom_bar(fill="steelblue")+
    labs(title = "DEFAULTERS")
  
  ###Defaulters by age range
  
  creditcard%>%mutate(age_range=paste0(trunc(AGE/10)*10,"'s"),prop=n())%>%
    filter(def=="yes")%>%ggplot(aes(age_range))+geom_bar(fill="steelblue")+
  labs(title = "Defaulters by age range")
  
  ##table of defaulters by age range
  
  totaldefault<-length(creditcard$ID[creditcard$def=="yes"])
  creditcard%>%mutate(age_range=paste0(trunc(AGE/10)*10,"'s"))%>%
    filter(def=="yes")%>%group_by(age_range)%>%
    summarize(def_pay=n(),prop=paste0(round(def_pay/totaldefault*100,2),"%"))%>%
    arrange(desc(def_pay))%>%kable()
  
  ###3.7 Limit balance
  
  ##mean limit balance per age range
  
  creditcard%>%mutate(age_range=paste0(trunc(AGE/10)*10,"'s"))%>%
  group_by(age_range)%>%
  summarize(avg=mean(LIMIT_BAL))%>%ggplot(aes(age_range,avg))+
  geom_bar(stat="identity",fill="steelblue")+
  labs(title = "Average limit balance per age range")
  
  ## distribution of limit balance per age range
  creditcard%>%mutate(age_range=paste0(trunc(AGE/10)*10,"'s"))%>%
    ggplot(aes(LIMIT_BAL,fill=age_range))+geom_density(alpha = 0.2)+
    facet_wrap(age_range ~ .,nrow=3,ncol=2)+
    labs(title = "Limit distribution per age range")
  
  ##stacked curve for limit balance 
  
  creditcard%>%mutate(age_range=paste0(trunc(AGE/10)*10,"'s"))%>%
    ggplot(aes(LIMIT_BAL,fill=age_range))+
    geom_density(alpha = 0.2, position = "stack")+
  labs(title = "Stacked limit distribution per age range")
  
  ## most popular limit balances
  
  creditcard%>%
  mutate(popular=ifelse(LIMIT_BAL%in%c(20000,30000,50000,80000,200000,360000,500000),
  "popular","not popular"))%>%ggplot(aes(x=reorder(LIMIT_BAL,LIMIT_BAL),fill=popular)) + 
    geom_bar() +theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_vline(xintercept = 500000, color= "Red")+
    labs(title = "Most frequent limit balance")
  
  ###Proportion of defaults per limit balance
  creditcard%>%mutate(Limit_range= 
        ifelse(LIMIT_BAL<=100000,"less than 100K",
        ifelse(LIMIT_BAL<=200000,"less than 200K",
        ifelse(LIMIT_BAL<=300000,"less than 300K",
        ifelse(LIMIT_BAL<=400000,"less than 400K",
        ifelse(LIMIT_BAL<=500000,"less than 500K","more than 500K" ))))),
        prop=n())%>% ggplot(aes(Limit_range,prop,fill=def))+
    geom_bar(position="fill", stat="identity")+
    labs(title = "Defaulters per limit balance")
  
  ###Table of defauts per limit balance
  
  creditcard%>%mutate(Limit_range= 
    ifelse(LIMIT_BAL<=100000,"less than 100K",
    ifelse(LIMIT_BAL<=200000,"less than 200K",
    ifelse(LIMIT_BAL<=300000,"less than 300K",
    ifelse(LIMIT_BAL<=400000,"less than 400K",
    ifelse(LIMIT_BAL<=500000,"less than 500K","more than 500K" ))))),
    prop=n())%>%group_by(Limit_range)%>%
    summarize(probability_of_def_on_category=paste0(round(sum(def=="yes")/n()*100,2),"%"),Proportion_of_total_defaults=paste0(round(sum(def=="yes")/totaldefault*100,2),"%"),
      n=n())%>%kable()
  
  ###defaulters by PAY_n (status of payment in previous month) 
  p1<-creditcard%>%mutate(n=n())%>%ggplot(aes(PAY_1,n,fill=def))+
    geom_bar(position="fill", stat="identity")+coord_flip()
  p2<-creditcard%>%mutate(n=n())%>%ggplot(aes(PAY_2,n,fill=def))+
    geom_bar(position="fill", stat="identity")+coord_flip()
  p3<-creditcard%>%mutate(n=n())%>%ggplot(aes(PAY_3,n,fill=def))+
    geom_bar(position="fill", stat="identity")+coord_flip()
  p4<-creditcard%>%mutate(n=n())%>%ggplot(aes(PAY_4,n,fill=def))+
    geom_bar(position="fill", stat="identity")+coord_flip()
  p5<-creditcard%>%mutate(n=n())%>%ggplot(aes(PAY_5,n,fill=def))+
    geom_bar(position="fill", stat="identity")+coord_flip()
  p6<-creditcard%>%mutate(n=n())%>%ggplot(aes(PAY_6,n,fill=def))+
    geom_bar(position="fill", stat="identity")+coord_flip()
  grid.arrange(p1,p2,p3,p4,p5,p6, ncol = 3)
  
  ###Defaulters by PAY_AMTN (amount payed on previous months)
  am1<-creditcard%>%ggplot(aes(PAY_AMT1,fill=def))+
    geom_histogram()+scale_x_log10()
  am2<-creditcard%>%ggplot(aes(PAY_AMT2,fill=def))+
    geom_histogram()+scale_x_log10()
  am3<-creditcard%>%ggplot(aes(PAY_AMT3,fill=def))+
    geom_histogram()+scale_x_log10()
  am4<-creditcard%>%ggplot(aes(PAY_AMT4,fill=def))+
    geom_histogram()+scale_x_log10()
  am5<-creditcard%>%ggplot(aes(PAY_AMT5,fill=def))+
    geom_histogram()+scale_x_log10()
  am6<-creditcard%>%ggplot(aes(PAY_AMT6,fill=def))+
    geom_histogram()+scale_x_log10()
  grid.arrange(am1,am2,am3,am4,am5,am6, ncol = 3)
  
  ### Defaulters by BILL_AMT1-BILL_AMT6 (bill of previous 6 months)
  
  
  bam1<-creditcard%>%ggplot(aes(BILL_AMT1,fill=def))+
    geom_histogram()+scale_x_log10()
  bam2<-creditcard%>%ggplot(aes(BILL_AMT2,fill=def))+
    geom_histogram()+scale_x_log10()
  bam3<-creditcard%>%ggplot(aes(BILL_AMT3,fill=def))+
    geom_histogram()+scale_x_log10()
  bam4<-creditcard%>%ggplot(aes(BILL_AMT4,fill=def))+
    geom_histogram()+scale_x_log10()
  bam5<-creditcard%>%ggplot(aes(BILL_AMT5,fill=def))+
    geom_histogram()+scale_x_log10()
  bam6<-creditcard%>%ggplot(aes(BILL_AMT6,fill=def))+
    geom_histogram()+scale_x_log10()
  grid.arrange(bam1,bam2,bam3,bam4,bam5,bam6, ncol = 3)
  
  
  ##Mean Bill (on previous months)
  lapply(creditcard[,13:18],mean)
  ##Median Bill (on previous months)
  lapply(creditcard[,13:18],median)
  ##Range Bill (on previous months)
  lapply(creditcard[,13:18],range)
  
  ## 3-Results
  
  ##Convert as factor output variable
  creditcard$def[creditcard$def=="yes"]<-1
  creditcard$def[creditcard$def=="no"]<-0
  creditcard$def<-as.factor(creditcard$def)
  
  ### To Simplify calculations I will take just most relevant variables. PAY_1,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6 and LIMIT_BAL
  filtered<-creditcard%>%select(PAY_1,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6,LIMIT_BAL,def)
  class(filtered)
  ##Scale matrix of predictors
  Mean_x<- sweep(as.matrix(filtered[,-14]), 2, colMeans(as.matrix(filtered[,-14])))
  x_scaled <- sweep(Mean_x, 2, colSds(as.matrix(filtered[,-14])), FUN = "/")
  set.seed(1, sample.kind="Rounding")
  ##Divide dataset in training and test sets
  test_index <- createDataPartition(y = filtered$def, times = 1, p = 0.25, list = FALSE)
  train_x<-x_scaled[-test_index,]
  train_y<-filtered$def[-test_index]
  test_x<-x_scaled[test_index,]
  test_y<-filtered$def[test_index]
  
  ###--------------------------------------------------------------------
  ### Model 0: Random Guessing
  ###--------------------------------------------------------------------
  
  ##To use as reference we are going to predict defaulter randomly
  set.seed(1974,sample.kind="Rounding")
  random_preds<-as.factor(sample(c(0,1),7500,replace=TRUE))
  ##Calculate confusion matrix and f1 Score
  cm_random<-confusionMatrix(random_preds, test_y)
  f1_random<-F_meas(data = random_preds, reference = factor(test_y))
  ##Save parameters
  model_0<-data.frame(Method= "random", 
                      Accuracy= cm_random$overall[["Accuracy"]],
                      F1 = f1_random, 
                      Sensitivity=cm_random$byClass[[1]],
                      Specificity=cm_random$byClass[[2]])
  model_0%>%kable()
  
  
  ###--------------------------------------------------------------------
  ### Model 1: Logistic regression
  ###--------------------------------------------------------------------
  
  ##train model
  train_glm<-train(train_x,train_y,method = "glm")
  ##predict outputs
  glm_preds<-predict(train_glm, test_x, type = "raw")
  ##Calculate confusion matrix and f1 Score
  cm_glm<-confusionMatrix(glm_preds, test_y)
  f1_glm<-F_meas(data = glm_preds, reference = factor(test_y))
  ##Save parameters
  model_1<-data.frame(Method= "glm", Accuracy= cm_glm$overall[["Accuracy"]],F1 = f1_glm, Sensitivity=cm_glm$byClass[[1]],Specificity=cm_glm$byClass[[2]])
  RESULTS<-bind_rows(model_0,model_1)
  RESULTS%>%kable()
  ###--------------------------------------------------------------------
  ### Model 2: Classification (decision) trees
  ###--------------------------------------------------------------------
  set.seed(1974, sample.kind="Rounding")
  ##train model
  train_rpart <- train(train_x,train_y,method = "rpart",
                       tuneGrid = data.frame(cp = seq(0.0, 0.01, len = 30)))
  ##Plot tuning CP to see where is optimum
  plot(train_rpart)
  ##Plot the tree
  plot(train_rpart$finalModel,margin=0.1)
  text(train_rpart$finalModel,cex=0.75)
  ##predict output
  rpart_preds <- predict(train_rpart, test_x)
  ##Calculate confusion matrix and f1 Score
  cm_rpart<-confusionMatrix(rpart_preds,test_y)
  f1_rpart<-F_meas(data = rpart_preds, reference = factor(test_y))
  ##Save parameters
  model_2<-data.frame(Method= "rpart", Accuracy= cm_rpart$overall[["Accuracy"]],F1 = f1_rpart, Sensitivity=cm_rpart$byClass[[1]],Specificity=cm_rpart$byClass[[2]])
  RESULTS<-bind_rows(model_0,model_1,model_2)
  RESULTS%>%kable()
  
  ###--------------------------------------------------------------------
  ## MODEL 3 Random Forest
  ###--------------------------------------------------------------------
  set.seed(19, sample.kind="Rounding")
  ##train model
  train_rf <- train(train_x,train_y,method = "rf",ntree=20,importance=TRUE,
                    tuneGrid = (data.frame(mtry = c(1,2,3))))
  ##Plot mtry to find optimum
  plot(train_rf)
  ##Variable importance
  print(varImp(train_rf,scale=T))
  #Predict output
  rf_preds <- predict(train_rf$finalModel, test_x)
  ##Calculate confusion matrix and f1 Score
  cm_rf<-confusionMatrix(rf_preds,test_y)
  f1_rf<-F_meas(data = rf_preds, reference = factor(test_y))
  ##Save parameters
  model_3<-data.frame(Method= "rf", Accuracy= cm_rf$overall[["Accuracy"]],F1 = f1_rf, Sensitivity=cm_rf$byClass[[1]],Specificity=cm_rf$byClass[[2]])
  RESULTS<-bind_rows(model_0,model_1,model_2,model_3)
  RESULTS%>%kable()
  
  ###--------------------------------------------------------------------
  ## MODEL 4 LDA
  ###--------------------------------------------------------------------
  
  ##train model
  train_lda<-train(train_x,train_y, method = "lda")
  #Predict output
  lda_preds <- predict(train_lda, test_x)
  ##Calculate confusion matrix and f1 Score
  cm_lda<-confusionMatrix(lda_preds,test_y)
  f1_lda<-F_meas(data = lda_preds, reference = factor(test_y))
  ##Save parameters
  model_4<-data.frame(Method= "lda", Accuracy= cm_lda$overall[["Accuracy"]],F1 = f1_lda, Sensitivity=cm_lda$byClass[[1]],Specificity=cm_lda$byClass[[2]])
  RESULTS<-bind_rows(model_0,model_1,model_2,model_3,model_4)
  RESULTS%>%kable()
  
  ###--------------------------------------------------------------------
  ## MODEL 5 Loess
  ###--------------------------------------------------------------------
  
  ##train model
  train_gam<-train(train_x,train_y, method = "gamLoess")
  #Predict output
  gam_preds <- predict(train_gam, test_x)
  ##Calculate confusion matrix and f1 Score
  cm_gam<-confusionMatrix(gam_preds,test_y)
  f1_gam<-F_meas(data = gam_preds, reference = factor(test_y))
  ##Save parameters
  model_5<-data.frame(Method= "gamLoess", Accuracy= cm_gam$overall[["Accuracy"]],F1 = f1_gam, Sensitivity=cm_gam$byClass[[1]],Specificity=cm_gam$byClass[[2]])
  RESULTS<-bind_rows(model_0,model_1,model_2,model_3,model_4,model_5)
  RESULTS%>%kable()
  
  ###--------------------------------------------------------------------
  ## MODEL 6 KNN
  ###--------------------------------------------------------------------
  set.seed(85, sample.kind="Rounding")
  ##train model
  train_knn <- train(train_x, train_y,method = "knn",tuneGrid = data.frame(k = seq(59, 62, 1)))
  ##plot accuracy vs K to see optimum numbers of neighbors
  train_knn$results %>% 
    ggplot(aes(x = k, y = Accuracy)) +
    geom_line() +
    geom_point() 
  #Predict output
  knn_preds <- predict(train_knn, test_x)
  ##Calculate confusion matrix and f1 Score
  cm_knn<-confusionMatrix(knn_preds,test_y)
  f1_knn<-F_meas(data = knn_preds, reference = factor(test_y))
  ##Save parameters
  model_6<-data.frame(Method= "knn", Accuracy= cm_knn$overall[["Accuracy"]],F1 = f1_knn, Sensitivity=cm_knn$byClass[[1]],Specificity=cm_knn$byClass[[2]])
  RESULTS<-bind_rows(model_0,model_1,model_2,model_3,model_4,model_5,model_6)
  RESULTS%>%kable()
  
  
  ###--------------------------------------------------------------------
  ## MODEL 7 gbm 
  ###--------------------------------------------------------------------
  
  set.seed(85, sample.kind="Rounding")
  ##train model
  #I tried 
  #caretGrid <- expand.grid(interaction.depth=c(1, 3, 5), 
  #                   n.trees = c(100,150),
  #                  shrinkage=c(0,001,0,01,0.025, 0.05, 0.1),
  #                   n.minobsinnode=10)
  ##   n.trees interaction.depth shrinkage n.minobsinnode
  ## 6     200                 5      0.025             10
  
  #i will use best tune i got to reduce calculation time 
  
  caretGrid <- expand.grid(interaction.depth=5, 
                           n.trees = 150,
                           shrinkage=0.025,
                           n.minobsinnode=10)
  
  
  
  train_gbm <- train(train_x, train_y,method = "gbm",tuneGrid=caretGrid)
  
  #Predict output
  gbm_preds <- predict(train_gbm, test_x)
  ##Calculate confusion matrix and f1 Score
  cm_gbm<-confusionMatrix(gbm_preds,test_y)
  f1_gbm<-F_meas(data = gbm_preds, reference = factor(test_y))
  ##Save parameters
  model_7<-data.frame(Method= "gbm", 
                      Accuracy= cm_gbm$overall[["Accuracy"]],
                      F1 = f1_gbm, 
                      Sensitivity=cm_gbm$byClass[[1]],
                      Specificity=cm_gbm$byClass[[2]])
  
  
  RESULTS<-bind_rows(model_0,model_1,model_2,model_3,model_4,model_5,model_6,model_7)
  RESULTS%>%kable()
  
  
  
  ###--------------------------------------------------------------------
  ## MODEL 8 Ensamble 
  ###--------------------------------------------------------------------
  ##Create ensamble matrix with predictions from 1 to 7
  ensamble<-data.frame(glm_preds,rpart_preds,rf_preds,
                       lda_preds,gam_preds,knn_preds,gbm_preds)
  ##Predict with ensamble
  ens_preds<-test_y
  for (i in 1:length(ens_preds)){
    ens_preds[i]<-ifelse(sum(ensamble[i,]==1)>=4,1,0)
  }
  ##Calculate confusion matrix and f1 Score
  cm_ens<-confusionMatrix(ens_preds,test_y)
  f1_ens<-F_meas(data = ens_preds, reference = factor(test_y))
  ##Save parameters
  model_8<-data.frame(Method= "ens", 
                      Accuracy= cm_ens$overall[["Accuracy"]],
                      F1 = f1_ens, 
                      Sensitivity=cm_ens$byClass[[1]],
                      Specificity=cm_ens$byClass[[2]])
  RESULTS<-bind_rows(model_0,model_1,model_2,model_3,
                     model_4,model_5,model_6,model_7,model_8)
  RESULTS%>%kable()
  
  
  
  ###--------------------------------------------------------------------
  ## Conclusion
  ###--------------------------------------------------------------------
  
  ##Of the 7 models analyzed GBM is the one with higher accuracy and 
  #F1 score
  RESULTS$Method[which.max(RESULTS$Accuracy)]
  max(RESULTS$Accuracy)
  RESULTS$Method[which.max(RESULTS$F1)]
  max(RESULTS$F1)
  print(varImp(train_gbm,scale=T))