
library(randomForest)
library(caret)

path='C:/Users/bryan/OneDrive - Universidad Nacional de Colombia/MAESTRIA/TESIS/DOCUMENTO FINAL/'
setwd(path)

#Paralelizacion

library(doParallel)
library(foreach)

#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer

X <- read.csv('data.csv',header=TRUE,sep=",")
df <- X[,!(names(X) %in% c("X0","eta","logit_eta","p"))]
df$y <- as.factor(df$y)
df$X4 <- as.factor(df$X4)
df$I1 <- as.factor(df$I1)
df$I2 <- as.factor(df$I2)
df$I3 <- as.factor(df$I3)
df$I4 <- as.factor(df$I4)
df$I5 <- as.factor(df$I5)
df$I6 <- as.factor(df$I6)

levels(df$y) <- c("no","si")
levels(df$X4) <- c("no","si")
levels(df$I1) <- c("no","si")
levels(df$I2) <- c("no","si")
levels(df$I3) <- c("no","si")
levels(df$I4) <- c("no","si")
levels(df$I5) <- c("no","si")
levels(df$I6) <- c("no","si")

trainIndex <- createDataPartition(df$y,p=0.9,list=FALSE)
train <- df[trainIndex,]
test <- df[-trainIndex,]


#Random Forest

registerDoParallel(cl)

model.rf <- randomForest(y~.,data=train,proximity=FALSE
                         ,trControl = trainControl(method = "cv"))


saveRDS(model.rf, "Scripts/model_rf.rds")


#knn

model.knn <- caret::train(y~.,
                          method="knn",
                          data=train,trControl = trainControl(method = "cv"))

saveRDS(model.knn, "Scripts/model_knn.rds")

#Knn Boot

model.knn_boot <- caret::train(y~.,method="knn",data=train,
                               trControl = trainControl(method = "boot",number=10))

saveRDS(model.knn_boot, "Scripts/model_knn_boot.rds")


#SVM con Kernel Lineal

model.svmL <- caret::train(y~.,method="svmLinear",
                              data=train,
                           trControl = trainControl(method = "cv",
                                              classProbs =  TRUE))
saveRDS(model.svmL, "Scripts/model_svml.rds")

stopCluster(cl)
  

#################################
######Cargado de modelos#########
#################################

model.knn = readRDS("Scripts/model_knn.rds")
model.knn_boot = readRDS("Scripts/model_knn_boot.rds")
model.rf = readRDS("Scripts/model_rf.rds")
model.svmL = readRDS("Scripts/model_svml.rds")

df_test <- df[,-c(11,12)]

y.rf <- predict(model.rf,df_test,type='prob')
y.knn <- predict(model.knn,df_test,type='prob')
y.knn_boot <- predict(model.knn_boot,df_test,type='prob')
y.svm <- predict(model.svmL,df_test,type='prob')

train.rf <- predict(model.rf,train[,-11],type='prob')
test.rf <- predict(model.rf,test[,-11],type='prob')

train.knn <- predict(model.knn,train[,-11],type='prob')
test.knn <- predict(model.knn,test[,-11],type='prob')

train.knn_boot <- predict(model.knn_boot,train[,-11],type='prob')
test.knn_boot <- predict(model.knn_boot,test[,-11],type='prob')

train.SVML <- predict(model.svmL,train[,-11],type='prob')
test.SVML <- predict(model.svmL,test[,-11],type='prob')


y.test = test$y
levels(y.test) <- c(0,1)

y.train = train$y
levels(y.train) <- c(0,1)

y = df$y
levels(y) <- c(0,1)


results.train <- data.frame(
  y = y,
  y_rf = y.rf[,2],
  y_knn = y.knn[,2],
  y_knn_boot = y.knn_boot[,2],
  y_svm = y.svm[,2]
)

results.train$y = y

write.csv(results.train,'train_predict.csv',row.names = FALSE)
results.train = read.csv("train_predict.csv")


library(ROCit)

measures <- function(x){
  rocit_obj <- rocit(score=x,class=results.train$y)
  measures_obj <-  measureit(rocit_obj,
                                        measure=c("ACC", "SENS", "FSCR"))
  df <- data.frame(ACC = measures_obj$ACC,
                   SENS = measures_obj$SENS,
                   FSCR = measures_obj$FSCR)
  max_acc <- max(df$ACC)
  return(list(df,df[df$ACC == max_acc,]))
}

a <-measures(results.train$y_rf)

calcula_matrix_confusion <- function(x,p){
  y.hat = factor(ifelse(x > p,1,0))
  y = factor(results.train$y)
  levels(y) <- c(0,1)
  
    return(confusionMatrix(y,y.hat))
}

calcula_matrix_confusion(results.train$y_rf,0.668)
calcula_matrix_confusion(results.train$y_knn,0.42857)
calcula_matrix_confusion(results.train$y_knn_boot,0.333)
calcula_matrix_confusion(results.train$y_svm,0.00767)
