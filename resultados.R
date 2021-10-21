
path='C:/Users/bryan/OneDrive - Universidad Nacional de Colombia/MAESTRIA/TESIS/DOCUMENTO FINAL/tesis/'
fig.path = 'C:/Users/bryan/OneDrive - Universidad Nacional de Colombia/MAESTRIA/TESIS/DOCUMENTO FINAL/tesis/figs'
setwd(path)

results.train = read.csv("train_predict.csv")
amortiza =read.csv('amortiza.csv')

print.fig <-function(plot,path=fig.path,name,w=500,h=350){
  path.name <- paste(path,name,sep="/")
  png(path.name,width = w, height = h)
  print(plot)
  dev.off()
}


library(pROC)
library(ROCit)

measures <- function(x){
  rocit_obj <- rocit(score=x,class=results.train$y)
  measures_obj <-  measureit(rocit_obj,
                             measure=c("ACC", "SENS", "FSCR"))
  df <- data.frame(p0 = measures_obj$Cutoff,
                   ACC = measures_obj$ACC,
                   SENS = measures_obj$SENS,
                   FSCR = measures_obj$FSCR)
  max_acc <- max(df$ACC)
  return(list(df,df[df$ACC == max_acc,]))
}

y = factor(results.train$y)
levels(y)<-c(0,1)

auc.model <- function(x){
  
  roc.model <- pROC::roc(y,x)
  return(pROC::auc(roc.model)[[1]])
  
}

AUC.TABLE<- data.frame(RF=auc.model(results.train$y_rf),
                       KNN=auc.model(results.train$y_knn),
                       KNNB=auc.model(results.train$y_knn_boot),
                       SVM=auc.model(results.train$y_svm))

xtable::xtable(AUC.TABLE,digits=5)

m.rf <- measures(results.train$y_rf)
m.knn<- measures(results.train$y_knn)
m.knn_b <- measures(results.train$y_knn_boot)
m.svm <- measures(results.train$y_svm)


metricas <- data.frame( Modelo = c("RF", "KNN", "b-NN","SVM"),
                        AUC = c(auc.model(results.train$y_rf),
                                auc.model(results.train$y_knn),
                                auc.model(results.train$y_knn_boot),
                                auc.model(results.train$y_svm)),
                        P_0 = c(m.rf[[2]]$p0, m.knn[[2]]$p0, 
                                m.knn_b[[2]]$p0, m.svm[[2]]$p0[1]),
                        ACC = c(m.rf[[2]]$ACC, m.knn[[2]]$ACC,
                                m.knn_b[[2]]$ACC, m.svm[[2]]$ACC[1]),
                        SENS = c(m.rf[[2]]$SENS, m.knn[[2]]$SENS, 
                                 m.knn_b[[2]]$SENS, m.svm[[2]]$SENS[1]),
                        FSCR = c(m.rf[[2]]$FSCR, m.knn[[2]]$FSCR, 
                                 m.knn_b[[2]]$FSCR, m.svm[[2]]$FSCR[1]))

#Metricas
xtable(metricas,digits = 5)

calcula_matrix_confusion <- function(x,p){
  y.hat = factor(ifelse(x > p,1,0))
  y = factor(results.train$y)
  levels(y) <- c(0,1)
  
  return(confusionMatrix(y,y.hat))
}

#Matriz de confusion
calcula_matrix_confusion(results.train$y_rf,0.668)
calcula_matrix_confusion(results.train$y_knn,0.42857)
calcula_matrix_confusion(results.train$y_knn_boot,0.333)
calcula_matrix_confusion(results.train$y_svm,0.00767)

#Estimacion S
library(actuar)
library(ggplot2)
LPcumul = function(lj,Fj){
  k1 = sum(lj*Fj)
  k2 = sum(lj*Fj^2)
  k3 = sum(lj*Fj^3) 
  k4 = sum(lj*Fj^4)
  g1 = k3/k2^(3/2)
  g2 = k4/k2^2
  kS = c(k1,k2,g1,g2)
  names(kS)=c("k1","k2","g1","g2")
  return(kS)}

np.aprox = function(x,Skj){
  z = (x-Skj[1])/sqrt(Skj[2])
  m = 9/Skj[3]^2+6*z/Skj[3]+1   
  print(m)
  z1 = ifelse(m<0,0,
              -3/Skj[3]+sqrt(m))
  
  p = dnorm(z1,0,1)
  return(p)
}

np.aprox.pdf = function(x,Skj){
  z = (x-Skj[1])/sqrt(Skj[2])
  z1 = ifelse(9/Skj[3]^2+6*z/Skj[3]+1 < 0,0,
              -3/Skj[3]+sqrt(9/Skj[3]^2+6*z/Skj[3]+1))
  p1 = 3*sqrt(2)*exp(-z1^2/2)
  p2 = ifelse(9/Skj[3]^2+6*z/Skj[3]+1 < 0,0,
              sqrt(9/Skj[3]^2+6*z/Skj[3]+1))
  p = p1/(2*sqrt(pi)*Skj[3]*sqrt(Skj[2]))
  return(p)
}

s.plot.function <- function(x){
  
  S <- data.frame(lj = x, 
                  Fj = amortiza$exposicion)
  S$Fj <- S$Fj /1.0e+06
  
  Skj = LPcumul(S$lj,S$Fj)
  
  Fs = aggregateDist("npower",moments = Skj)
  var.x <- actuar::VaR(Fs)
  
  f = function(x){np.aprox(x,Skj)}
  C = integrate(f, lower = 0, upper = 10000, 
                stop.on.error = FALSE)$value
  
  xe = seq(0,Skj[1]+8*sqrt(Skj[2]))
  fGa.np=np.aprox.pdf(xe,Skj)
  S.df <- data.frame(y=fGa.np/C,x=xe)
  s.plot <- ggplot(S.df,aes(x=x,y=y))+
    geom_area(colour="black",alpha=0.6,lwd=1)
  return(list(s.plot,var.x))
  
}



var.calculate <- function(x,r){
  
  var.vector <- double(length = length(r))
  j <- 1
  for (i in r){
    Y.hat <- ifelse(x>i,1,0)
    var.vector[j] <- s.plot.function(Y.hat)[[2]][3]
    j <- j + 1
  }
  return(var.vector)
}


########SVM##########
Y.svm <- ifelse(results.train$y_svm>0.53416,1,0)
S.svm <- s.plot.function(Y.svm)

S.svm.plot <- S.svm[[1]]+  
  xlim(c(375,440))
print.fig(S.svm.plot,name="S_svm.png")



cutoff.range <- seq(0.01,0.99,0.01)
var.SVM <- double(length(cutoff.range))
j <- 1
for (i in cutoff.range){
  Y.hat <- ifelse(results.train$y_svm>i,1,0)
  var.SVM[j] <- s.plot.function(Y.hat)[[2]][3]
  j <- j + 1
}

ggplot(data = data.frame("CutOff"=cutoff.range,"VaR"=var.SVM),aes(x=CutOff,y=VaR))+
  geom_point(size=2,shape=3)+
  xlim(c(0,0.53416))
  

########RF##########
S.rf <- s.plot.function(results.train$y_rf)
S.rf.plot <- S.rf[[1]]+
  xlim(c(350,425))
print.fig(S.svm.plot,name="S_rf.png")

var.RF <- var.calculate(results.train$y_rf,cutoff.range)

ggplot(data = data.frame("CutOff"=cutoff.range,"VaR"=var.RF),aes(x=CutOff,y=VaR))+
  geom_point(size=2,shape=3)

########KNN##########
S.knn <- s.plot.function(results.train$y_knn)
S.knn.plot <- S.knn[[1]]+
  xlim(c(220,270))
print.fig(S.svm.plot,name="S_knn.png")

var.knn <- var.calculate(results.train$y_knn,cutoff.range)

ggplot(data = data.frame("CutOff"=cutoff.range,"VaR"=var.knn),aes(x=CutOff,y=VaR))+
  geom_point(size=2,shape=3)

########BNN##########
S.knn_b <- s.plot.function(results.train$y_knn_boot)
S.knn_b.plot <- S.knn_b[[1]]+
  xlim(c(220,270))
print.fig(S.svm.plot,name="S_bnn.png")

var.bnn <- var.calculate(results.train$y_knn_boot,cutoff.range)

ggplot(data = data.frame("CutOff"=cutoff.range,"VaR"=var.bnn),aes(x=CutOff,y=VaR))+
  geom_point(size=2,shape=3)
