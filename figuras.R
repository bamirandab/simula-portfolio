fig.path = 'C:/Users/bryan/OneDrive - Universidad Nacional de Colombia/MAESTRIA/TESIS/DOCUMENTO FINAL/figs'
setwd(fig.path)

library(ggplot2)

print.fig <-function(plot,path=fig.path,name,w=500,h=350){
  path.name <- paste(path,name,sep="/")
  png(path.name,width = w, height = h)
  print(plot)
  dev.off()
}


#cluster 
cluster.plot<-ggplot(data=data.frame('Cluster'=segmento),aes(x=Cluster,col=Cluster))+
  geom_bar(fill='white')+  
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=20,face="bold"))+
  geom_text(stat='count', aes(x = Cluster, label = ..count..), vjust = -1)+
  xlab('')+ylab('Frecuencia')+ylim(c(0,55000))


print.fig(cluster.plot,fig.path,'cluster.png',w=500,h=350)


#activos 
activos.hist <- ggplot(data = data.frame('x'=activos),aes(x=x))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#1239BA")+ 
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=20,face="bold"))+
  xlab('')+
  ylab('Frecuencia')

activos.boxplot <- ggplot(data = data.frame('x'=activos),aes(x=x))+
  geom_boxplot(position = "identity")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=20,face="bold"))+
  geom_point(aes(x=k,y=0),col=2)+
  xlab('A')+ylab('')

activos.plot<-egg::ggarrange(activos.hist+rremove("x.text"), activos.boxplot+rremove("y.text"), 
                             heights = 2:1
)

print.fig(activos.plot,fig.path,'activos.png',w=500,h=350)


#pasivos y patrimonio 

pp_pasivos.plot <- ggplot(data=pasivos.df) +
  geom_line(aes(x=x,y=y,colour="black"),size=1)+
  geom_line(aes(x=x.1,y=y.1,colour='darkred'),size=1)+
  geom_line(aes(x=x.2,y=y.2,colour='steelblue'),size=1)+
  geom_line(aes(x=x.3,y=y.3,colour='darkgreen'),size=1)+
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.text.align=0,
        axis.title=element_text(size=15,face="bold"))+ylab("Frecuencia")+xlab('')+
  scale_color_discrete(name = "Variable",
                       labels = c(expression(paste(L^"*")),
                                  expression(paste(L[3]^"*","  con  ",alpha," = 9","  ",beta," = 6",sep="")),
                                  expression(paste(L[1]^"*","  con  ",alpha," = 2","  ",beta," = 6",sep="")),
                                  expression(paste(L[2]^"*","  con  ",alpha," = 4","  ",beta," = 6",sep=""))))+
  geom_vline(xintercept =c(0.1,0.8),linetype="dotted",size=1)

pasivos.line.plot <- ggplot(data=data.frame(x=density(pasivos)$x,y=density(pasivos)$y),aes(x=x,y=y))+
  geom_line(size=1)+theme(axis.text=element_text(size=10),
                          axis.title=element_text(size=15,face="bold"))+
  ylab("Densidad")+xlab("Pasivos (L)")

patrimonio.line.plot <- ggplot(data=data.frame(x=density(patrimonio)$x,y=density(patrimonio)$y),aes(x=x,y=y))+
  geom_line(size=1)+theme(axis.text=element_text(size=10),
                          axis.title=element_text(size=15,face="bold"))+
  ylab("Densidad")+xlab("patrimonio (E)")

pasivos.plot<-egg::ggarrange(pp_pasivos.plot, pasivos.line.plot,nrow=1,ncol=2)

print.fig(pasivos.plot,fig.path,'pasivos.png',w=1000,h=350)
print.fig(patrimonio.line.plot,fig.path,'patrimonio.png',w=500,h=350)


#ebitda 
ebitda.d.plot <- ggplot(data=ebitda.df) +
  geom_line(aes(x=x,y=y,colour="black"),size=1)+
  geom_line(aes(x=x.1,y=y.1,colour='darkred'),size=1)+
  geom_line(aes(x=x.2,y=y.2,colour='steelblue'),size=1)+
  geom_line(aes(x=x.3,y=y.3,colour='darkgreen'),size=1)+
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.text.align=0,
        axis.title=element_text(size=15,face="bold"))+ylab("Frecuencia")+xlab('')+
  scale_color_discrete(name = "Variable",
                       labels = c(expression(V^"*"),
                                  expression(paste(V[3]^"*","  con  ",alpha," = 1","  ",beta," = 5",sep="")),
                                  expression(paste(V[1]^"*","  con  ",alpha," = 2","  ",beta," = 5",sep="")),
                                  expression(paste(V[2]^"*","  con  ",alpha," = 1.5","  ",beta," = 5",sep=""))))+
  geom_vline(xintercept =c(0.01,0.3),linetype="dotted",size=1)






ebitda.l.plot <- ggplot(data=data.frame(x=density(ebitda)$x,y=density(ebitda)$y),aes(x=x,y=y))+
  geom_line(size=1)+theme(axis.text=element_text(size=10),
                          axis.title=element_text(size=15,face="bold"))+
  ylab("Densidad")+xlab("Utilidad neta (V)")

ebitda.plot<-egg::ggarrange(ebitda.d.plot, ebitda.l.plot,nrow=1,ncol=2)

print.fig(ebitda.plot,fig.path,'utilidad.png',w=1000,h=350)

#exposición 
exp.d.plot <- ggplot(data=exposicion.df) +
  geom_line(aes(x=x,y=y,colour="black"),size=1)+
  geom_line(aes(x=x.1,y=y.1,colour='darkred'),size=1)+
  geom_line(aes(x=x.2,y=y.2,colour='steelblue'),size=1)+
  geom_line(aes(x=x.3,y=y.3,colour='darkgreen'),size=1)+
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.text.align=0,
        axis.title=element_text(size=15,face="bold"))+ylab("Frecuencia")+xlab('')+
  scale_color_discrete(name = "Variable",
                       labels = c(expression(EI^"*"),
                                  expression(paste(EI[3]^"*","  con  ",alpha," = 2","  ",beta," = 4",sep="")),
                                  expression(paste(EI[1]^"*","  con  ",alpha," = 6","  ",beta," = 4",sep="")),
                                  expression(paste(EI[2]^"*","  con  ",alpha," = 4","  ",beta," = 4",sep=""))))+
  geom_vline(xintercept =c(0.2,0.8),linetype="dotted",size=1)

exp.l.plot <- ggplot(data=data.frame(x=density(exposicion_init)$x,y=density(exposicion_init)$y),aes(x=x,y=y))+
  geom_line(size=1)+theme(axis.text=element_text(size=10),
                          axis.title=element_text(size=15,face="bold"))+
  ylab("Densidad")+xlab("Exposicion inicial EI")

exp.plot <- egg::ggarrange(exp.d.plot, exp.l.plot,nrow=1,ncol=2)

print.fig(exp.plot,fig.path,'exposicion_init.png',w=1000,h=350)

#tasas 
rates.barplot <- ggplot(data=data.frame(x=paste(as.character(rates.i*100),"%",sep='')),aes(x))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=10),
        axis.title=element_text(size=15,face="bold"))+
  xlab("r")

print.fig(rates.barplot,fig.path,'rates.png',w=500,h=350)

#flag
flag.barplot <- ggplot( data=data.frame(x=data.frame(x = X$X4 ) ),
                        aes( factor(x))) +  geom_bar()+
  theme(axis.text.x = element_text(angle = 90),
        axis.text=element_text(size=10),
        axis.title=element_text(size=15,face="bold")) + xlab("") + ylab("")

print.fig(flag.barplot,fig.path,'flag.png',w=500,h=350)


#Plot 6 pagos

I5.plot <-ggplot(data=df,aes(x=I1))+
        geom_bar()+xlab("I5") + ggtitle("(1)")+
  theme(plot.title = element_text(hjust = 0.5))
I4.plot <-ggplot(data=df,aes(x=I2))+
      geom_bar()+xlab("I4")+ ggtitle("(2)")+
  theme(plot.title = element_text(hjust = 0.5))
I3.plot <-ggplot(data=df,aes(x=I3))+
      geom_bar()+xlab("I3")+ ggtitle("(3)")+
  theme(plot.title = element_text(hjust = 0.5))
I2.plot <-ggplot(data=df,aes(x=I4))+
      geom_bar()+xlab("I2")+ ggtitle("(4)")+
  theme(plot.title = element_text(hjust = 0.5))
I1.plot <-ggplot(data=df,aes(x=I5))+
      geom_bar()+xlab("I1")+ ggtitle("(5)")+
  theme(plot.title = element_text(hjust = 0.5))
I0.plot <-ggplot(data=df,aes(x=I6))+
      geom_bar()+xlab("I0")+ ggtitle("(6)")+
  theme(plot.title = element_text(hjust = 0.5))


Pagos.plot <- egg::ggarrange(I5.plot,
                           I4.plot,
                           I3.plot,
                           I2.plot,
                           I1.plot,
                           I0.plot,widths=c(3,3))

print.fig(Pagos.plot,name="Pagos.png",w=700,h=500)


#ROC
library(pROC)
library(ROCR)

y.train = train$y
levels(y.train) <- c(0,1)

# Plot:
roc.rf <- prediction( y.rf[,2], y )
roc.knn <- prediction( y.knn[,2], y )
roc.knn_boot <- prediction( y.knn_boot[,2], y)
roc.svm <- prediction( y.svm[,2], y)

perf.rf <- performance( roc.rf, "tpr", "fpr" )
perf.knn <- performance(roc.knn, "tpr", "fpr")
perf.knn_boot <- performance(roc.knn_boot, "tpr", "fpr")
perf.svm <- performance(roc.svm, "tpr", "fpr")

win.graph(8,6,13)

roc.train.plot <-plot(perf.rf,main="Curva ROC",lwd=2,type="l")
grid(NULL, NULL)
plot(perf.knn, add=TRUE, col='red',lty=2,lwd=2)
plot(perf.knn_boot, add=TRUE, col='blue',lty=2,lwd=2)
plot(perf.svm, add=TRUE, col='green',lty=2,lwd=2)
lines(cbind(c(0,1),c(0,1)),col="gray")

legend("bottomright",legend=c("RandomForest", "KNN","KNN-Boot","SVM", "Linea de Oportunidad"),
       col=c("black","red","blue","green","gray"), lty=c(1,2,2,2,1),cex=0.8,lwd=c(2,2,2,2,1))

y.test = test$y
levels(y.test) <- c(0,1)

# Plot:
roc.rf <- prediction( test.rf[,2], y.test )
roc.knn <- prediction( test.knn[,2], y.test )
roc.knn_boot <- prediction( test.knn_boot[,2], y.test)
roc.svm <- prediction( test.SVML[,2], y.test)

perf.rf <- performance( roc.rf, "tpr", "fpr" )
perf.knn <- performance(roc.knn, "tpr", "fpr")
perf.knn_boot <- performance(roc.knn_boot, "tpr", "fpr")
perf.svm <- performance(roc.svm, "tpr", "fpr")

roc.test.plot <- plot(perf.rf,main="Curva ROC data de prueba",lwd=2)
plot(perf.knn, add=TRUE, col='red',lty=2,lwd=2)
plot(perf.knn_boot, add=TRUE, col='blue',lty=2,lwd=2)
plot(perf.svm, add=TRUE, col='green',lty=2,lwd=2)
legend("bottomright",legend=c("RF", "KNN","KNN-Boot","SVM"),
       col=c("black","red","blue","green"), lty=c(1,2,2,2),cex=0.8,lwd=2)
# Histograma y

hist(prob)
tapply(prob,segmento,mean)

p.plot <- ggplot2::ggplot(X,aes(x=p))+
  geom_histogram(bins=20)

print.fig(p.plot,name="p.png")





#Figura
levels( df$y ) <- c(0,1)
y.plot <- ggplot2::ggplot(df,aes(x=y))+
  geom_bar()

print.fig(y.plot,name="y.png")

y.plot.final <- egg::ggarrange(p.plot,
               y.plot,widths=c(2,1))

print.fig(y.plot.final,name="y_final.png",w=700,h=350)
