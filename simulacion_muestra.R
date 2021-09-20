fig.path = 'C:/Users/bryan/OneDrive - Universidad Nacional de Colombia/MAESTRIA/TESIS/DOCUMENTO FINAL/figs'
setwd('C:/Users/bryan/OneDrive - Universidad Nacional de Colombia/MAESTRIA/TESIS/DOCUMENTO FINAL/')

library(truncdist)
library(markovchain)
library(ggplot2)
library(ggpubr)
library(egg)
library(xtable)

#TAMAÑO DE MUESTRA

N = 100000   

###IDENTIFICADOR
id = 1:N

## CLUSTERS
a=0.5
b=0.3
c=1-a-b
pesos = c(a,b,c)


print.fig <-function(plot,path,name,w,h){
  path.name <- paste(path,name,sep="/")
  png(path.name,width = w, height = h)
  print(plot)
  dev.off()
}

clst <- function(x, a = pesos[1],b = pesos[2]){
  
  if (x <= a) {
    temp = "1" #Riesgo 1
  }else if( (x > a) & (x <= a + b) ){
    temp = "2" #Riesgo 2
  }else if( x > a + b){
    temp = "3" #Riesgo 3
  }
  return(temp)

  }

create_cluster <- function(N){
  
  x = cbind ( runif(N) )

return( factor(apply(x, 1, clst)) )  
}


segmento = create_cluster(N)




pp_beta <- function(segmento,betas,min,max){
  seg <- as.integer(segmento)
  a <- betas[seg,1]
  b <- betas[seg,2]
  
  q.min <- pbeta(min,a,b,lower.tail=TRUE)
  q.max <- pbeta(max,a,b,lower.tail=TRUE)
  
  r <- truncdist::rtrunc(1,"beta",a=q.min,b=q.max,shape1=a,shape2=b)
  return(r)
  
}

beta.qtl <- function(N,a,b,min,max){
  q.min <- pbeta(min,a,b,lower.tail=TRUE)
  q.max <- pbeta(max,a,b,lower.tail=TRUE)
  
  r <- runif(N,q.min,q.max)
  
  return(qbeta(r,a,b))
  
}


k=300000

activos <- rgamma(N,shape=2,rate=2/k)

activos.D = density(activos)

beta.pasivos <- matrix(c(2,6,0.2,4,6,0.2,9,6,0.2),byrow=T,nrow=3)

pp_pasivos <- sapply(segmento,function(x) pp_beta(x,betas = beta.pasivos,min=0.1,max = 0.8))

pasivos <- activos * pp_pasivos
patrimonio <- activos - pasivos

pasivos.df <- data.frame(x=density(pp_pasivos)$x,y=density(pp_pasivos)$y,
                         y.1=density(rbeta(N,2,6))$y,x.1=density(rbeta(N,2,6))$x,
                         y.2=density(rbeta(N,4,6))$y,x.2=density(rbeta(N,4,6))$x,
                         y.3=density(rbeta(N,9,6))$y,x.3=density(rbeta(N,9,6))$x)


beta.ebitda <- matrix(c(2,5,0.3,1.5,5,0.3,1,5,0.3),byrow=T,nrow=3)

pp_ebitda <- sapply(segmento,function(x) pp_beta(x,betas=beta.ebitda,min=0.01,max=0.3))

ebitda.df <- data.frame(x=density(pp_ebitda)$x,y=density(pp_ebitda)$y,
                         y.1=density(rbeta(N,2,5))$y,x.1=density(rbeta(N,2,5))$x,
                         y.2=density(rbeta(N,1.5,5))$y,x.2=density(rbeta(N,1.5,5))$x,
                         y.3=density(rbeta(N,1,5))$y,x.3=density(rbeta(N,1,5))$x)


ebitda <- pp_ebitda * (patrimonio)
n.deuda <- pasivos / patrimonio
roa <- ebitda / activos
roe <- ebitda / patrimonio

data.ef = data.frame("activos"=activos, "pasivos" = pasivos, "patrimonio"=patrimonio,
                     "nivel de deuda" = n.deuda,"roa"=roa,"roe"=roe)
write.csv(data.ef,"estados_financieros.csv",row.names = FALSE)

data.ef = read.csv("estados_financieros.csv")


beta.exposicion <- matrix(c(6,4,0.8,4,4,0.8,2,4,0.8),byrow=T,nrow=3)
pp_exposicion <- sapply(segmento,function(x) pp_beta(x,betas=beta.exposicion,min = 0.2,max=0.8))
exposicion_init = pasivos * pp_exposicion

exposicion.df <- data.frame(x=density(pp_exposicion)$x,y=density(pp_exposicion)$y,
                         y.1=density(rbeta(N,6,4))$y,x.1=density(rbeta(N,6,4))$x,
                         y.2=density(rbeta(N,4,4))$y,x.2=density(rbeta(N,4,4))$x,
                         y.3=density(rbeta(N,2,4))$y,x.3=density(rbeta(N,2,4))$x)



rates = data.frame("X3" = seq(0.014,0.018,by=0.001), #
                   "X2" = seq(0.011,0.015,by=0.001), #
                   "X1" = seq(0.008,0.012,by=0.001)) #
rates.i = sapply(segmento,function(x) sample(rates[,paste("X",x,sep="")],1))


pago <- function(x,rate,n){
  
  pago_cuota = x * ( (rate * ( rate +1 ) ^ n) / ((1+rate)^n - 1 ) )
  
  return(pago_cuota)
}

plazos <- seq(12,60,by=12)

plazos.i <- sample(plazos,N,replace = T)
hist(plazos.i)

#Markov-Chain de dos estados 0 = Pago y 1 = No pago
States <- c("0","1")

P01.A <- c(0.05,0.2)
P01.B <- c(0.1,0.3)
P01.M <- c(0.2,0.4)

data.B = c( 1 - P01.A[1], P01.A[1], 1 - P01.A[2], P01.A[2]) # nivel de endeudamiento <= 0.5  BUENO
data.A = c( 1 - P01.B[1], P01.B[1], 1 - P01.B[2], P01.B[2]) # nivel de endeudamiento e (0.5,1] ACEPTABLE
data.M = c( 1 - P01.M[1], P01.M[1], 1 - P01.M[2], P01.M[2]) # nivel de endeudamiento > 1 MALO


matrix.pago <- function(x){
  require(markovchain)
  if(x <0.6){
    
    pagoMatrix <- matrix(data = data.B , byrow = T, nrow = 2,
                         dimnames = list(States, States))
    mcPago <- new("markovchain", states = States, byrow = T,
                   transitionMatrix = pagoMatrix, name = "pago")
  }else if(x<1){
    pagoMatrix <- matrix(data = data.A , byrow = T, nrow = 2,
                         dimnames = list(States, States))
    mcPago <- new("markovchain", states = States, byrow = T,
                   transitionMatrix = pagoMatrix, name = "pago")
  }else{
    pagoMatrix <- matrix(data = data.M , byrow = T, nrow = 2,
                         dimnames = list(States, States))
    mcPago <- new("markovchain", states = States, byrow = T,
                   transitionMatrix = pagoMatrix, name = "pago")
  }
  
  return(mcPago)
  
}




rate.m <- 0.027
rate.d <- (1+rate.m)^(1/30)-1


calculate_saldo <- function(exposicion,segmento,n.deuda,umbral=3){
  
  seg = segmento
  plazo = sample(plazos,1)
  rate_column = paste("X",seg,sep="")
  rate = sample(rates[,rate_column],1)
  mcPago <- matrix.pago(n.deuda)
  flag = as.integer(markovchain::rmarkovchain(n = plazo, 
                                 object = mcPago, t0 = "0"))
  
  cuota = pago(exposicion,rate,plazo)
  cuota_acum = 0
  amortizacion = data.frame(periodo = c(0)
                            ,saldo = c(exposicion)
                            ,abono = c(0)
                            ,interes = c(0)
                            ,cuota = c(0)
                            ,nopago = c(cuota_acum)
                            ,mora = c(0)
                            )
  k = 0

    for( i in 1:plazo){
    amortizacion_prev = amortizacion[amortizacion$periodo == (i-1)
                                     ,"saldo"] 
  
    if(flag[i] == 0){
      mora = 0
        if(i>=2){
          
          if(flag[(i-1)] == 1){
            dias.mora <- min(27,rgeom(1,0.4))
            # dias.mora <- min(27,rgeom(1,1/15))
            mora <- amortizacion_prev * ((1+rate.d)^(dias.mora)-1)
            cuota = pago(amortizacion_prev + cuota_acum + mora,rate,plazo - i+1)
            interes_prev = (amortizacion_prev + cuota_acum + mora) * rate
            abono_prev = cuota - interes_prev - cuota_acum 

            cuota_acum <- 0
            k <- k + 1
          }else{
            
            interes_prev = amortizacion_prev * rate
            abono_prev = cuota - interes_prev 
            cuota_acum <- 0
            k <- k + 1
          }
          
        }else{
         
          interes_prev = amortizacion_prev * rate
          abono_prev = cuota - interes_prev
          k <- k + 1
        }
      
    }else{
      
      mora = 0
      if(i>=2){
        
        if(flag[(i-1)] == 1){
          cuota = pago(amortizacion_prev,rate,plazo - i+1)
          interes_prev = amortizacion_prev * rate.m
          abono_prev = - interes_prev
          mora = amortizacion_prev * ( rate.m - rate )
        }else{
          interes_prev = amortizacion_prev * rate
          abono_prev = - interes_prev
          
        }
        
      }else{
        interes_prev = amortizacion_prev * rate
        abono_prev = - interes_prev
        
      }
      
      cuota_acum = cuota_acum - cuota
      
  
    }

    amortizacion = rbind( amortizacion,
                          c(i,
                            amortizacion_prev - abono_prev + mora,
                            abono_prev,
                            interes_prev,
                            cuota,
                            cuota_acum,
                            mora
                            ))
  
  }
  return(list(flag,amortizacion,plazo,rate))
}


calculo <- function(k){
  message("id is:", k)
  message("exposicion is:", exposicion_init[k])
  message("segmento is:", segmento[k])
  return(calculate_saldo(exposicion_init[k],
                         as.integer(segmento[k]),
                         n.deuda[k]
                                    ))
}

calculo(2)

calculo_data <- function(k,n=6,umbral = 3){
  message("id equal to:", k)
  message("exposicion equal to:", exposicion_init[k])
  message("segmento equal to:", segmento[k])
  list_tmp <- calculate_saldo(exposicion_init[k],
                         as.integer(segmento[k]),
                         n.deuda[k])
  plazo <-list_tmp[[3]]
  t0<- sample(n:(plazo-n),1)
  tf<- t0+n-1
  flag.t <- list_tmp[[1]]
  flag.k <- flag.t[t0:tf]
  
  k <-0
  exposicion_final <-  list_tmp[[2]][tf,"saldo"]
  
  flag.p <- 0
  for( j in 1:tf ){

    if(j>=umbral){
      no.pagos <- sum( tail(flag.t[1:j],umbral) )
      if( no.pagos == umbral ){
        flag.p <- 1
        k <- j
        flag.k <- rep(1,6)
        exposicion_final <-  list_tmp[[2]][k,"saldo"]
        break
      }
    }
  }

    
  return(list(flag.k,exposicion_final,flag.p,k,t0))
  
}

calculo_data(1)

library(foreach)
library(doParallel)


cores <- detectCores()
cl <- makeCluster(cores[1]-1) 
registerDoParallel(cl)


lista_data <-foreach(k=1:N) %dopar% {
  tempList = calculo_data(k,umbral = 4) 
  tempList 
}


amortiza <- data.frame(matrix(unlist(lapply(lista_data,unlist)), ncol=10, byrow=TRUE))
names(amortiza) <- c(paste("P",1:6,sep=""),
                     "exposicion","flag_npago","pagos","t0")


write.csv(amortiza,'amortiza.csv',row.names = FALSE)
amortiza =read.csv('amortiza.csv')

b0 <- -5
b1 <- 0.25 #Nivel de endeudamiento
b2 <- -3 #ROE
b3 <- -3 #ROA
b4 <- 7 #FACTOR K NO PAGOS
i1 <- 0.3
i2 <- 0.3
i3 <- 0.5
i4 <- 0.5
i5 <- 0.8
i6 <- 0.8


betas = rbind(b0,b1,b2,b3,b4,
              i1,i2,i3,i4,i5,i6)
betas

X = data.frame(X0=1,X1=n.deuda,X2=roe,X3=roa,X4=amortiza$flag_npago,
               I1=amortiza$P1,I2=amortiza$P2,I3=amortiza$P3,I4=amortiza$P4,
               I5=amortiza$P5,I6=amortiza$P6)

eta <-  (as.matrix(X) %*% betas)
X$eta <- eta
hist(eta)

logit_eta <- 1 / (1 + exp(  -(eta) ) )
hist(logit_eta)
length(logit_eta[logit_eta>0.4 & logit_eta<0.9])
boxplot(logit_eta~segmento)
X$logit_eta <- logit_eta
fi = 0.5
  par(mfrow=c(5,5))
  dev.off()
  p.v <- seq(0,1,by=0.1)
  p.lines = c(0.2,0.4,0.5,0.6,0.8)
  for (i in p.v){
    plot(density(rbeta(N,i,fi)),main = paste("mu equal to ",i,sep=""))
    abline(v=qbeta(p.lines,i,fi),col=2,lty=2)
  }

prob=double(N)

for (i in 1:N) {

  prob[i] = rbeta(1,shape1 = logit_eta[i],shape2=fi)

}

X$p <- prob
X$segmento <- segmento
boxplot(prob~segmento)

abline(h=0.6,col=2,lty=2)
hist(prob)
tapply(prob,segmento,mean)

y = ifelse( prob > 0.7, 1, 0)
X$y <- y


write.csv(X,file="data.csv",row.names = FALSE)
