
library(gamlss)
library(Rlab)
library(markovchain)

#ESCRIBIR LA TESIS EN TIPO BOOK
#UTILIZAR EL ESTILO DE BIBLIOGRAFIA HARVARD

N = 100000   #TENTATIVO

###IDENTIFICADOR
id = 1:N

## CLUSTERES
a=0.5
b=0.4
c=1-a-b
pesos = c(a,b,c)

clst <- function(x, a = pesos[1],b = pesos[2]){
  
  if (x <= a) {
    temp = "1" #MYPIMES
  }else if( (x > a) & (x <= a + b) ){
    temp = "2" #MEDIANAS
  }else if( x > a + b){
    temp = "3" #GRANDES
  }
  return(temp)

  }

create_cluster <- function(N){
  
  x = cbind ( runif(N) )

return( as.factor(apply(x, 1, clst)) )  
}


segmento = create_cluster(N)
# Se debe hacer regresiones por cada cluster

create_variable<-function(x,N,factores){
  
  
  X.1 <- rgamma(N,shape=2,scale=2.0) 
  X.2 <- rgamma(N,shape=7,scale=2.0) 
  X.3 <- rgamma(N,shape=3,scale=2.0) 
  
  temp <- ((x == 1) * X.1 * factores[1]) + ((x == 2) * X.2 * factores[2]) + ((x == 3) * X.3 * factores[3])

  return(temp)
  }

factores <- c(10000,60000,200000)
factores

activos <- create_variable(segmento,N,factores)
pp_pasivos <- apply( cbind(rbeta(N,5,4),0.8),1, min )
pp_ebitda <- apply( cbind(rbeta(N,1.5,5),0.4),1, min )
pasivos <- activos * pp_pasivos
patrimonio <- activos - pasivos
ebitda <- pp_ebitda * (patrimonio)

n.deuda <- pasivos / patrimonio
hist(n.deuda)
roa <- ebitda / activos
roe <- ebitda / patrimonio

rates = data.frame("1" = seq(0.014,0.018,by=0.001), #MYPYMES
                   "2" = seq(0.011,0.015,by=0.001), #MEDIANAS
                   "3" = seq(0.008,0.012,by=0.001)) #GRANDES

pp_exposicion <- rbeta(N,4,2)

exposicion_init = pasivos * pp_exposicion
p_pago=c(0.18,0.12,0.05)

pago <- function(x,rate,n){
  
  pago_cuota = x * ( (rate * ( rate +1 ) ^ n) / ((1+rate)^n - 1 ) )
  
  return(pago_cuota)
}

plazos <- seq(12,60,by=12)

#Markov-Chain de dos estados 0 = Pago y 1 = No pago
States <- c("0","1")
pagoMatrix <- matrix(data = c(0.9, 0.1, 0.65, 0.35), byrow = T, nrow = 2,
                        dimnames = list(States, States))
mcPagoo <- new("markovchain", states = States, byrow = T,
                  transitionMatrix = pagoMatrix, name = "pago")


# #ajustar cambio de probabilidad de impago con una cadena de markov
# rbern_acum <- function(n,p,lambda = 0.01){
#   x = double(n)
#   p_acum = p
#   
#   for(j in 1:n){
#     if(j == 1){
#       x[j] <- Rlab::rbern(1,p)
#     }else{
#       
#       if(x[(j-1)]==1){
#         p_acum = p_acum + lambda
#         p_final = ifelse(p_acum>1,1,p_acum)
#         x[j] <- Rlab::rbern(1,p_final)
#       }else{
#         p_final = ifelse(p_acum>1,1,p_acum)
#         x[j] <- Rlab::rbern(1,p_final)
#       }
#     }
#   }
#   
#   return(list(x,p_final))
# }

rate.m <- 0.027
rate.d <- (1+rate.m)^(1/30)-1
#TODO DEBE EXPLICARSE! TODAS LAS DEFINICIONES!!
#INCLUIR CASO DONDE EL CLIENTE SE LLEVE A CARTERA NO RECUPERABLE DESPUES DE K MESES DE NO PAGO
#INCLUIR INTERESES DE MORA Y SI ES EL CASO ACUMULARLOS
calculate_saldo <- function(exposicion,segmento,umbral=3){
  
  seg = segmento
  plazo = sample(plazos,1)
  rate_column = paste("X",seg,sep="")
  rate = sample(rates[,rate_column],1)
  flag = as.integer(markovchain::rmarkovchain(n = plazo, 
                                 object = mcPagoo, t0 = "0"))
  
  # list_flag = rbern_acum( plazo , p_pago[ seg ] )
  # flag = list_flag[[1]]
  # p = list_flag[[2]]
  
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
  # i = 1
  # while(amortizacion[i,"saldo"] > 1){
    for( i in 1:plazo){
    amortizacion_prev = amortizacion[amortizacion$periodo == (i-1)
                                     ,"saldo"] 
  
    if(flag[i] == 0){
      mora = 0
        if(i>=2){
          
          if(flag[(i-1)] == 1){
            dias.mora <- min(27,rnbinom(1,25,0.75))
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
      
      # dias.mora <- min(27,rnbinom(1,25,0.75))
      # mora <- amortizacion_prev * ((1+rate.d)^(dias.mora)-1)
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
      
      # if(i>=umbral){
      #   no.pagos <- sum( tail(flag[1:i],umbral) )
      #   if(no.pagos == umbral){
      #     reps = plazo - i + 1
      #     data.frame(periodo = c(i:plazo)
      #                ,saldo = c(exposicion)
      #                ,abono = c(rep(amortizacion_prev - abono_prev,reps))
      #                ,interes = c(0)
      #                ,cuota = c(0)
      #                ,nopago = c(cuota_acum)
      #                # ,mora = c(0)
      #     )
      # 
      #     
      #   }
      #   
      #   
      # }
      
      
      
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
    
    # i = i + 1
  
  }
  return(list(flag,amortizacion,plazo,rate))
}

# lista = list()
# for(k in 1:1000){
#   message("id is:", k)
#   message("initial exposicion is:", format( exposicion_init[k], scientific = F))
#   message("segmento is:", segmento[k])
#   lista[[k]] <- calculate_saldo(exposicion_init[k],
#                          as.integer(segmento[k]))
# }


calculo <- function(k){
  message("id is:", k)
  message("exposicion is:", exposicion_init[k])
  message("segmento is:", segmento[k])
  return(calculate_saldo(exposicion_init[k],
                         as.integer(segmento[k])
                                    ))
}



calculo_data <- function(k,n=6,umbral = 3){
  message("id equal to:", k)
  message("exposicion equal to:", exposicion_init[k])
  message("segmento equal to:", segmento[k])
  list_tmp <- calculate_saldo(exposicion_init[k],
                         as.integer(segmento[k]))
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


library(foreach)
library(doParallel)


#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

# lista <- foreach(k=1:N) %dopar% {
#   tempList = calculo(k) #calling a function
#   #do other things if you want
#   tempList #Equivalent to finalMatrix = list(finalMatrix, tempMatrix)
# }
# 
# lista[[1]]

lista_data <-foreach(k=1:N) %dopar% {
  tempList = calculo_data(k,umbral = 4) #calling a function
  #do other things if you want
  tempList #Equivalent to finalMatrix = list(finalMatrix, tempMatrix)
}


amortiza <- data.frame(matrix(unlist(lapply(lista_data,unlist)), ncol=10, byrow=TRUE))
names(amortiza) <- c(paste("P",1:6,sep=""),
                     "exposicion","flag_npago","pagos","t0")


#impagos mas recientes tienen mas pesos
# b0 <- 50
# b1 <-  1/ c(pesos %*% factores)
# b2 <- (- 1/ c(pesos %*% factores)) / (2/(2+5))
# b3 <- (( 1/ c(pesos %*% factores)) / (2/(2+5) ) ) / 4/10
# i1 <- -1
# i2 <- -1.5
# i3 <- -2.5
# i4 <- -2
# i5 <- -2.5
# i6 <- -3
# b4 <- ((- 1/ c(pesos %*% factores)) / (4/(4+2))) / 1/10

b0 <- 7
b1 <- -1 #Nivel de endeudamiento
b2 <- 4/50 #ROE
b3 <- 4/50 #ROA
b4 <- -10 #FACTOR K NO PAGOS

b5 <-  1/ c(pesos %*% factores) * 1 /5000
b6 <- (- 1/ c(pesos %*% factores)) * (5/(5+4)) * 1 / 40
b7 <- (( 1/ c(pesos %*% factores)) / (4/(5+4)))  / (5/(5+5))  * 1/5000


i1 <- -2/5
i2 <- -2/5
i3 <- -3/5
i4 <- -4/5
i5 <- -5/5
i6 <- -6/5


betas = rbind(b0,b1,b2,b3,b4,
              b5,b6,b7,
              i1,i2,i3,i4,i5,i6
              )
betas

X = data.frame(X0=1,X1=n.deuda,X2=roe,X3=roa,X4=amortiza$flag_npago,
               X5=activos,X6=pasivos,X7=ebitda,
               I1=amortiza$P1,I2=amortiza$P2,I3=amortiza$P3,I4=amortiza$P4,
               I5=amortiza$P5,I6=amortiza$P6)
hist(X$X1*b1)
hist(X$X2*b2)
hist(X$X3*b3)
hist(X$X4*b4)

eta <-  (as.matrix(X) %*% betas)
X$eta <- eta
hist(eta)
logit_eta <- 1 / (1 + exp(  (eta) ) )
hist(logit_eta)
summary(logit_eta)

head(logit_eta)
X$logit_eta <- logit_eta

head(X[X$logit_eta<0.01,])

fi = 0.2
prob=double(N)

for (i in 1:N) {
  # prob[i] = rbeta(1,shape1 = logit_eta[i],shape=fi)
  prob[i] = qbeta(0.5,shape1 = logit_eta[i],shape=fi)
}


segmento_ne_1 <- segmento != "1"
boxplot(prob~segmento
        ,ylim=c(0,0.00000000000000000000000000000000005)
        )
hist(prob)

boxplot(prob[segmento_ne_1]~segmento[segmento_ne_1],
        ylim=c(0,0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000001))

y = ifelse( prob > 0.7, 1, 0)
summary(as.factor(y))

