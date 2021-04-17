
library(gamlss)
library(Rlab)
library(markovchain)

#ESCRIBIR LA TESIS EN TIPO BOOK
#UTILIZAR EL ESTILO DE BIBLIOGRAFIA HARVARD

N = 100000   #TENTATIVO


###IDENTIFICADOR

id = 1:N
a=0.5
b=0.4
c=1-a-b
pesos = c(a,b,c)

## CLUSTERES

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
  
  X.1[X.1 < 0] <- 0
  X.2[X.2 < 0] <- 0
  X.3[X.3 < 0] <- 0
  
  temp <- ((x == 1) * X.1 * factores[1]) + ((x == 2) * X.2 * factores[2]) + ((x == 3) * X.3 * factores[3])

  return(temp)
  }

factores <- c(10000,60000,200000)
factores

activos <- create_variable(segmento,N,factores)
pp_pasivos <- apply( cbind(rbeta(N,8,2),0.5),1, max )
pp_ebitda <- apply( cbind(rbeta(N,1.5,5),0.4),1, min )
pasivos = activos * pp_pasivos
ebitda = pp_ebitda * (activos - pasivos)
sum(ebitda/pasivos > 0.4)
(activos - pasivos)/activos

rate_lower = c(0.017,0.012,0.08)
rate_upper = c(0.025,0.020,0.016)

rates = data.frame("1" = seq(0.018,0.024,by=0.001), #MYPYMES
                   "2" = seq(0.013,0.019,by=0.001), #MEDIANAS
                   "3" = seq(0.008,0.014,by=0.001)) #GRANDES

pp_exposicion <- rbeta(N,4,2)
plot(density(pp_exposicion))

exposicion_init = pasivos * pp_exposicion
p_pago=c(0.18,0.12,0.05)

pago <- function(x,rate,n){
  
  pago_cuota = x * ( (rate * ( rate +1 ) ^ n) / ((1+rate)^n - 1 ) )
  
  return(pago_cuota)
}

plazos <- seq(12,60,by=12)

States <- c("0","1")
pagoMatrix <- matrix(data = c(0.8, 0.2, 0.4, 0.6), byrow = T, nrow = 2,
                        dimnames = list(States, States))
mcPagoo <- new("markovchain", states = States, byrow = T,
                  transitionMatrix = pagoMatrix, name = "pago")
as.integer(rmarkovchain(n = 50, object = mcPagoo, t0 = "0"))


#ajustar cambio de probabilidad de impago con una cadena de markov
rbern_acum <- function(n,p,lambda = 0.01){
  x = double(n)
  p_acum = p
  
  for(j in 1:n){
    if(j == 1){
      x[j] <- Rlab::rbern(1,p)
    }else{
      
      if(x[(j-1)]==1){
        p_acum = p_acum + lambda
        p_final = ifelse(p_acum>1,1,p_acum)
        x[j] <- Rlab::rbern(1,p_final)
      }else{
        p_final = ifelse(p_acum>1,1,p_acum)
        x[j] <- Rlab::rbern(1,p_final)
      }
    }
  }
  
  return(list(x,p_final))
}



#TODO DEBE EXPLICARSE! TODAS LAS DEFINICIONES!!
#INCLUIR CASO DONDE EL CLIENTE SE LLEVE A CARTERA NO RECUPERABLE DESPUES DE K MESES DE NO PAGO
#INCLUIR INTERESES DE MORA Y SI ES EL CASO ACUMULARLOS
calculate_saldo <- function(exposicion,segmento){
  seg = segmento
  plazo = sample(plazos,1)
  rate = sample(rates[,paste("X",seg,sep="")],1)
  
  list_flag = rbern_acum( plazo , p_pago[ seg ] )
  flag = list_flag[[1]]
  p = list_flag[[2]]
  
  cuota = pago(exposicion,rate,plazo)
  
  amortizacion = data.frame(periodo = c(0),
                            saldo = c(exposicion),
                            abono = c(0),
                            interes = c(0),
                            cuota = c(0))
  k = 0
  # i = 1
  # while(amortizacion[i,"saldo"] > 1){
    for( i in 1:plazo){
    amortizacion_prev = amortizacion[amortizacion$periodo == (i-1)
                                     ,"saldo"]
  
    if(flag[i] == 0){
      
        if(i>=2){
          
          if(flag[(i-1)] == 1){
            
            cuota = pago(amortizacion_prev,rate,plazo - k)
            interes_prev = amortizacion_prev * rate
            abono_prev = cuota - interes_prev
            k <- k + 1
          }else{
            
            interes_prev = amortizacion_prev * rate
            abono_prev = cuota - interes_prev
            k <- k + 1
          }
          
        }else{
         
          interes_prev = amortizacion_prev * rate
          abono_prev = cuota - interes_prev
          k <- k + 1
        }
      
    }else{
      
      if(i>=2){
        
        if(flag[(i-1)] == 1){
          cuota = pago(amortizacion_prev,rate,plazo - k)
          interes_prev = amortizacion_prev * rate
          abono_prev = cuota - interes_prev
        }else{
          interes_prev = amortizacion_prev * rate
          abono_prev = - interes_prev
        }
        
      }else{
        interes_prev = amortizacion_prev * rate
        abono_prev = - interes_prev
        
      }
      

      
    }

    amortizacion = rbind( amortizacion,
                          c(i,
                            amortizacion_prev - abono_prev,
                            abono_prev,
                            interes_prev,
                            cuota
                            ))
    
    # i = i + 1
  
  }
  return(list(flag[1:i],p,amortizacion,plazo,rate))
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

calculo_data <- function(k){
  message("id is:", k)
  message("exposicion is:", exposicion_init[k])
  message("segmento is:", segmento[k])
  list_tmp <- calculate_saldo(exposicion_init[k],
                         as.integer(segmento[k]))
  plazo <-list_tmp[[4]]
  t0<- sample(6:(plazo-6),1)
  tf<- t0+5
  
  flag <- list_tmp[[1]][t0:tf]
  exposicion_final <-  list_tmp[[3]][tf,"saldo"]
  flag_acum <- sum(list_tmp[[1]][1:(t0-1)])
  return(list(flag,exposicion_final,flag_acum))
  
}


library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

lista <- foreach(k=1:N) %dopar% {
  tempList = calculo(k) #calling a function
  #do other things if you want
  tempList #Equivalent to finalMatrix = list(finalMatrix, tempMatrix)
}

lista[[1]]

lista_data <-foreach(k=1:N) %dopar% {
  tempList = calculo_data(k) #calling a function
  #do other things if you want
  tempList #Equivalent to finalMatrix = list(finalMatrix, tempMatrix)
}

lista_data[[1]]
  
amortiza <- data.frame(matrix(unlist(lapply(lista_data,unlist)), ncol=8, byrow=TRUE))
names(amortiza) <- c(paste("P",1:6,sep=""),"exposicion","flag_acum")

#impagos mas recientes tienen mas pesos
b0 <- 50
b1 <-  1/ c(pesos %*% factores)
b2 <- (- 1/ c(pesos %*% factores)) / (2/(2+5))
b3 <- (( 1/ c(pesos %*% factores)) / (2/(2+5) ) ) / 4/10
i1 <- -1
i2 <- -1.5
i3 <- -2.5
i4 <- -2
i5 <- -2.5
i6 <- -3
b4 <- ((- 1/ c(pesos %*% factores)) / (4/(4+2))) / 1/10

betas = rbind(b0,b1,b2,b3,b4,i1,i2,i3,i4,i5,i6)
betas

X = data.frame(X0=1,X1=activos,X2=pasivos,X3=ebitda,X4=amortiza$exposicion,
               I1=amortiza$P1,I2=amortiza$P2,I3=amortiza$P3,I4=amortiza$P4,
               I5=amortiza$P5,I6=amortiza$P6)

eta <-  (as.matrix(X) %*% betas)

logit_eta <- 1 / (1 + exp( - (eta) ) )

fi = 2
prob=double(N)

for (i in 1:N) {
  prob[i] = rbeta(1,shape1 = logit_eta[i],shape=fi)
}
segmento_ne_1 <- segmento != "1"
boxplot(prob~segmento)

boxplot(prob[segmento_ne_1]~segmento[segmento_ne_1],ylim=c(0,0.001))

y = ifelse( prob > 0.7, 1, 0)
summary(as.factor(y))



