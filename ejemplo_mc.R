library(markovchain)

A = c(0.5,0.2,0.2,0.1,
      0.2,0.3,0.3,0.2,
      0.1,0.25,0.25,0.4,
      0,   0,   0, 1)

States <- c("0","30","60","CC")

A.m <- matrix(data = A , byrow = T, nrow = 4,
       dimnames = list(States, States))

A.m

mc <- new("markovchain", states = States, byrow = T,
              transitionMatrix = A.m, name = "Matriz de rodamiento")

mc

markovchain::rmarkovchain(n = 10,object = mc, t0 = "0")

