#parte1
h <- function(u) {
 xis <- u/(1-u)
 a <- exp(-xis**2/2)/(1-u)**2
  return(a)
}
n <- 1000
u <-runif(n) 
mean(h(u))
gera.1 <- function(n){
  u <-runif(n) 
  return(mean(h(u)))
}
estimativas <- replicate(1000, gera.1(n))
hist(estimativas, freq=F))
         )diff()quantile(estimativas, probs = c(0.025, 0.975))
#fazendo isso para n=10000
estimativas <- replicate(100, gera.1(n))
hist(estimativas, freq=F)bs = c(0.025, 0.975))
lines(density(estimativas), col = "blue", lwd = 2)
mean(estimativas)
sd(estimativas)
quantile(estimativas, probs = c(0.025, 0.975))
diff(quantile(estimativas, probs = c(0.025, 0.975)))#amplitude      
#fazendo isso para n=10000
estimativas <- replicate(10000, gera.1(n))
hist(estimativas, freq=F)
lines(density(estimativas), col = "blue", lwd = 2)
mean(estimativas)
sd(estimativas)
quantile(estimativas, probs = c(0.025, 0.975))
diff(quantile(estimativas, probs = c(0.025, 0.975))) 

#parte 2
#deu errado 
#set.seed(666)
#amostra <- rcauchy(10)+350
#veross <-function(teta){
  
 # u <- prod(dcauchy(amostra-teta))
  #return(u)
  #}

#integrate(veross,-Inf,Inf)


