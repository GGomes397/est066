#Fazendo a geração para uma amostra de tamanho de 1 
z1 <- rnorm(1, mean=0,sd=1)
z1
rho <- 0.8
mi2 <-rho*z1
mi2
var2 <- 1-rho**2
z2 <- rnorm(1,mean=mi2,sd=sqrt(var2))

#para mais pontos 
zn1 <- replicate(1e3,rnorm(1, mean=0,sd=1))
zn1
hist(zn1)

mi2 <-rho*zn1
mi2
var2 <- 1-rho**2
zn2 <- replicate(1e3,rnorm(1,mean=mi2,sd=sqrt(var2)))
zn2
cov(zn1, zn2)
plot(zn2~zn1)
qqnorm(zn1)

rm(list = ls())



  # parametros
ro <- 0.8

  # gera z1
z1 <- rnorm(n=1)

  # gera z2
mi2 <- ro*z1
dp2 <- sqrt(1-ro**2)

z2 <- rnorm(n=1, mean = mi2, sd = dp2)


