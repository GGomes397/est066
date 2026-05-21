# estamos tentando estimar o pi 

n <- c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6)
n_vezes <- function(n) {
  x <- 2*(runif(n, 0, 1)) - 1
  y <- 2*(runif(n, 0, 1)) - 1
  amostra <- x^2 + y^2
  mean(amostra <= 1)
  estimativa <- mean(amostra<=1)
  estimativa_pi <- 4*estimativa
  return(estimativa_pi)
}
ene <- rep(n,10)

estimativas <- sapply(ene, n_vezes)
erros <- abs(pi - estimativas) / pi
x <- log(ene, base = 10)
y <- log(erros, base = 10)
plot(x, y,
     main = "Evolução dos Erros para cada n",
     ylab = "erro relativo (log10)", 
     xlab = "log10(n)")
#estamos fazendo o log_10(erro relatico )= intercepto -xlog_10(n)
lm(y~x)
abline(lm(y~x),lty=2,col="blue")
erro_medio <- aggregate(y~x,FUN = mean)
erro_medio

points(erro_medio,pch=3,col="red", lwd=2)
mean(estimativas)
sd(estimativas)

# exemplo I - area do retângulo
# to entendendo nada 
N <- 1e5
x <- runif(N,min = 0,max = 2)
y <- runif(N,min = 0,max = 1)
area <- 2
f <- function(x,y){
  2*x**2+y^2
}
estimativa <- area*mean(f(x,y))
estimativa
#
N <- 1e5
x <- runif(N)
y <- runif(N)
dentro <- y> x**2
valores <- x+y
mean(dentro)
resultado<- mean(valores[dentro])
resultado
estima <- mean(valores[dentro])*mean(dentro)
estima
