n <- 1e3
u <- runif(n)
h <- function(y){
  exp((-(y+1)**2)/2)
}
h_bar <-mean(h(u))

theta <- (1/sqrt(2*pi))*h_bar
theta
erro <- log10(abs(theta - 0.13590))
erro
#fazendo para diferentes tipos de N
teste <- function(n){
  u <- runif(n)
  hbar <- mean(h(u))
  t <- (1/sqrt(2*pi))*hbar
  return(t)  
}
ttn <- function(n){
  pontos <- replicate(50,teste(n))
  x <- mean(amostra1)
  dp <- (1 / sqrt(2 * pi)) * sd(amostra1) 
  erro <- log10(abs(x - 0.13590))
  
  return(c(n = n, media = x, desvio = dp, log_erro = erro))
}
valores_n <- c(100, 500, 1000, 2000, 5000)


resultados <- t(sapply(valores_n, ttn))

df_resultados <- as.data.frame(resultados)
df_resultados
