gera_cauchy_normais <- function(n) {
  
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  return(x1 / x2)
}
gera_cauchy_inv <- function(n) {
  u <- runif(n)
  
  x <- tan(pi * (u - 0.5))
  return(x)
}
t1 <- system.time(amostra_inv <- gera_cauchy_inv(1e3))
t2 <- system.time(amostra_norm <- gera_cauchy_normais(1e6))
cat("Tempo usando a inversa para uma amostra de tamanho 1e3:", t1[3], "segundos\n")
cat("Tempo usando a normal padrãopara uma amostra de tamanho 1e3:", t2[3], "segundos\n")

n_medias <- 2000
tamanho_amostra <- 100 # n do TCL

# Geramos uma matriz onde cada coluna é uma amostra e calculamos a média de cada uma
medias_cauchy <- replicate(n_medias, mean(gera_cauchy_inv(tamanho_amostra)))

# Plotando o resultado
par(mfrow=c(1,1))
# Usamos xlim porque a Cauchy gera valores extremos que estragam o gráfico
hist(medias_cauchy, breaks=500, freq=FALSE, xlim=c(-10, 10), 
     main="TCL na Cauchy: A média continua sendo uma Cauchy!", 
     col="plum", border="white")

# Adicionamos a curva teórica da Cauchy padrão para comparar
curve(dlogis(x, 0, 1), add=TRUE, col="red", lwd=2) # Apenas referência visual
curve(dcauchy(x, 0, 1), add=TRUE, col="blue", lwd=2, lty=2)

legend("topright", legend=c("Distribuição das Médias", "Densidade Cauchy Teórica"),
       col=c("plum", "blue"), lwd=c(10, 2))
gera_10 <- function(n) {
  x_final <- numeric(n)
  
  for(i in 1:n) {
    sucesso <- FALSE
    while(!sucesso) {
      u1 <- runif(1)
      if(u1 < 2/3) {
        # Tenta gerar da primeira parte (0 < x < 1)
        # Usamos rejeição aqui pois a densidade é proporcional a (1 + e^{-2x})
        candidato <- runif(1)
        if(runif(1) < (1 + exp(-2*candidato))/2) {
          x_final[i] <- candidato
          sucesso <- TRUE
        }
      } else {
        # Gera da segunda parte (x > 1) - Exponencial deslocada
        x_final[i] <- 1 + rexp(1, rate = 2)
        sucesso <- TRUE
      }
    }
  }
  return(x_final)
}

amostra10 <- gera_10(1000)
hist(amostra10, breaks=50, prob=TRUE, col="lightblue", main="Distribuição Mista (Ross)")
