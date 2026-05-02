#AULA 23/04
f <- function(x){
  (1/sqrt(2*pi))*exp((-x**2)/2)
}
integrate(f = f, lower = 1, upper = 2) #normal padrão
diff(pnorm(q = c(1,2))) #dá no mesmo de calcular a integral acima


f.2 <- function(y){
  (1/sqrt(2*pi))*exp(((-(y+1))**2)/2)
}
integrate(f = f.2, lower = 0, upper = 1)


h <- function(y){
  exp((-(y+1)**2)/2) 
}
u <- runif(1000, 0, 1)
h_bar <- mean(h(u))
teta_hat <- (1/sqrt(2*pi))*h_bar
teta_hat
erro <- log10(abs(teta_hat - 0.13590))

cat("teta chapéu: ", teta_hat, "\n", "log10 do erro absoluto: ",
    erro, "\n")

#######################
#função h(y)
h <- function(y){
  exp((-(y+1)^2)/2) 
}
#função que retorna os 4 a seguir: média, desvio padrão, erro log e erro padrão
funcao <- function(n){
  u <- runif(n, 0, 1)
  h_val <- h(u)
  h_bar <- mean(h_val)
  h_sd <- sd(h_val)
  teta_hat <- (1/sqrt(2*pi))*h_bar
  ep <- (1/(sqrt(2*pi)))*h_sd
  erro_log <- log10(abs(teta_hat - 0.13590))
  
  return(c(h_bar = h_bar, h_sd = h_sd, erro_log = erro_log, ep = ep))
}

#transpondo a matriz em cada resultado para ficar em foarmato long

rtd <- replicate(50, funcao(100))
df_100 <- as.data.frame(t(rtd))

rtd <- replicate(50, funcao(500))
df_500 <- as.data.frame(t(rtd))

rtd <- replicate(50, funcao(1000))
df_1000 <- as.data.frame(t(rtd))

rtd <- replicate(50, funcao(2000))
df_2000 <- as.data.frame(t(rtd))

rtd <- replicate(50, funcao(5000))
df_5000 <- as.data.frame(t(rtd))

#juntando os 5 dataframes em um só
df_definitivo <- dplyr::bind_rows(df_100, df_500, df_1000, df_2000, df_5000)

#criando a coluna n
df_definitivo$n <- rep(c(100, 500, 1000, 2000, 5000), each = 50)

#plot básico para ter uma noção do resultado
plot(erro_log ~ n, data = df_definitivo, col = 'red', pch = 19)

#transformando a coluna n em fator, mas se fizer isso
#o gráfico se torna um conjunto de boxplots
#df_definitivo$n <- as.factor(df_definitivo$n)

############################################
h <- function(y){
  val <- exp(-((1/y - 1)**2)/2)/y**2
  return(val)
}

func <- function(){
  u <- runif(1000, 0, 1)
  valores.hu <- h(u)
  teta.hat <- (sum(valores.hu))/1000
  return(teta.hat)
}

func()

rtd <- replicate(50, func())
df <- as.data.frame(rtd)
View(df)

###############################
###############################
###############################


h.2 <- function(u) {
  val <- exp(-(u/1-u)^2)/(1-u)^2
  return(val)
}


func.2 <- function(){
  u <- runif(1000, 0, 1)
  valores.hu <- h.2(u)
  teta.hat <- (sum(valores.hu))/1000
  return(teta.hat)
}

func.2()

rtd.2 <- replicate(50, func.2())
df.2 <- as.data.frame(rtd.2)
View(df.2)









