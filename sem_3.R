#Deseja-se gerar numeros aleatórios de X com fdp f_c:
#f_x=2/(sqrt(2pi))**exp{-x^2/2} para x>=0 e 0 caso contrário
#note que não temos que commo fazer o metódo da inversa 
#vamos usar o metodo da rejeição e aceitação 
dx<- function(x) {
  (2/sqrt(2*pi))*exp(-x**2/2)
}
g <- function(x){
  exp(-x)
}
g2 <-  function(x){
  2*exp(-x)
}
# Plotar
plot(dx)
#add=true plota o gráficos do curve sobrepostos
curve(dx, from =0 , to = 4 )
curve(g, from=0,to=4, add = TRUE, col="blue")
curve(g2, from=0,to=4, add = TRUE, col="red")

#o que eu to fazendo ???
curve(2*dnorm(x),from =0 , to = 4, ylim=c(0,2))
curve(dexp(x),from =0 , to = 4, col="blue", add = T)
curve(2*dexp(x),from =0 , to = 4, col="red", add = T)
set.seed(666)
rexp(1)
i <- 0
u <- 1
razao <- 0
gerador <- function(c = 2){
  i <- 0
  while(u > razao){
    y <- rexp(1)
    u <- runif(1)
    f <- 2 * dnorm(y)
    cg <- 2 * dexp(y)
    razao <- f/cg
    i <- i +1
  }
  cat("Número:",y,"\nVezes rodadas:",i)
}
gerador()
 
