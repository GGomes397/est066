#acrestando a função que deu erro na ultima aula 
N <- 1e5
x <- runif(N)
y <- runif(N)
dentro <- y>= x**2
f <- x+y
estimativa <- (1/N)*(sum(f[dentro]))
estimativa
dcauchy(x,location = 0,scale = 1)

curve(dcauchy(x), xlim = c(-4, 4), ylim = c(0, 0.4))

curve(dnorm(x), col = "blue", add = T)

## avaliando como a densidade da distribuição da cauchy
f <- function(x){
  cos(100*x)^2*dcauchy(x)
}
curve(f, from=-3, to=3)
integrate(f,lower= -Inf,upper = -Inf)
MASS:area
library(MASS)
#sabendo como a função area do mass funciona para integrar 
area(f,a=-3,b=3)
area(f,a=-10,b=10)
area(f,a=-20,b=20)
area(f,a=-50,b=50)
area(f,a=-100,b=100)
area(f,a=-300,b=300)
area(f,a=-400,b=400)
area(f,a=-500,b=500)
area(f,a=-600,b=600)
area(f,a=-1000,b=100)
N <- 1e6
amostras <- rcauchy(N)
#integral de monte carlo 
resultados_mc <- mean(cos(amostras*100)^2)
#cada um dos alunos 
resultados_mc
