# integração monte carlos

gamma(1/2) # isso é 
sqrt(pi) # igual a isso
a <- 1/2
f <- function(x) x^(a - 1) * exp(-x)
integrate(f = f, lower = 0, upper = Inf)

dbeta(2, 4)
u <- runif(1000)
g <- function(x, a = 2, b = 4) x^(a - 1) * (1 - x)^(b - 1)
teta.hat <- mean(g(u))

rodadas <- 1e3
gera.teta <- function() {
  u <- runif(rodadas)
  teta.hat <- mean(g(u))
}
estimativas <- replicate(gera.teta(), n = 500)
teta <- 0.05
vies <- mean(estimativas - teta)
erro.padrao <- sd(estimativas)
eqm <- mean((estimativas - teta)**2)
hist(estimativas, freq = F, border = "blue4")
lines(density(estimativas), col = "blue4") # density da uma estimação da sua curva de densidade
