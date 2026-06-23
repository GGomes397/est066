p1 <- 0.5
p2 <- 0.1 
p3 <- 0.4
mu1 <- -2
mu2 <- 0
mu3 <- 2
dp1 <- sqrt(0.5)
dp2 <- sqrt(0.2)
dp3 <- 1
MN1 <- function(x){
  p1*dnorm(x, mean = mu1, sd = dp1) + p2*dnorm(x, mean = mu2, sd = dp2) + p3*dnorm(x, mean = mu3, sd = dp3)
}
#acumulada
pMN1 <- function(x){
  p1*pnorm(x, mean = mu1, sd = dp1) + p2*pnorm(x, mean = mu2, sd = dp2) + p3*pnorm(x, mean = mu3, sd = dp3)
}
curve(MN1, xlim = c(-4, 4), col = 'red', lwd = 2)
curve(p1*dnorm(x, mean = mu1, sd = dp1), col = 'blue', add = T, lwd = 2, lty = 2)
curve(p2*dnorm(x, mean = mu2, sd = dp2), col = 'blue', add = T, lwd = 2, lty = 2)
curve(p3*dnorm(x, mean = mu3, sd = dp3), col = 'blue', add = T, lwd = 2, lty = 2)
abline(v = 1/3, col = 'black', lty = 2, lwd = 2)

medias <- c(mu1,mu2,mu3)
desvios <- c(dp1,dp2,dp3)
gera_MN1 <- function(x){
  pop <- sample(x = c(1, 2, 3), 1, prob = c(p1, p2, p3))
  rnorm(1)*desvios[pop] + medias[pop]
}
amostra <- replicate(100, gera_MN1())

hist(amostra, ylim = c(0, 0.3), prob = T, col = 'skyblue')
curve(MN1, add = T, lwd = 2, lty = 2)
lines(density(amostra))
#
Fn <- ecdf(amostra)
plot(Fn)
curve(pMN1, add = T, col = 'red', lwd = 2)
ks.test(amostra, pMN1)

#qq-plot
eq.MN1 <- function(x, p){
  pMN1(x) - p
}

q.empirico <- sort(amostra)
pes <- ppoints(100, a = 1/2)
q.empirico <- sort(amostra)
pes <- ppoints(100, a = 1/2)
q.teorico <- numeric(100)
i <- 1

for(p in pes){
  q <- uniroot(eq.MN1, lower = -5, upper = 5, p = p)
  q.teorico[i] <- q$root
  i <- i + 1
}

eq.MN1 <- function(x, p=0.75){
  pMN1(x) - p
}

uniroot(eq.MN1, lower = -5, upper = 5)

quartil_1 = -2.000
#quartil_2 = -0.586
quartil_3 = 1.681

pontos.empiricos <- quantile(amostra, probs = c(.25, .75))
pontos.teoricos <- c(quartil_1, quartil_3)

plot(x = q.teorico, y = q.empirico, col = 'red')
abline(lm(pontos.empiricos ~ pontos.teoricos), col = 'black')

#gAMMA
p4 <- 2/3
p5 <- 1/3
a_1 <-11 
a_2 <- 11
l_1 <- 1/20
l_2 <- 7/20
MG1 <- function(x){
  p4*dgamma(x,shape = a_1, rate= l_1) + p5*dgamma(x,shape = a_2, rate= l_2)
}
#acumulada das gamas 
pMG1 <- function(x){
  p4*pgamma(x,shape = a_1, rate= l_1) + p5*pgamma(x,shape = a_2, rate= l_2)
}
curve(MG1, xlim = c(0, 400), col = 'red', lwd = 2)
curve(p4*dgamma(x,shape = a_1, rate= l_1), col = 'blue', add = T, lwd = 2, lty = 2)
curve( p5*dgamma(x,shape = a_2, rate= l_2), col = 'blue', add = T, lwd = 2, lty = 2)

parametros <- matrix(c(a_1,a_2,l_1,l_2),
                     ncol = 2,
                     byrow = FALSE)
gerador_gamma <-function(x){
  i <- sample(x = c(1, 2), 1, prob = c(p4, p5))
  rgamma(1,shape = parametros[i,1],rate = parametros[i,2])
}
amostra2 <- replicate(100, gerador_gamma())

hist(amostra2, ylim = c(0, 0.02), prob = T, col = 'skyblue')
curve(MG1, add = T, lwd = 2, lty = 2)
lines(density(amostra2))

Fn <- ecdf(amostra2)
plot(Fn)
curve(pMG1, add = T, col = 'red', lwd = 2)
ks.test(amostra, pMG1)
