gera.z <- function(){
  u <- runif(2)
  mod <- sqrt(-2*log(u[1]))
  teta <- 2*pi*u[2]
  c(cos(teta), sin(teta)) * mod
}
gera.z()
amostra <- as.vector(replicate(gera.z(), n = 500))
mean(amostra)
var(amostra)
hist(amostra, freq = F, xlim = c(-3.5, 3.5), ylim = c(0, 0.45))
curve(dnorm(x), col = "blue4", add = T)
lines(density(amostra), col = "purple4", lwd = 2)

fn <- ecdf(amostra)
plot(fn)
curve(pnorm, col = "blue4", add = T)
ks.test(amostra, "pnorm")
qqnorm(amostra)
qqline(amostra, col = "magenta")

amostra.t <- rt(n = 100, df = 1)
qqnorm(amostra.t)
qqline(amostra.t)
amostra.t <- rt(n = 100, df = 5)
qqnorm(amostra.t)
qqline(amostra.t)

amostra.exp <- rexp(100)
qqnorm(amostra.exp)
qqline(amostra.exp)
