1 / beta(2, 6)
curve(dbeta(x, shape1 = 2, shape2 = 6), from = 0, to = 1, col = "blue")
curve(dbeta(x, shape1 = 6, shape2 = 2), from = 0, to = 1, col = "purple", add = T)
moda <- 1/6
dbeta(x = moda, shape1 = 2, shape2 = 6)
dbeta(x = 5/6, shape1 = 6, shape2 = 2)

# olhar foto dos passos para gerar
gerador.beta <- function() {
  alfa <- 2
  beta <- 6
  u <- runif(n = alfa + beta)
  y <- -log(u) 
  x1 <- sum(y[1:alfa]) # gama(2, 1)
  x2 <- sum(y[(alfa + 1): (alfa + beta)]) # gama(6, 1)
  w <- x1 / (x1 + x2) # beta(2, 6)
  return(w)
}
gerador.beta()
amostra <- invisible(replicate(n = 100, gerador.beta()))
hist(amostra, freq = F, breaks = 30, xlim = c(0, 1))
curve(dbeta(x, shape1 = 2, shape2 = 6), add = T)
plot(ecdf(amostra))
ks.test(amostra, "pbeta", shape1 = 2, shape2 = 6) # p-valor alto, não tem evidênicas para rejeitar que percente a beta(2, 6)  

605 %% 3 
