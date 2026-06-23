original <- c(7,5,3,9,6)
Fn <- ecdf(original)
summary(original)
knots(Fn)
plot(Fn,ylim=c(0,1.1),main= "")
text(knots(Fn),1:5/5,knots(Fn),cex=0.8,pos=3)
#geração da amostra bootstrap
set.seed(666)
amostra.boot <- sample(original,5, replace = TRUE)
amostra.boot
mean(amostra.boot)
#geração de todas as amostras com reposição da original
library(gtools)
amostras.boot <- permutations(n=5,r=5,v= original,repeats.allowed = T)
dim(amostras.boot)
medias.boot <- apply(amostras.boot, 1, mean)
mean(medias.boot)
hist(medias.boot, freq = F, ylab = "Densidade", main = "Todas possíveis")
lines(density(medias.boot), col = "blue")
# Aproximação Monte Carlo da distribuição bootstrap
medias.boot2 <- replicate(1000, mean(sample(original, 5, replace = T)))
mean(medias.boot2)
hist(medias.boot2, freq = F, ylab = "Densidade",main = "Aproximação Monte Carlo")
lines(density(medias.boot2), col = "blue")
