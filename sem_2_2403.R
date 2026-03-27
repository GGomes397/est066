#a
m <- 2**32
A <- 1664525
B <- 1013904223
xo <- 2
rmeu <- function(n,xo,m,A,B){
  v <- numeric(n)
  xn <- xo
  for (i in 1:n) {
    v[i]=(A*xn+B)%%m->xn
    
  }
  return(v/m)
}
amostra <- rmeu(1000,xo,m,A,B)
amostra
hist(amostra,freq = F)
hist(amostra,freq = F,breaks = 30)
valores <- cut(amostra,breaks = seq(from=0,to=1,by=0.1))
tabela <- table(valores)
#teste 1
chisq.test(tabela)

#aula de hoje 
valores2 <- cut(amostra,breaks = seq(from=0,to=1,by=0.01))
tabela2 <- table(valores2)
#teste 2
chisq.test(tabela2)
#Fazendo com intervalos
hist(amostra,freq = F,breaks =1000 )
#Com numero maior de intervalos nós podemos ver se é mais homogêneo 

Fn <- ecdf(amostra)
plot(Fn)
abline(a=0,b=1,col='red')
Fn(0.5)#
median(amostra)
quantile(amostra,probs = c(0,0.25,0.5,0.75,1))
summary(amostra)
#Vamos fazer o teste de komolgorv 
#H0 =A função empirica = Função acumulada
ks.test(amostra,punif)
# o teste de Kmologorov confirma o teste 1
# e está nos indicado que a geração não foi boa 
plot(amostra, ylim = c(0,1))
abline(h=seq(0,1,by=0.1,lty=2,
             col='gray'))
abline(v=100,lty=2,col="blue")
acf(amostra)
par <- (1:500)*2
impar <- par -1
plot(x=amostra[impar],y=amostra[par])
abline(h=seq(0,1,by=0.1,lty=2,
             col='gray'))
abline(v=seq(0,1,by=0.1,lty=2,
             col='gray'))


# Definir os limites dos quadrados (0 a 1, com intervalos de 0.1)
intervalos <- seq(0, 1, by = 0.1)

# Criar categorias para x e y
x_cat <- cut(amostra[impar], breaks =intervalos  , include.lowest = TRUE)
y_cat <- cut(amostra[par], breaks = intervalos , include.lowest = TRUE)

# Criar tabela de contagem por quadrado
contagem_quadrados <- table(x_cat, y_cat)
sum(contagem_quadrados)
#Fazendo a estatistica chi-squared 
x2 <-sum((contagem_quadrados[1.2,2.2]-10)**(0.2))
x2
chisq.test(contagem_quadrados)

#aula 26/03
#Porque na ultima aula deu erro o teste
amostra <- rmeu(1e3,xo,m,A,B)
par <- (1:500)*2
impar <- par -1
plot(x= amostra[impar],y=amostra[par],xlim = c(0,1),ylim=c(0,1))
reticulado <-seq(0,1,by=0.1)
abline(h=reticulado, v = reticulado, col= "blue")
cutx <- cut(amostra[impar],breaks = reticulado, include.lowest =T)
cuty <- cut(amostra[par],breaks = reticulado, include.lowest =T)
tabela <- table(cutx,cuty)
sum(tabela)
for(i in 1: 10){
  xis <-(i-1)/10+0.05
  for(j in 1:10){
    ipsilon <- (j-1)/10+0.05
    text(x=xis, y=ipsilon , label =tabela[i,j],
         cex=0.85, font=2)
  }
}
boxplot(tabela)
mean(tabela)
abline(h=5,lty=2, lwd=2)
# Vamos fazer o teste chi squared 
Ei <- 5
X2 <- sum((tabela - Ei)**2)/Ei
X2

#vamos calcular o p valor 
1-pchisq(X2,df=99)

chisq.test(c(tabela))# Fazendo isso nós conseguimos transformar a tabela em um
#unico valor que colocamos num vetor 
#Gerando outro conjunto de números aleatórios 
#o Professor escolheu X~Uniforme(a,b)
# x= a+(b-a)U u é amostra que nós criamos 
x <- -1 +2*amostra
x
hist(x)
#gerador 2
m <- 2**31
A <- 65539
B <- 0
xo <- 5
amostra2<- rmeu(1000,xo,m,A,B)
hist(amostra2,freq = F)
hist(amostra2,freq = F,breaks = 30)

#A nossa amotra deve resultar numa uniforme(a,b) nós temos que ter que a nossa 
#esperança deve ser (a+b)/2 e variância (b-a)^2/12 na nossa teoria a,b=0,1
valores <- cut(amostra2,breaks = seq(from=0,to=1,by=1/100))
tabela <- table(valores)
#teste 1 (média)
chisq.test(tabela)
ks.test(amostra2,punif)
#Como nosso p valor deu 0.2066>0.05 
qqplot(qunif(ppoints(200), 0, 1), sample(amostra2, 200),
       main = "Q-Q Plot", xlab = "Teórico", ylab = "Observado",
       pch = 16, col = rgb(0, 0, 0, 0.5))
abline(0, 1, col = "red")
#
hist(amostra2, breaks = 30, freq = FALSE, 
     main = "Histograma", xlab = "Valor", col = "lightblue")
curve(dunif(x, 0, 1), add = TRUE, col = "red", lwd = 2)