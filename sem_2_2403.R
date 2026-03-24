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
