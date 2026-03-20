m <- 10
A <- 103
B <- 17
gera_al <- function(m,A,B,xn){
  xn1 <-(A*xn+B)%%m
  return(xn1)
}
gera_un <- function(x,m){
  u <- x/m
  return(u)
}
xo <- 2
u0 <- gera_un(xo,m)
x1 <- gera_al(m,A,B,xo)
u1 <- gera_un(x1,m)
x2 <- gera_al(m,A,B,x1)
u2 <- gera_un(x2,m)
x3 <- gera_al(m,A,B,x2)
u3 <- gera_un(x3,m)
x4 <- gera_al(m,A,B,x3)
u4 <- gera_un(x4,m)
x5 <- gera_al(m,A,B,x4)
u5 <- gera_un(x5,m)
x6 <- gera_al(m,A,B,x5)
u6 <- gera_un(x6,m)
x7 <- gera_al(m,A,B,x6)
u7 <- gera_un(x7,m)
x8 <- gera_al(m,A,B,x7)
u8 <- gera_un(x8,m)
#Você percebe que um dado momento os numeros se repetem 
#geramos o numero aleatório 
m <- 2**32
A <- 1664525
B <- 1013904223
xo <- 2
u0 <- gera_un(xo,m)
x1 <- gera_al(m,A,B,xo)
u1 <- gera_un(x1,m)
x2 <- gera_al(m,A,B,x1)
u2 <- gera_un(x2,m)
x3 <- gera_al(m,A,B,x2)
u3 <- gera_un(x3,m)
x4 <- gera_al(m,A,B,x3)
u4 <- gera_un(x4,m)
x5 <- gera_al(m,A,B,x4)
u5 <- gera_un(x5,m)
x6 <- gera_al(m,A,B,x5)
u6 <- gera_un(x6,m)
x7 <- gera_al(m,A,B,x6)
u7 <- gera_un(x7,m)
x8 <- gera_al(m,A,B,x7)
u8 <- gera_un(x8,m)

rmeu <- function(n,xo,m,A,B){
  v <- numeric(n)
  xn <- xo
  for (i in 1:n) {
    v[i]=(A*xn+B)%%m->xn
    
  }
  return(v/m)
}
rmeu(10,xo,m,A,B)  
amostra <- rmeu(1000,xo,m,A,B)
amostra
#Nós queremos saber os valores que geramos "bate " com os valores da uniforme(0,1)
#vamos calcular a media e o desvio padrão 
mean(amostra)
var(amostra)
#E para saber o que esta dando a mesma densidade vamos plotar o histograma 
hist(amostra,freq = F)
#note que ainda não é exatamente então vamos aumentar a quantidade de intervalos  
hist(amostra,freq = F,breaks = 30)
#queremos os numeros de pontos por intervalos 
hist.obj <- hist(amostra, freq=F, breaks=10)
hist.obj$counts # Mostra quantas observações caíram em cada intervalo
# Fazendo isso usando com o cuts 
intervalos <- cut(amostra,breaks = seq(from=0,to=1,by=0.1))
table(intervalos)
# vamos fazer um teste de hipotese pra isso . A pergunta desse teste é :
#Será que a verdadeira média da população (de onde veio esta amostra) é igual a 0,5?
t.test(amostra,mu=0.5)
#fazendo um outro teste , o teste qui-quadrado
v1 <- hist.obj$counts
(sum((v1 - 100)**2))/100
1-pchisq((sum((v1 - 100)**2))/100,df=9)
