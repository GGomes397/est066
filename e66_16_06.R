continha <- function(x){
  a <- 2*(x^2)+2*(x^3)-5*(x^4)+2*(x^5)
  return(a)
}#isso significa o sistme
continha(0.8)
continha(0.5)
contas
curve(continha)
continhas <- function(p1,p3){
  a <- (1-(1-p1^2)^2)*(1-p3)+((1-(1-p1)^2)^2)*(p3)
  return(a)
}
continhas(0.8,0.96)
#pra descobrirmos as raízes da equação 
polyroot(c(-0.95,0,2,2,-5,2))
#como eu faço uma simulação ???
#montando a matriz de adjacência pensando do sistema passar L pra R 
c1 <- 0
c2 <- 1
c3 <- 1
c4 <- 1
c5 <- 0
m_adj <- matrix(c(0,c1,c4,0,
                  0,0,c3,c2,
                  0,c3,0,c5,
                  0,0,0,1),nrow = 4,byrow = 4,
                dimnames = list(c('L',"U","D","R"),c('L',"U","D","R")))
m_adj
m_adj%*%m_adj
(m_adj%*%m_adj)%*%m_adj
c4 <- 0
m_adj <- matrix(c(0,c1,c4,0,
                  0,0,c3,c2,
                  0,c3,0,c5,
                  0,0,0,1),nrow = 4,byrow = 4,
                dimnames = list(c('L',"U","D","R"),c('L',"U","D","R")))
m_adj
m_adj%*%m_adj
#eu quero fazer os sorteio  pra selecionar 
#se a chave está aberta(0) ou fechada (1)

sorteia <- function(p){
  rbinom(n=1,size=1,prob = p)
}
p1 <- p2 <-p3 <- p4 <- p5 <- 0.8
p
c1 <- sorteia(p1)
c2 <-sorteia(p2)
c3 <- sorteia(p3)
c4 <- sorteia(p4)
c5 <- sorteia(p5)
print(c(c1,c2,c3,c4,c5))
m_adj <- matrix(c(0,c1,c4,0,
                  0,0,c3,c2,
                  0,c3,0,c5,
                  0,0,0,1),nrow = 4,byrow = 4,
                dimnames = list(c('L',"U","D","R"),c('L',"U","D","R")))
m_adj[1,4]!=0
(m_adj%*%m_adj)%*%m_adj

n <- 1000
sort2 <- function(n){
  adj <- logical(n)

  c1 <- sorteia(0.96)
  c2 <-sorteia(0.96)
  c3 <- sorteia(p3)
  c4 <- sorteia(0.96)
  c5 <- sorteia(0.96)
  m_adj <- matrix(c(0,c1,c4,0,
                    0,0,c3,c2,
                    0,c3,0,c5,
                    0,0,0,1),nrow = 4,byrow = 4)
  m_adj <- m_adj%*%m_adj%*%m_adj
  adj <- m_adj[1,4]!=0
  return(adj)
}

a <- replicate(n,sort2(n))
mean(a)
#para trabalhar com tempo nós trabalhamos com a distribuição de weibull,exp,gamma

sort2 <- function(n){
  adj <- logical(n)
  
  c1 <- sorteia(0.96)
  c2 <-sorteia(0.96)
  c3 <- sorteia(p3)
  c4 <- sorteia(0.96)
  c5 <- sorteia(0.96)
  m_adj <- matrix(c(0,c1,c3,c4,c5,0,0,
                    ),nrow = 4,byrow = 4)
  m_adj <- m_adj%*%m_adj%*%m_adj
  adj <- m_adj[1,4]!=0
  return(adj)
}
continha2 <- function(p){
  p+ 3*(p^2) - 4*(p^6)- (p^4) + 3*(p^5)- p^6
}
continha2(0.8)
