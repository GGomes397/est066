# aula 12/05
corr <- 0.8

df_z1_z2 <- data.frame(z1_vals = numeric(0), z2_vals = numeric(0))

func <- function(){
  z1 <- rnorm(1, 0, 1)
  # Método correto: usar a decomposição de Cholesky ou a fórmula:
  z2 <- corr * z1 + sqrt(1 - corr^2) * rnorm(1, 0, 1)
  
  df_z1_z2 <<- rbind(df_z1_z2, data.frame(z1_vals = z1, z2_vals = z2))
}

replicate(100, func())

plot(df_z1_z2, col = 'red')
cov(df_z1_z2$z1_vals, df_z1_z2$z2_vals)  # Deve estar próximo de 0.8
cor(df_z1_z2$z1_vals, df_z1_z2$z2_vals)  # Deve estar próximo de 0.8

# Resto do seu código está correto...
shapiro.test(df_z1_z2$z1_vals)
shapiro.test(df_z1_z2$z2_vals)

qqnorm(df_z1_z2$z1_vals, main = 'Z1')
qqline(df_z1_z2$z1_vals)
text(x = 1, y = -1, label = c('p-valor = '))
text(x = 1.8, y = -0.93, label = round(shapiro.test(df_z1_z2$z1_vals)$p.value, 4))

qqnorm(df_z1_z2$z2_vals, main = 'Z2')
qqline(df_z1_z2$z2_vals)
text(x = 1, y = -1, label = c('p-valor = '))
text(x = 1.8, y = -0.93, label = round(shapiro.test(df_z1_z2$z2_vals)$p.value, 4))

dados_z1 <- df_z1_z2$z1_vals
dados_z2 <- df_z1_z2$z2_vals

hist(dados_z1, freq = F, col = 'orange', main = 'Z1')
curve(dnorm(x), col = 'red', add = T, lwd = 2)

hist(dados_z2, freq = F, col = 'orange', main = 'Z2')
curve(dnorm(x), col = 'red', add = T, lwd = 2)


z <- cbind(dados_z1, dados_z2) 
s <- cov(z)
s_inv <- solve(s)
zc <- scale(z, center = TRUE, scale = FALSE)
n <- nrow(zc)
dj_quadrado <- numeric(n)

for(j in 1:n){
 
  zc_j <- zc[j, , drop = FALSE]  
  
    temp <- zc_j %*% s_inv

  dj_quadrado[j] <- temp %*% t(zc_j)
}

q.emp <- sort(dj_quadrado)
q.teorico <- qchisq(ppoints(100,a=1/2),df=2)
plot(x=q.teorico,y=q.emp)
pontos.emp <- quantile(dj_quadrado,probs = c(0.25,0.75))
pontos.teo <-qchisq(p=c(0.25,0.75),df=2)
abline(lm(pontos.emp~pontos.teo))

#fazendo a distância de mahalanobis(dj_quadrado) de outras forma
zc%*%s_inv%*%t(zc)
all.equal(dj_quadrado,diag(zc%*%s_inv%*%t(zc)))
#fazendo pela função mahalanobis 
zc_maha <- mahalanobis(x=zc,center=c(0,0),cov = s)
all.equal(dj_quadrado,zc_maha)

#elipse de 95% de confiança 
dispersao <- z
correlacao <- cor(dispersao)[1,2]

elipse.95 <- ellipse::ellipse(zc,levels=0.95,
                                        scale= apply(dispersao, 2, sd),
                                        centre=apply(dispersao, 2, mean))

plot(zc,xlim=range(c(zc[,1], elipse.95[,1])),
     ylim=range(c(zc[,2], elipse.95[,2])), )
lines(elipse.95,col='red',lwd=2)
#sabendo quantos pontos ficaram fora da elipse 
sum(dj_quadrado> qchisq(p=0.95,df=2))
#e quais pontos deram valores diferentes
which(dj_quadrado> qchisq(p=0.95,df=2))
points(z1[])

# para 99
plot(zc)
elipse.99 <- ellipse::ellipse(zc,levels=0.99,
                              scale= apply(dispersao, 2, sd),
                              centre=apply(dispersao, 2, mean))
elipse.50 <- ellipse::ellipse(zc,levels=0.5,
                              scale= apply(dispersao, 2, sd),
                              centre=apply(dispersao, 2, mean))
plot(zc,xlim=range(c(zc[,1], elipse.99[,1])),
     ylim=range(c(zc[,2], elipse.99[,2])), )
lines(elipse.99,col='red',lwd=2)
#sabendo quantos pontos ficaram fora da elipse 
sum(dj_quadrado> qchisq(p=0.99,df=2))
#e quais pontos deram valores diferentes
which(dj_quadrado> qchisq(p=0.99,df=2))
lines(elipse.50,col='orange',lwd=2)
#falou em normalidade pensei em elipses 


