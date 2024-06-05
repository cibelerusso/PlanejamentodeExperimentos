# ANOVA modelo fatorial - 2 fatores 2 níveis
# Planejamento de experimentos I
# Profa. Cibele Russo

# Exemplo 

# Y: tempo de execução
# A: máquina
# B: operador


# Entrada de dados


dados <- read.csv('https://raw.githubusercontent.com/cibelerusso/PlanejamentodeExperimentos/main/Dados/tempo_execucao.csv')

# Dados

#View(dados)

dados$A <- factor(dados$A)
dados$B <- factor(dados$B)


# Média geral

mean(dados$Y)

# Efeitos dos fatores

# Efeito principal de A

mean(dados$Y[dados$A=='1']) - mean(dados$Y[dados$A=='-1'])


# Efeito principal de B

mean(dados$Y[dados$B=='1']) - mean(dados$Y[dados$B=='-1'])

# Efeito da interação entre A e B

mean(dados$Y[dados$AB=='1']) - mean(dados$Y[dados$AB=='-1'])

# Modelo fatorial

mod <- lm(Y ~ A * B , data=dados)

summary(mod)

# Análise dos resíduos

plot(mod$residuals)

res <- rstudent(mod)


# Análise gráfica

par(mfrow=c(1,2),pty="s")
plot(fitted(mod),residuals(mod),pch=19,col="blue")
abline(h=0,col="red")
qqnorm(res,col="blue",pch=19)
qqline(res)


# Half-normal plot
#install.packages('hnp')

par(mfrow=c(1,2))
library(hnp)
# half-normal plot with simulated envelope
hnp(mod)
# normal plot with simulated envelope
hnp(mod, half = F)

# Testes

#### Normalidade dos erros

shapiro.test(rstudent(mod))


# Vamos interpretar o quadro da ANOVA

anova(mod)


### Exemplo 2

# Vamos simular dados de um problema com 3 fatores, 2 níveis cada e interação

# Definir os fatores e seus níveis
fator_A <- c(-1, 1)
fator_B <- c(-1, 1)
fator_C <- c(-1, 1)

# Criar a matriz de planejamento fatorial
X <- expand.grid(A = fator_A, B = fator_B, C = fator_C)

# Exibir a matriz de planejamento fatorial
print(X)

# Adicionar colunas de interação (opcional)
X$AB <- X$A * X$B
X$AC <- X$A * X$C
X$BC <- X$B * X$C
X$ABC <- X$A * X$B * X$C

# Exibir a matriz de planejamento fatorial com interações
print(X)

# réplicas:
replicas = 3

X_rep <- kronecker(t(t(rep(1,replicas))), as.matrix(X))
colnames(X_rep) <- colnames(X)


# Simulando dados de resposta:
X_rep <- as.data.frame(X_rep)

dados <- X_rep

# Definir os coeficientes
coeficientes <- c(Intercepto = 10, A = 2, B = 3, C = -1, AB = 4, AC = -2, BC = 1, ABC = -3)

# Calcular a resposta Y
dados$Y <- coeficientes["Intercepto"] + 
  coeficientes["A"] * dados$A + 
  coeficientes["B"] * dados$B + 
  coeficientes["C"] * dados$C + 
  coeficientes["AB"] * dados$AB + 
  coeficientes["AC"] * dados$AC + 
  coeficientes["BC"] * dados$BC + 
  coeficientes["ABC"] * dados$ABC


# Adicionar erro aleatório
set.seed(123) # Para reprodutibilidade
epsilon <- rnorm(nrow(X_rep), mean = 0, sd = 1)
dados$Y <- dados$Y + epsilon

#dados <- as.data.frame(dados)
#dados

#dados <- dados[,-1]

dados

# Modelo fatorial
mod <- lm(Y ~ A * B * C, data=dados)

summary(mod)
# Análise dos resíduos

plot(mod$residuals)
res <- rstudent(mod)

# Análise gráfica

par(mfrow=c(1,2),pty="s")
plot(fitted(mod),residuals(mod),pch=19,col="blue")
abline(h=0,col="red")
qqnorm(res,col="blue",pch=19)
qqline(res)


# Half-normal plot

par(mfrow=c(1,2))
library(hnp)
# half-normal plot with simulated envelope
hnp(mod)
# normal plot with simulated envelope
hnp(mod, half = F)

# Testes

#### Normalidade dos erros

shapiro.test(rstudent(mod))


# Vamos interpretar o quadro da ANOVA

anova(mod)



# Exemplo pilhas

dados <- read.csv('https://raw.githubusercontent.com/cibelerusso/PlanejamentodeExperimentos/main/Dados/pilhas.csv', dec=',')
dados

mod <- lm(Y~A * B * C, data=dados)

summary(mod)
