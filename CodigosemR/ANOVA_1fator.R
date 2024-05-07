# Aula prática 2 - ANOVA com um fator - DIC
# Planejamento de experimentos I
# Profa. Cibele Russo

# Experimento

####

#- Quatro projetos de circuito de computador

#- Variável resposta: número de ruídos

# Entrada de dados

y<-c(19, 20, 19, 30, 8,
     80, 61, 73, 56, 80,
     47, 26, 25, 35, 50,
     95, 46, 83, 78, 97)
trat <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5))
trat <- as.factor(trat)
dados <- data.frame(trat,y)
str(dados)

# Dados

dados

# Boxplot

boxplot(split(dados$y,dados$trat))

# Variabilidade

(varx <- tapply(dados$y,dados$trat,var))
max(varx)/min(varx)


# Análise dos resíduos

mod <- lm(y ~ trat,data=dados)

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

#### Homocedasticidade de variâncias

bartlett.test(y ~ trat, data=dados)

#

####

#- Os pressupostos do modelo não foram satifeitos

#- A hipótese de normalidade dos dados foi rejeitada

#- Verificou-se a existência de um outlier.


data.frame(trat=trat,yobs=y,residuo=res)

#Observe no tratamento 4 o resíduo da obs. 46 é $-3.7530$
  
plot(y,res,col="blue")
#locator(1)



tapply(y,trat,summary)
(media <- tapply(y,trat,mean))
(dp    <- tapply(y,trat,sd))



#Retirando-se a observação $46$.

y.m <- c(19, 20, 19, 30, 8,
         80, 61, 73, 56, 80,
         47, 26, 25, 35, 50,
         95, 83, 78, 97)   

trat.m <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,4))
trat.m <- as.factor(trat.m)
str(dados)
dados.novo <- data.frame(trat.m=trat.m,y.m=y.m)


#

#### Observe que o tratamento 4 passou a ter 4 repetições

dados.novo

boxplot(split(dados.novo$y.m,dados.novo$trat.m))

tapply(dados$y,dados$trat,var)
(var.novo <- tapply(dados.novo$y.m,dados.novo$trat,var))
max(var.novo)/min(var.novo)

mod.novo <- lm(y.m ~ trat.m,data=dados.novo)
res.novo <- rstudent(mod.novo)

par(mfrow=c(1,2),pty="s")
plot(fitted(mod.novo),residuals(mod.novo),
     pch=19,col="blue")
abline(h=0,col="red")

qqnorm(res.novo)
qqline(res.novo)
layout(1)

par(mfrow=c(1,2))
library(hnp)
# half-normal plot with simulated envelope
hnp(mod.novo)
# normal plot with simulated envelope
hnp(mod.novo, half = F)



# Testes

#### Normalidade

shapiro.test(rstudent(mod.novo))

#### Homogeneidade de variâncias

bartlett.test(y.m ~ trat.m, data=dados.novo)


####
#- As suposições básicas do modelo foram atendidas

# Teste F


anova(mod.novo)




# Vamos interpretar o quadro da ANOVA

anova(mod)

# Verifica-se que existe efeito de tratamento, ou ainda, 
# que existe diferenças estatisticamente significantes 
# ao nível alpha=5% em pelo menos um contraste de médias.
# valor P = 1.106e-07 < 0,05.



# Croqui na agricultura

trat  <- c("A","B","C","D")
rep(trat,each=4)
set.seed(13); x <- sample(rep(trat,each=5))


# Croqui

croqui <- data.frame(Parcela = 1:20, trat.parcela = x )
croqui


# O croqui de campo (Código)

#- Não se esqueça de instalar o pacote **dae**
  
  

require(dae)
k <- 5 # Número de repetições
a <- 4 # Número de tratamentos
n <- a*k # Número de unidades observacionais
(DIC.unid.obs <- list(Plot = n))
(tratamento <- factor(rep(c(1:a), each=k),
                      labels=c("A","B","C","D")))
DIC.croqui <- designRandomize(recipient = DIC.unid.obs,
                              allocated = tratamento, seed=300)
DIC.croqui # dê as duas últimas colunas para o pesquisador

DIC.layout <- cbind(fac.gen(list(rows = 4,
                                 
                                 columns = 5)),DIC.croqui)

designGGPlot(DIC.layout,
             labels = "tratamento",
             label.size = 5,
             row.factors = "rows",
             column.factors = "columns",
             blockdefinition = cbind(4,5),
             title = "",
             xlab = "",
             ylab = "",
             colour= c("lightblue","lightgreen",
                       "lightgrey",
                       "lightpink"))




# Como instalar o experimento

## Sequência na indústria

a <- 4    # número de tratamentos
k <- 5    # número de repetições
n <- a*k  # número de unidades observacionais
niveis.potencia <- as.factor(c(160,180,200,220))
potencia <- rep(niveis.potencia,each=5)
set.seed(13); x <- sample(1:n)
esquema <- data.frame(sequencia = 1:n, potencia = potencia, seq.inst=x, pot.inst=potencia[x] )

# Esquema
esquema

## Anova Plasma

# Análise exploratória

# Leitura dos dados

dados <- read.csv("https://raw.githubusercontent.com/cibelerusso/PlanejamentodeExperimentos/main/Dados/plasma_pag_62_ed7.csv")

# Dados

dados

head(dados,2)
tail(dados,2)
names(dados)
str(dados)


# O tratamento deve ser transformado em fator

dados$Potencia <- as.factor(dados$Potencia)
str(dados)

levels(dados$Potencia)

d1 <- tapply(dados$Etch.rate,dados$Potencia,summary)
d2 <- tapply(dados$Etch.rate,dados$Potencia,length)
d3 <- tapply(dados$Etch.rate,dados$Potencia,sum)
d4 <- tapply(dados$Etch.rate,dados$Potencia,mean)
d5 <- tapply(dados$Etch.rate,dados$Potencia,var)
d6 <- tapply(dados$Etch.rate,dados$Potencia,sd)


data.frame(n=d2,soma=d3,media=d4,
           variancia=d5,desvio.pad=round(d6,1))


require(ggplot2)
ggplot(dados,aes(Potencia,Etch.rate)) + 
  geom_boxplot(fill="gray",colour="blue")+
  geom_jitter(width=0.0,shape=16,size=2)+
  labs(title="Boxplot com valores observados)",
       x="Potencia",y="Etch Rate")



# Análise utilizando a função **aov** do R

mod1 <- aov(dados$Etch.rate ~  dados$Potencia)


anova(mod1)


mod2 <- lm(dados$Etch.rate ~ dados$Potencia)


## Visualização do quadro da análise de variância
anova(mod2)


mu   <- mean(dados$Etch.rate)
(med.trat <- tapply(dados$Etch.rate,dados$Potencia,mean)) 
tau1 <- med.trat[1] - mu 
tau2 <- med.trat[2] - mu
tau3 <- med.trat[3] - mu
tau4 <- med.trat[4] - mu
theta.soma <-c(mu,tau1,tau2,tau3,tau4)
media.trat <- c(mu+tau1,mu+tau2,mu+tau3,mu+tau4)


# Interpretação

#### Solução com a restrição soma dos efeitos de tratamento igual a zero


data.frame(theta=theta.soma,
           media=c(mu,media.trat))


n <- 20
n.trat <- rep(5,4)
X.mu <- rep(1,length(n))
X1   <- c(rep(1,n.trat[1]),rep(0,n-n.trat[1]))
X2   <- c(rep(0,n.trat[1]),rep(1,n.trat[2]),
          rep(0,n.trat[3]),rep(0,n.trat[4]))
X3   <- c(rep(0,n.trat[1]),rep(0,n.trat[2]),
          rep(1,n.trat[3]),rep(0,n.trat[4]))
X4   <- c(rep(0,n.trat[1]),rep(0,n.trat[2]),
          rep(0,n.trat[3]),rep(1,n.trat[4]))       
X  <- cbind(X.mu,X1,X2,X3,X4)

dim(X)


X


data.frame(X=X,yhat=X%*%theta.soma,y=dados$Etch.rate,erro=dados$Etch.rate-X%*%theta.soma)


(mod_szero <- lm(Etch.rate ~ Potencia, 
                 data = dados,
                 contrasts = list("Potencia" = contr.sum)))


theta.soma

dummy.coef(mod_szero)


mod <- lm(Etch.rate ~ Potencia,
          data=dados,
          contrasts = list("Potencia" = contr.sum))
names(mod)



# $Y$ estimado e o erro estimado


data.frame(Y.obs=dados$Etch.rate,y.hat=X%*%theta.soma, e=dados$Etch.rate-X%*%theta.soma,
           mod_szero$fit,mod_szero$res)

# Tratamento tem que ser definido como fator 

(nivel <- levels(dados$Potencia))

# Restrição: Primeiro nível igual a  zero

dados$Potencia <- relevel(dados$Potencia,ref="160")
model.matrix(~dados$Potencia,ref="160")

dados$Potencia <- relevel(dados$Potencia,ref="180")
levels(dados$Potencia)
model.matrix(~dados$Potencia,ref="180")


dados$Potencia <- relevel(dados$Potencia,ref="200")
levels(dados$Potencia)
model.matrix(~dados$Potencia,ref="200")


dados$Potencia <- relevel(dados$Potencia,ref="220")
levels(dados$Potencia)
(X.4<-model.matrix(~dados$Potencia,ref="220"))



X <- X.4
mat <- t(X)%*%X
det(mat)
## [1] 625
head(X,4)

(est.par <- solve(mat)%*%t(X)%*%dados$Etch.rate)


media <- tapply(dados$Etch.rate,dados$Potencia,mean)

data.frame(est.par=est.par,
           media=media,
           d.med = media-media[1])

mod1 <- aov(Etch.rate ~ Potencia, dados )
coef(mod1)

dummy.coef(mod1)



# Modelo de médias

modelo2 <- lm(Etch.rate ~ Potencia-1,dados)


modelo1 <- lm(Etch.rate ~ Potencia,dados)
modelo2 <- lm(Etch.rate ~ Potencia-1,dados)
library(car)
compareCoefs(modelo1,modelo2)


# Análise gráfica

par(mfrow=c(2,2))
qqnorm(res,pch=19,col="blue")
qqline(res,col="blue")
hist(res,border="blue",col="lightgray")
boxplot(res)
plot(mod1$fit,res,col="blue",pch=19)
abline(h=0)



