# # Exemplo Delineamento Quadrado Latino
# Planejamento de Experimentos I
# Profa. Cibele Russo

# Delineamento Quadrado Latino para Tempo de processo químico usando Lote, Dia e Ingrediente (tratamento)

require(readxl)
## Carregando pacotes exigidos: readxl

dados <- read.csv('https://raw.githubusercontent.com/cibelerusso/PlanejamentodeExperimentos/main/Dados/pag_180_4_22.csv')
str(dados)



dados$Lote <- as.factor(dados$Lote)
dados$Dia <- as.factor(dados$Dia)
dados$Ingrediente <- as.factor(dados$Ingrediente)
str(dados)


boxplot(dados$Tempo ~ dados$Ingrediente)

(media
  <- tapply(dados$Tempo,dados$Ingrediente,mean))

(sd.dados
  <- tapply(dados$Tempo,dados$Ingrediente,sd))
##

(soma.linha
  <- tapply(dados$Tempo,dados$Lote,sum))

(soma.coluna <- tapply(dados$Tempo,dados$Dia,sum))

(soma.Trat
  <- tapply(dados$Tempo,dados$Ingrediente,sum))

mod1 <- aov(dados$Tempo ~ Lote + Dia + Ingrediente, data=dados)
res.s <- rstudent(mod1)

# Normalidade
par(mfrow=c(1,2),pty="s")
qqnorm(res.s,col="blue")
qqline(res.s,col="blue")
boxplot(res.s)

# Homogeneidade de variâncias

par(mfrow=c(1,2))
require(lattice)
dotplot(dados$Tempo ~ dados$Ingrediente)

# Homogeneidade de variâncias

plot(mod1$fit,res.s,xlab="y.hat")
abline(h=0)


# Shapiro Wilk

shapiro.test(res.s)

# Homogeneidade de variâncias

bartlett.test(dados$Tempo ~ dados$Ingrediente)

# Teste para independência

library(car)
durbinWatsonTest(mod1)

# Quadro ANOVA - cálculos

(n <- length(dados$Tempo))
(correcao <- n*mean(dados$Tempo)^2) 
(SQTotal  <- sum(t(dados$Tempo)%*%dados$Tempo)-correcao)
(SQLinha  <- sum(soma.linha^2)/5-correcao)
(SQTrat   <- sum(soma.Trat^2)/5-correcao)
(SQColuna <- sum(soma.coluna^2)/5-correcao)
(SQRes    <- SQTotal - SQLinha - SQColuna-SQTrat)


(SQRes    <- SQTotal - SQLinha - SQColuna-SQTrat)
p <- 5
(gl.Trat <- p-1)
(gl.Res  <- (p-2)*(p-1))
(QMTrat <- SQTrat/gl.Trat)
(QMRes <- SQRes/gl.Res)


(Fcalc <- QMTrat/QMRes)
(valor.p <- pf(Fcalc,gl.Trat,gl.Res,lower.tail=F))


anova(mod1)
summary(mod1,intercept = T)


layout(1)
graf <- TukeyHSD(mod1,cterms="Ingrediente",order=T)
model.tables(mod1, cterms="Ingrediente",type="means")
(gl.Res <- df.residual(mod1))
(QMRes  <- sigma(mod1)^2)
alpha  <- 0.05
a <- 5 # Número de tratamentos


names(graf)
class(graf$Ingrediente)
graf$Ingrediente[,1]

q   <- qtukey(0.95, a, gl.Res)
(dms <- q*sqrt(QMRes/5))

graf$Ingrediente[,1] > dms


require(agricolae)
Teste <- HSD.test(mod1,"Ingrediente",group=T)
names(Teste)
Teste$statistics
Teste$means

Teste$comparison # Não apresenta porque group=T
Teste$groups     # Apresenta porque group=T


plot(Teste)


