# # Exemplo Delineamento em Blocos completos casualizados
# Planejamento de Experimentos I
# Profa. Cibele Russo

# Experimento competição de variedades de batatas


bloco <- gl(4,8) #Quatro blocos 8 tratamentos
trat <- gl(8,1,32)
prod <- c( 9.2,21.1,22.6,15.4,12.7,20.0,23.1,18.0,
           13.4,27.0,29.9,11.9,18.0,21.1,24.2,24.6,
           11.0,26.4,24.2,10.1,18.2,20.0,26.4,24.0,
           9.2,25.7,25.1,12.3,17.1,28.0,16.3,24.6)
dados <- data.frame(bloco=bloco,trat=trat,prod=prod)
#trat = variedades

View(dados)
# Número de unidades experimentais
(n<-length(dados$bloco))

# Quantos blocos? E quantas observações por bloco?
tapply(dados$prod,dados$bloco,length)
# 4 blocos e 8 observações por bloco
b <- 4
# Quantos tratamentos? E quantas observações dos tratamento
tapply(dados$prod,dados$trat,length)

# 8 tratamentos repetidos nos 4 blocos
a <- 8

# Soma por bloco
(soma.bloco <- tapply(dados$prod,dados$bloco,sum))
# Soma por tratamento
(soma.trat <- tapply(dados$prod,dados$trat,sum))
# Soma total
(soma.total <- sum(prod))


#Média por tratamento
(media <- tapply(dados$prod,dados$trat,mean))
#Média geral
(media.geral <- mean(dados$prod))
boxplot(dados$prod ~ dados$trat )

#Aparentemente existem diferenças entre médias de tratamentos

# Verificação das pressuposições básicas
mod1 <- aov(dados$prod ~ dados$bloco + dados$trat)
names(mod1)
res <- rstudent(mod1)

#Interprete
par(mfrow=c(1,2))
qqnorm(res)
qqline(res)

# Exiba os cinco passos do teste hipóteses
shapiro.test(res)
boxplot(res)
layout(1)


# Homogeneidade de variâncias
# Interprete
plot(mod1$fit,res)
abline(h=0)
require(lattice)
dotplot(dados$prod ~ dados$trat)

# Exiba os 5 passos do teste de hipóteses
bartlett.test(dados$prod, dados$trat)
require(car)

#?leveneTest
leveneTest(dados$prod,dados$trat)

# Independência
#?durbinWatsonTest
durbinWatsonTest(lm(dados$prod ~ dados$bloco + dados$trat))
# Conclua sobre a verificação das pressuposições básicas

# Quadro da ANOVA
anova(mod1)
summary(mod1,intercept = T)
#Conclusão: Existe pelo menos um contraste de duas médias diferente de zero.

## Realizando os cálculos
# Compare com o quadro da ANOVA

(correcao <- round(soma.total^2/n,0))
correcao <- soma.total^2/n
(SQBloco<- round(sum(soma.bloco^2)/8-correcao,0))
(SQTrat<- round(sum(soma.trat^2)/4-correcao,0))
(SQTotal <- sum(t(dados$prod)%*%dados$prod)-correcao)
(SQRes <- round(SQTotal - SQBloco - SQTrat,0))

#Número de comparações
#m número de comparações duas a duas
a <- 8
(m <- a*(a-1)/2)

# Tukey base
TukeyHSD(mod1)
plot(TukeyHSD(mod1),las=1)

# Médias em ordem decrescente
(medias <- tapply(dados$prod,dados$trat,mean))
sort(medias,decreasing = T)

# A média do tratamento 3 recebe a letra a
# A média do tratamento 3 difere da média do tratamento 2? Não! Recebe a letra a

out <- TukeyHSD(mod1)
names(out)
out$`dados$trat`
dim(out$`dados$trat`)
out$`dados$trat`[1,]

# Sim, a média do tratamento 3 difere da média do
# tratamento 8? Não! Recebe a letra a

sort(medias,decreasing = T)
# A média do trat 3 difere da média do trat 7? sim

# e por ai vai até finalizar

#Teste Tukey agricolae
require(agricolae)
(gl.Res <- df.residual(mod1))
(QMRes <- sigma(mod1)^2)
out <- HSD.test(dados$prod, dados$trat, DFerror=gl.Res,
                MSerror=QMRes,alpha = 0.05,
                group=TRUE, main = NULL,
                unbalanced=FALSE,console=FALSE)
out$statistics
media.geral

# Coeficiente de variação
(CV <- sqrt(QMRes)/media.geral*100)
alpha <- 0.05

# q tabelado
(q.crit <- qtukey(1-alpha,a,gl.Res))

#Amplitude studentizada
#abs(yb_i-yb_j)/sqrt(QMREs/b)
#abs(yb_i-yb_j)/sqrt(QMREs/b) >qt_crit
#abs(yb_i-yb_j) > qt_crit*sqrt(QMREs=MSError/b)

#Diferença mínima significativa do Teste de Tukey
(MSD <- q.crit*sqrt(QMRes/b))
print(out$groups)

out.comp <- HSD.test(dados$prod,dados$trat,group=F,
                     MSerror = QMRes,DFerror = gl.Res)

names(out)
print(out$statistics)
print(out$comparison)
out.graf <- HSD.test(dados$prod,dados$trat,group=T,
                     MSerror = QMRes,DFerror = gl.Res)

# Veja as diferenças e similaridades
plot(out,las=1)
#plot(out.comp,las=1)
plot(out.graf,las=1)

