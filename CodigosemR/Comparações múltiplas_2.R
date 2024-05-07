# Planejamento de experimentos I
# Comparações múltiplas
# Profa. Cibele Russo

# Experimento

# Carregando pacotes exigidos
require(readxl)
require(ggplot2)
require(hnp)

# Carregando os dados
dados <- read.csv("https://raw.githubusercontent.com/cibelerusso/PlanejamentodeExperimentos/main/Dados/plasma_pag_62_ed7.csv")

# Convertendo Potencia para fator
dados$Potencia <- as.factor(dados$Potencia)

# Modelos lineares
mod.lm  <- lm(Etch.rate  ~ Potencia, data=dados)
mod.aov <- aov(Etch.rate ~ Potencia, data=dados)

# ANOVA
anova(mod.lm)
anova(mod.aov)


# Verificaçãoo de pressupostos
bartlett.test(Etch.rate ~ Potencia, data=dados)
shapiro.test(rstudent(mod.lm))

# Teste de Tukey
TukeyHSD(mod.aov, "Potencia", ordered = TRUE)

# Gráficos
plot(TukeyHSD(mod.aov, "Potencia"), col = "blue")
hnp(mod.lm, resid.type = "student", how.many.out = T)

tapply(dados$Etch.rate, dados$Potencia, mean)
tapply(dados$Etch.rate, dados$Potencia, sd)

# Para ajudar a interpretar os gráficos
tapply(dados$Etch.rate, dados$Potencia, range)

mean.trat <- tapply(dados$Etch.rate, dados$Potencia,mean)
mat.comp <-matrix(c(
  mean.trat[2]-mean.trat[1],
  mean.trat[3]-mean.trat[1],
  mean.trat[4]-mean.trat[1],
  0,
  mean.trat[3]-mean.trat[2],
  mean.trat[4]-mean.trat[2],
  0,
  0,
  mean.trat[4]-mean.trat[3]),3,3)
row.names(mat.comp) <- c(180,200,220)
colnames(mat.comp)  <- c(160,180,200)


coeff1 <- c(-1, 1, 0, 0)
coeff2 <- c(-1, 0, 1, 0)
coeff3 <- c(-1, 0, 0, 1)
coeff4 <- c(0, -1, 1, 0)
coeff5 <- c(0, -1, 0, 1)
coeff6 <- c(0, 0, -1, 1)

dif21 <- sum(coeff1 * mean.trat)
dif31 <- sum(coeff2 * mean.trat)
dif41 <- sum(coeff3 * mean.trat)
dif32 <- sum(coeff4 * mean.trat) 
dif42 <- sum(coeff5 * mean.trat)
dif43 <- sum(coeff6 * mean.trat)

(dif.media <- c(dif21, dif31, dif41, dif32, dif42, dif43))

a <- 4 # Número de tratamentos
k <- 5 # Número de repetições por tratamento (dados balanceados)
n <- length(dados$Etch.rate) # Número de unidades experimentais

alpha <- 0.05 # Nível de significância do teste
qtukey(1-alpha,a,n-a)   # Amplitude studentizada
(QMRes <- sigma(mod.lm)^2)
(DMS <- qtukey(1-alpha,4,n-4)*sqrt(QMRes/k))
int.conf <- cbind(dif.media - DMS, dif.media + DMS)

mat.comp
DMS

abs(mat.comp) > DMS


# Teste de Dunnet

library(DescTools)
## Hollander & Wolfe (1973), 116.
## Mucociliary efficiency from the rate of removal of dust in no
## subjects, subjects with obstructive airway disease, and subj
## with asbestosis.

## Eficiência mucociliar pela taxa de remoção de poeira em nenhum
## sujeitos, sujeitos com doença obstrutiva das vias aéreas e sujeitos
## com asbestose.
x <- c(2,9, 3,0, 2,5, 2,6, 3,2) # indivíduos normais
y <-c(3,8, 2,7, 4,0, 2,4) # com doenças obstrutivas das vias aéreas
z <- c(2,8, 3,4, 3,7, 2,2, 2,0) # com asbestose

# Controle x
DunnettTest(list(x, y, z))

#Controle y
DunnettTest(list(y,x, z))

#Controle z
DunnettTest(list(z,x,y))


## Formula interface
layout(1)
str(airquality)

boxplot(Ozone ~ Month, data = airquality)

DunnettTest(Ozone ~ Month, data = airquality)

DunnettTest(Ozone ~ Month, data = airquality, control="8", conf.level=0.9)


# Teste de Duncan

y <- c(25,17,27,21,15,
       10,-2,12, 4,16,
       18, 8, 4,14, 6,
       23,29,25,35,33,
       11,23, 5,17, 9,
       8,-6, 6, 0, 2)
trat <- as.factor(rep(1:6,each=5))


(dados
  <- data.frame(y=y,trat=trat))


#Normalidade
mod <- aov(y ~ trat, data=dados )
shapiro.test(rstudent(mod))

# Homogeneidade de variâncias
bartlett.test(y ~ trat, data=dados )

# Interpretação Quadro ANOVA
anova(mod)


#Pairwise t.test
attach(airquality)
Month <- factor(Month, labels = month.abb[5:9])
pairwise.t.test(Ozone, Month)


pairwise.t.test(Ozone, Month, p.adjust.method = "bonf")


