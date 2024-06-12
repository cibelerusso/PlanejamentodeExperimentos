# ANOVA modelo fatorial - delineamento em blocos completos casualizados e Quadrado Latino
# Planejamento de experimentos I
# Profa. Cibele Russo

# Exemplo 5.6 Intensidade na detecção radar 

# Entrada de dados
dados <- read.csv('https://raw.githubusercontent.com/cibelerusso/PlanejamentodeExperimentos/main/Dados/Intensidade_deteccao.csv')
#View(dados)

#install.packages('nlme')
library(nlme)

# Dados
#View(dados)

dados$Filtro <- factor(dados$Filtro)
dados$Desordem.no.solo <- factor(dados$Desordem.no.solo)
dados$Operador <- factor(dados$Operador)

# Visualização dos dados
boxplot(Intensidade.na.detecção ~ Filtro, data = dados, main = "Intensidade na Detecção por Tipo de Filtro")
boxplot(Intensidade.na.detecção ~ Desordem.no.solo, data = dados, main = "Intensidade na Detecção por Desordem no Solo")

# Visualização dos dados em painel
#install.packages('Hmisc')
#library('Hmisc')

attach(dados)

library('lattice')
xyplot(Intensidade.na.detecção ~ Filtro | Operador, pch=16)
xyplot(Intensidade.na.detecção ~ Desordem.no.solo | Operador, pch=16)
xyplot(Intensidade.na.detecção ~ Filtro + Desordem.no.solo | Operador, pch=16)


# Modelo fatorial com blocos
mod <- lme(fixed = Intensidade.na.detecção ~ Filtro * Desordem.no.solo, random = ~ 1 | Operador, data = dados)
summary(mod)

res<-residuals(mod)

# Resíduos vs Valores Ajustados
plot(fitted(mod), residuals(mod), pch=19, col="blue", main="Resíduos vs Ajustados")
abline(h=0, col="red")


# QQ-Plot
qqnorm(res, col="blue", pch=19, main="QQ-Plot dos Resíduos")
qqline(res)


# Testes

#### Normalidade dos erros

shapiro.test(residuals(mod))


# Tabela ANOVA
anova(mod)


##### Exemplo 5.6 com delineamento Quadrado Latino

# Entrada de dados

dados <- read.csv('https://raw.githubusercontent.com/cibelerusso/PlanejamentodeExperimentos/main/Dados/Intensidade_deteccao_QL%20.csv')

#install.packages('nlme')
library(nlme)

# Dados
#View(dados)

dados$Tipo.de.filtro <- factor(dados$Tipo.de.filtro)
dados$Desordem.no.solo <- factor(dados$Desordem.no.solo)
dados$Operador <- factor(dados$Operador)
dados$Dia <- factor(dados$Dia)


# Visualização dos dados
boxplot(Intensidade.na.detecção ~ Tipo.de.filtro, data = dados, main = "Intensidade na Detecção por Tipo de Filtro")
boxplot(Intensidade.na.detecção ~ Desordem.no.solo, data = dados, main = "Intensidade na Detecção por Desordem no Solo")
boxplot(Intensidade.na.detecção ~ Tipo.de.filtro + Desordem.no.solo, data = dados, main = "Intensidade na Detecção por Desordem no Solo")


# Modelo fatorial

mod1 <- lme(fixed = Intensidade.na.detecção ~ Tipo.de.filtro * Desordem.no.solo, random = ~ 1 | Operador/Dia, data = dados)
summary(mod1)

mod2 <- lme(fixed = Intensidade.na.detecção ~ Tipo.de.filtro * Desordem.no.solo, random = ~ 1 | Dia/ Operador, data = dados)
summary(mod2)

res <- residuals(mod1)


# Resíduos vs Valores Ajustados
plot(fitted(mod1), residuals(mod1), pch=19, col="blue", main="Resíduos vs Ajustados")
abline(h=0, col="red")

# QQ-Plot
qqnorm(res, col="blue", pch=19, main="QQ-Plot dos Resíduos")
qqline(res)



# Testes

#### Normalidade dos erros

shapiro.test(residuals(mod1))


# Tabela ANOVA
anova(mod1)

mod1$groups

mod1$modelStruct

mod1$varFix

mod1$sigma

mod1$terms

mod1$contrasts


