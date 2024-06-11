# ANOVA modelo fatorial - delineamento em blocos completos casualizados e Quadrado Latino
# Planejamento de experimentos I
# Profa. Cibele Russo

# Exemplo 5.6 Intensidade na detecção radar 

# Entrada de dados
dados <- read.csv('https://raw.githubusercontent.com/cibelerusso/PlanejamentodeExperimentos/main/Dados/Intensidade_deteccao.csv')

#install.packages('nlme')
library(nlme)

# Dados
#View(dados)

dados$Filtro <- factor(dados$Filtro)
dados$Desordem.no.solo <- factor(dados$Desordem.no.solo)
dados$Operador <- factor(dados$Operador)

dados$Intensidade.na.detecção

# Modelo fatorial

mod <- lme(fixed = Intensidade.na.detecção ~ Filtro * Desordem.no.solo, random = ~ 1 | Operador, data = dados)
summary(mod)

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

# Modelo fatorial

mod <- lme(fixed = Intensidade.na.detecção ~ Tipo.de.filtro * Desordem.no.solo, random = ~ 1 | Operador/Dia, data = dados)
summary(mod)

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

