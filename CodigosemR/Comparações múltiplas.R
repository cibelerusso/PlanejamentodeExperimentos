# Códigos de https://www.uel.br/projetos/experimental/pages/arquivos/Dic_patol.html

rm(list=ls())

(croqui = expand.grid(trat=c('M0','M1','M4','D0'), rep=1:6))



Conc = c(400.6, 390.7, 420.4, 348.5,
         387.2, 408.3, 402.8, 345.4,
         399.6, 396.1, 419.9, 367.8,
         397.8, 398.2, 394.8, 357.8,
         395.4, 393.6, 410.1, 357.0,
         401.4, 389.2, 394.7, 362.1)

dados = data.frame(croqui, resp=Conc)

str(dados)

head(dados)

tail(dados)

names(dados)

# Estat?stica Descritiva 

# Geral
media = mean(dados$resp)
variancia = var(dados$resp)
desvio = sd(dados$resp)
CV = desvio / media * 100

desc.geral = data.frame(Média=media, Variância=variancia, Desvio=desvio, CV=CV)
rownames(desc.geral) = c('Concentra??o de C?lcio')
desc.geral

# Por tratamento
medias = tapply(dados$resp, dados$trat, mean)
variancias = tapply(dados$resp, dados$trat, var)
desvios = tapply(dados$resp, dados$trat, sd)
cv = desvios / medias * 100

# Descritiva por tratamento
tab.desc = data.frame(Médias=medias, Variâncias=variancias, Desvios=desvios, CV=cv)
tab.desc

# Gráfico de Caixas 
boxplot(dados$resp ~ dados$trat, las=1, col='lightyellow', xlab='Misturas', 
        ylab='Concentração de cálcio (mg)')

points(medias, pch='+', cex=1.5, col='red')

# Análise de Variância 
mod = aov(resp ~ trat, data=dados)
summary(mod)

# Teste de Tukey 
tukey = TukeyHSD(mod)
tukey

plot(tukey, las=1, col='blue')

# Pressupostos -

# Normalidade dos erros
shapiro.test(mod$res)

# Homogeneidade de Variâncias
# Teste de Bartlett
bartlett.test(mod$res ~ dados$trat)

# Teste de Levene
library(car)
leveneTest(mod$res ~ dados$trat, center=mean)

leveneTest(mod$res ~ dados$trat, center=median)

# Independência dos erros
plot(mod$res, las=1, pch=19, col='red', ylab='Res?duos')

# Ajuste do modelo usando o Q-Q Plot
library(hnp)
HNP = hnp(mod, print.on=TRUE, plot=FALSE)

plot(HNP, lty=c(2,3,2), pch=20, cex=1.2, col=c(2,1,2,1))

# Teste t                                            -
#                                                    -
# Caso as hip?teses sejam feitas a priori, deve-se   -
# utilizar o teste t para os contrastes. Suponha que -
# se queira verificar se existe diferen?a entre os   -
# seguintes contrastes:                              -
#                                                    -
# Y1 = 3mu1 - (mu2 + mu3 + mu4)                      -
# Y2 = 2mu2 - (mu3 + mu4)                            -
# Y3 = mu3 - mu4                                     -
#
library(gmodels)
(Contrastes = rbind(" 3D0 vs (M0+M1+M4) " = c(3, -1, -1, -1),
                    " 2M0 vs (M1 + M4) "  = c(0,  2, -1, -1),
                    " M1 vs M4 "          = c(0,  0,  1, -1)))

fit.contrast(mod, "trat", Contrastes, conf=0.95)

# Teste F                                       -
# Desdobrando a Soma de Quadrado de Tratamentos -
mod.c = aov(resp ~ trat, data=dados, contrast=list(trat=make.contrasts(Contrastes))) 
summary(mod.c, split=list(trat=1:3))

Desdobramento = summary(mod.c, split=list(trat = c(" 3D0 vs (M0 + M1 + M4) " = 1, 
                                                   " 2M0 vs (M1 + M4) "      = 2,
                                                   " M1 vs M4 "              = 3)))
Desdobramento

mod1 = lm(resp ~ trat, data=dados)
anova(mod1)

summary(mod1)

library(agricolae)
teste.HSD = with(dados, HSD.test(mod, 'trat', main='Concentração de Cálcio', console=T))

with(dados, HSD.test(mod, 'trat', main='Concentração de Cálcio', alpha=0.01, console=T))

bar.group(teste.HSD$groups)

bar.group(teste.HSD$groups, ylim=c(0,500))

bar.group(teste.HSD$groups, density=10, border="blue", ylim=c(0,500), 
          las=1, angle=45, col='red', main='Teste de Tukey',
          xlab='Misturas', ylab='Concentração de Cálcio (mg)')
abline(h=0, col='black')

teste.duncan = with(dados, duncan.test(mod, "trat", main="Concentração de Cálcio (mg)", console=T))

with(dados, duncan.test(mod, "trat", alpha=0.01, console=T))

bar.group(teste.duncan$groups, ylim=c(0,500), density=10, border="blue",
          las=1, main='Teste de Duncan',
          xlab='Misturas', ylab='Concentração de Cálcio (mg)')
abline(h=0, col='black')

teste.snk = with(dados, SNK.test(mod, "trat", main="", console=T))

with(dados, SNK.test(mod, "trat", group=FALSE, console=T))

bar.group(teste.snk$groups, density=10, border="blue", ylim=c(0,500), 
          las=1, angle=45, col='red', main='Teste de Student-Newman-Keuls',
          xlab='Misturas', ylab='Concentração de Cálcio (mg)')
abline(h=0, col='black')

teste.scheffe = with(dados, scheffe.test(mod, "trat", main="", console=T))

bar.group(teste.scheffe$groups, ylim=c(0,500), density=10, border="blue",
          las=1, main='Teste Scheffé',
          xlab='Tipos de Ração', ylab='Peso médio das aves (kg)')
abline(h=0, col='black')

teste.bonferroni = with(dados, LSD.test(mod, "trat", p.adj="bonferroni", main="Concentração de cálcio (mg)", 
                                        console=T))

bar.group(teste.bonferroni$groups, ylim=c(0,500), density=10, border="blue", space=.7,
          las=1, main='Teste de Bonferroni',
          xlab='Misturas', ylab='Concentração de Cálcio (mg)')
abline(h=0, col='black')

#install.packages('asbio')
require(asbio)

with(dados, bonfCI(resp, trat, conf.level = 0.95))

with(dados, pairw.anova(x=trat, y=resp, method="bonf"))

require(multcomp)

teste.dunnett = with(dados, glht(mod, linfct = mcp(trat = c("M4 - D0 = 0", 
                                                            "M0 - D0 = 0",
                                                            "M1 - D0 = 0"))))

summary(teste.dunnett)  

confint(teste.dunnett, level = 0.95)

with(dados, pairw.anova(y=resp, x=trat, control="D0", method="dunnett"))



                                          