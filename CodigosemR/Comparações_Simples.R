# Aula prática 1 - Comparações simples
# Planejamento de experimentos I
# Profa. Cibele Russo


library(ggplot2)

# Exemplo 1

modificada<-c(16.85,16.40,17.21,16.35,16.52,17.04,16.96,17.15,16.59,16.57)
nao_modificada<-c(17.50,17.63,18.25,18.00,17.86,17.75,18.22,17.02,17.08,18.15)

boxplot(modificada, nao_modificada)


# Criar um dataframe com os dados
dados <- data.frame(
  grupo = c(rep("Modificada", length(modificada)), rep("Não Modificada", length(nao_modificada))),
  valor = c(modificada, nao_modificada)
)

# Criar o boxplot usando ggplot
ggplot(dados, aes(x = grupo, y = valor)) +
  geom_boxplot() +
  labs(x = "Fórmula", y = "Resistência", title = "Boxplot da resistência por grupos") +
  theme_bw()


# Teste de normalidade (Shapiro-Wilk)
shapiro.test(modificada)
shapiro.test(nao_modificada)

# Teste de comparação de médias
t.test(modificada, nao_modificada)


# E se a normalidade fosse rejeitada?
# Teste de Mann-Whitney
wilcox.test(modificada, nao_modificada)


## Exemplo 2

maquina1 <- c(30.9, 30.9, 30.8, 30.7, 30.9, 30.6, 30.8, 30.9, 30.7, 30.9, 30.7,31.0)
maquina2 <- c(30.8, 30.9, 30.7, 30.5, 30.5, 30.6, 30.7, 30.3, 30.6, 30.7)

boxplot(maquina1, maquina2)

# Criar um dataframe com os dados
dados <- data.frame(
  grupo = c(rep("Máquina 1", length(maquina1)), rep("Máquina 2", length(maquina2))),
  valor = c(maquina1, maquina2)
)

# Criar o boxplot usando ggplot
ggplot(dados, aes(x = grupo, y = valor)) +
  geom_boxplot() +
  labs(x = "Máquina", y = "Volume envasado", title = "Boxplot de volume envasado") +
  theme_bw()


# Teste de normalidade (Shapiro-Wilk)
shapiro.test(maquina1)
shapiro.test(maquina2)


# não fazer o teste para as amostras juntas
# shapiro.test(c(maquina1, maquina2))


# Teste de comparação de médias
t.test(maquina1, maquina2)




# Exemplo 3
# Teste pareado

diferença <-c(2.3,0.5,1.4,-0.4,2.6,-1.7,0.9, 1.2, -2.3,0.5)

boxplot(diferença, horizontal = T)

shapiro.test(diferença)

t.test(diferença-0.2, alternative = 'greater')

t.test(diferença-(-1), alternative = 'greater')


# Exemplo 4

termopar <- c(553, 552, 567, 579, 550, 541, 537, 553, 552, 546, 538, 553, 581, 539, 529)

# Teste de normalidade
shapiro.test(termopar)

t.test(termopar-560)

# Criar o gráfico de histograma usando ggplot
histograma <- ggplot(data = NULL, aes(x = termopar)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", aes(y = ..count..)) +
  labs(title = "Histograma do Termopar",
       x = "Medições",
       y = "Frequência") +
  theme_minimal()

print(histograma)


# Teste t para uma única amostra
# H0: mu = 560
# H1: mu < 560

t.test(termopar~1, mu=560, alternative = 'less')


# Exemplo 5 - Comparação de variâncias

grupo1<-c(35, 35, 37, 33, 31, 33)
grupo2<-c(32, 34, 34, 31, 32)

var(grupo1)
var(grupo2)

Fobs <- var(grupo1)/var(grupo2)

# Como 0.1353 < Fobs < 9.36, não rejeito a igualdade de variâncias
# ao nível de signficância alpha = 0.05


# Teste de Bartlett
resultado_bartlett <- var.test(grupo1, grupo2)

# Exibir os resultados do teste
print(resultado_bartlett)

shapiro.test(grupo1)
shapiro.test(grupo2)

# Como nao rejeitamos a normalidade dos dados de cada grupo, 
# podemos aplicar o teste t, assumindo variâncias iguais

t.test(grupo1, grupo2, var.equal = T)

# t = 1.2836, df = 9, p-value = 0.2314
# não rejeitamos a igualdade das médias


# Dados simulados

grupoA <- rnorm(20, 3, 2)
grupoB <- rnorm(20, 5, 2)

t.test(grupoA, grupoB)

