# Planejamento de Experimentos I
# Profa. Cibele Russo
# Modelo de componentes de variância

# Geração de dados com efeitos fixos e aleatórios com o pacote nlme


#install.packages('nlme')
library(nlme)

set.seed(123)

a <- 3 # número de níveis do fator A
b <- 10 # número de níveis do fator B
replicas <- 5

intercepto <- 10
efeito_A <- c(2, -3, 1)
sigma_aleatorio_B <- 5
sigma_residual <- 2

A <- as.factor(rep(1:a, each = b  * replicas))
B <- as.factor(rep(rep(1:b, each = replicas), times = a))

efeito_aleatorio_B <- rnorm(n_niveis_B, mean = 0, sd = sigma_aleatorio_B)
efeito_aleatorio_B <- rep(efeito_aleatorio_B, each = replicas, times = a)

erro <- rnorm(a * b * replicas, mean = 0, sd = sigma_residual)

y <- intercepto + efeito_A[A] + efeito_aleatorio_B + erro

dados <- data.frame(A = as.factor(A), B = as.factor(B), y = y)

dados

# Ajuste do modelo com nlme
modelo_nlme <- lme(fixed = y ~ A, random = ~ 1 | B, data = dados)

summary(modelo_nlme)

# Componentes de variância
var_components_nlme <- VarCorr(modelo_nlme)
print(var_components_nlme)


