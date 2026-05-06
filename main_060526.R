# nolint start

# ---------------------------------------------------------------------
# Modelos lineales simples
# ---------------------------------------------------------------------

getwd()

# Test: Y es cuantitativa y X es factor
set.seed(123)
y <- rnorm(15, 15, 15)
x <- rep(c(0, 1), times = c(7, 8))
z <- rep(c(0, 1, 0, 1), times = c(3, 5, 4, 3))
o <- data.frame(Y = y, X = as.factor(x), Z = as.factor(z))
summary(o)
with(o, table(X, Z))

m2 <- lm(Y ~ X + Z, data = o)
m2 <- lm(Y ~ X + Z + X * Z, data = o)
m2 %>% summary()
m2$coefficients

m <- lm(Y ~ X, data = o)
m %>% summary()

m <- lm(Y ~ Z, data = o)
m %>% summary()

with(o, aggregate(Y ~ X, FUN = mean))
with(o, aggregate(Y ~ Z, FUN = mean))
with(o, aggregate(Y ~ X + Z, FUN = mean))


# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------
set.seed(1)
n <- 100
X <- runif(n, 0, 10)
Z <- runif(n, 0, 10)

Y <- 5 + 2 * X + 3 * Z + rnorm(n, 0, 2)

df <- data.frame(Y, X, Z)
df %>% head()

m1 <- lm(Y ~ X + Z, data = df)
m1 %>% summary()

g1 <-
  ggplot(df, aes(x = X, y = Y, color = Z)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

Y2 <- 5 + 2 * X + 3 * Z + 1.5 * X * Z + rnorm(n, 0, 2)
df$Y2 <- Y2

m2 <- lm(Y2 ~ X * Z, data = df)
m2 %>% summary()

g2 <-
  ggplot(df, aes(x = X, y = Y2, color = Z, group = Z)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

grid.arrange(g1, g2, ncol = 2)

df %>% head()

df %>% ggplot(aes(X, Y, group = Z)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Binaria
set.seed(2)
n <- 200
X <- runif(n, 0, 10)
Z <- factor(sample(c("A", "B"), n, replace = TRUE))

eta <- -3 + 0.8 * X + ifelse(Z == "B", 1, 0)
p <- 1 / (1 + exp(-eta))
Y <- rbinom(n, 1, p)

df2 <- data.frame(Y, X, Z)
df2 %>% head

m3 <- glm(Y ~ X + Z, family = binomial, data = df2)
m3 %>% summary()

ggplot(df2, aes(x = X, y = Y, color = Z)) +
  geom_jitter(height = 0.05, width = 0) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  theme_minimal()


# =========================================================
# SCRIPT ÚNICO: comparación de 3 escenarios (con/sin interacción)
# =========================================================

set.seed(123)

library(ggplot2)
library(dplyr)
library(patchwork)  # para combinar gráficos

# =========================================================
#  CASO 1: Y numérica, X y Z numéricos
# =========================================================

n1 <- 150
df1 <- data.frame(
  X = runif(n1, 0, 10),
  Z = runif(n1, 0, 10)
)

# Sin interacción
df1$Y_add <- 5 + 2*df1$X + 3*df1$Z + rnorm(n1, 0, 2)

# Con interacción
df1$Y_int <- 5 + 2*df1$X + 3*df1$Z + 1.5*df1$X*df1$Z + rnorm(n1, 0, 2)

# Discretizamos Z solo para visualizar líneas
df1$Zg <- cut(df1$Z, breaks = 3, labels = c("Z bajo", "Z medio", "Z alto"))

p1 <- ggplot(df1, aes(X, Y_add, color = Zg)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Caso 1 (sin interacción)") +
  theme_minimal()

p2 <- ggplot(df1, aes(X, Y_int, color = Zg)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Caso 1 (con interacción)") +
  theme_minimal()

# =========================================================
# CASO 2: Y binaria, X numérica, Z factor
# =========================================================

n2 <- 200
df2 <- data.frame(
  X = runif(n2, 0, 10),
  Z = factor(sample(c("A","B"), n2, replace = TRUE))
)

# Sin interacción
eta_add <- -3 + 0.8*df2$X + ifelse(df2$Z=="B", 1, 0)
p_add <- 1/(1+exp(-eta_add))
df2$Y_add <- rbinom(n2, 1, p_add)

# Con interacción
eta_int <- -3 + 0.8*df2$X + ifelse(df2$Z=="B", 1, 0) + 0.5*df2$X*(df2$Z=="B")
p_int <- 1/(1+exp(-eta_int))
df2$Y_int <- rbinom(n2, 1, p_int)

p3 <- ggplot(df2, aes(X, Y_add, color = Z)) +
  geom_jitter(height = 0.05, alpha = 0.6) +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE) +
  ggtitle("Caso 2 (sin interacción)") +
  theme_minimal()

p4 <- ggplot(df2, aes(X, Y_int, color = Z)) +
  geom_jitter(height = 0.05, alpha = 0.6) +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE) +
  ggtitle("Caso 2 (con interacción)") +
  theme_minimal()

# =========================================================
# CASO 3: Y binaria, X y Z factores
# =========================================================

n3 <- 300
df3 <- data.frame(
  X = factor(sample(c("0","1"), n3, replace = TRUE)),
  Z = factor(sample(c("0","1"), n3, replace = TRUE))
)

# Sin interacción (estructura aditiva)
p_add <- ifelse(df3$X=="0" & df3$Z=="0", 0.2,
         ifelse(df3$X=="1" & df3$Z=="0", 0.4,
         ifelse(df3$X=="0" & df3$Z=="1", 0.6, 0.8)))
df3$Y_add <- rbinom(n3, 1, p_add)

# Con interacción (rompe paralelismo)
p_int <- ifelse(df3$X=="0" & df3$Z=="0", 0.2,
         ifelse(df3$X=="1" & df3$Z=="0", 0.5,
         ifelse(df3$X=="0" & df3$Z=="1", 0.6, 0.9)))
df3$Y_int <- rbinom(n3, 1, p_int)

p5 <- ggplot(df3, aes(X, Y_add, color = Z, group = Z)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  ggtitle("Caso 3 (sin interacción)") +
  theme_minimal()

p6 <- ggplot(df3, aes(X, Y_int, color = Z, group = Z)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  ggtitle("Caso 3 (con interacción)") +
  theme_minimal()

# =========================================================
# MOSTRAR TODO JUNTO
# =========================================================

(p1 | p2) / (p3 | p4) / (p5 | p6)


































# nolint end
