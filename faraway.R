# nolint start
getwd()
library(faraway)
p <- pima
p %>%
  str() %>%
  capture.output() %>%
  writeLines(., "pstr.txt")
p %>% head()

hist(p$pregnant)
hist(p$insulin)
hist(p$age)
p %>%
  filter(age > 60) %>%
  arrange(age)

p %>%
  mutate(cinsulin = ifelse(insulin == 0, 0, 1)) %>%
  group_by(cinsulin) %>%
  with(., table(test, cinsulin))

hist(p$diastolic)

p %>% ggplot(aes(glucose, pregnant)) +
  geom_jitter() +
  geom_smooth(se = FALSE)

p %>% ggplot(aes(glucose, diastolic)) +
  geom_jitter() +
  geom_smooth(se = FALSE)

p %>% ggplot(aes(glucose, triceps)) +
  geom_jitter() +
  geom_smooth(se = FALSE)

p %>% ggplot(aes(diastolic, triceps)) +
  geom_jitter() +
  geom_smooth(se = FALSE)

p %>%
  mutate(test = factor(test)) %>%
  ggplot(aes(test, diastolic)) +
  geom_jitter(width = .1, alpha = .2) +
  geom_boxplot(width = .2)


p$bmi %>%
  is.na() %>%
  sum()

cp <-
  p %>%
  mutate(
    diastolic = ifelse(diastolic == 0, NA, diastolic),
    glucose = ifelse(glucose == 0, NA, glucose),
    triceps = ifelse(triceps == 0, NA, triceps),
    insulin = ifelse(insulin == 0, NA, insulin),
    bmi = ifelse(bmi == 0, NA, bmi)
  )

cp$test <- cp$test %>% factor()
levels(cp$test) <- c("negative", "positive")
str(cp)
summary(cp)

g1 <- hist(cp$diastolic)
plot(density(cp$diastolic, na.rm = TRUE))
g3 <- plot(sort(cp$diastolic))


g1 <- cp %>% ggplot(aes(diastolic)) +
  geom_histogram()
g2 <- cp %>% ggplot(aes(diastolic)) +
  geom_density()
g3 <- cp %>%
  arrange(diastolic) %>%
  mutate(order = seq_along(diastolic)) %>%
  ggplot(aes(order, diastolic)) +
  geom_point()

grid.arrange(g1, g2, g3, ncol = 3)

a1 <-
  cp %>% ggplot(aes(diastolic, diabetes)) +
  geom_point() +
  geom_smooth(se = FALSE)

a2 <-
  cp %>% ggplot(aes(test,diabetes)) +
  geom_boxplot()

grid.arrange(a1,a2,ncol=2)
















# nolint end
