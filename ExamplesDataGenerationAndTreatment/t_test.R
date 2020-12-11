#Ejemplo 1
set.seed(27)
(HgTot <- round(rnorm(8, 470, 30), 0)) #g kg^-1
mu <- 500
shapiro.test(HgTot)

t.test(x = HgTot, mu = mu, alternative = 'less')

#Ejemplo 2
set.seed(0)
(Analista1 <- round(rnorm(6, 1.3, 0.1), 2))
(Analista2 <- round(rnorm(6, 1.5, 0.2), 2))
shapiro.test(Analista1);shapiro.test(Analista2)
t.test(x = Analista1, y = Analista2)      

#Ejemplo 3
PM_10_a <- c(12, 23, 8, 19, 33, 50, 26, 21)
PM_10_b <- c(12, 24, 15, 26, 26, 62, 34, 28)
t.test(PM_10_a, PM_10_b, alternative = 'less')
t.test(PM_10_a, PM_10_b, paired = TRUE, alternative = 'less')
