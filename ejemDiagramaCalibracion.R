source('functions.R')
## TRM DIAGRAMA DE CALIBRACIÓN
##LITHIUM AAS DATA 19-05-21 - UNAM
cStock <- 129.5 * 0.187872 * 0.99 / 0.1200962
Std7   <- cStock * 0.7495 / 60.1995

LithiumF  <- data.frame(Conc = c(0.0000, 0.4725, 1.2025, 2.4371, 4.8029, 9.6060) * Std7 /
                          c(12.0000, 11.8008, 12.0137, 11.9934, 11.9848, 12.0406),
                        Signal = c(0.000, 0.019, 0.046, 0.095, 0.188, 0.364))

LiF <- ggplot(data = LithiumF, aes(x = Conc, y = Signal)) +
  theme_bw() + 
  labs(y = 'Absorbancia (UA)', x = expression(paste('Concentración (mg k', g^{-1}, ')'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_y_continuous(labels = function(x) sprintf("%.2f", x), breaks = seq(0, .37, .07)) +
  scale_x_continuous(limits = c(-1, 3), labels = function(x) sprintf("%.2f", x), breaks = seq(0, 2, .4)) +
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 0.37)) +
  geom_smooth(method = 'lm', formula = y ~ x, fullrange = TRUE, color = 'black', size = 0.4, 
              level = 0.999, fill = '#97d7e4', alpha = 0.8) +
  geom_point(size = 3, shape = 16)  +
  geom_segment(aes(x = -1, y = 0.285, xend = 1.627, yend = 0.285), size = 0.3, linetype = "dashed") +
  geom_segment(aes(x = 1.627, y = -1, xend = 1.627, yend = 0.285), size = 0.3, linetype = "dotted") +
  geom_segment(aes(x = 1.48, y = -1, xend = 1.48, yend = 0.285), size = 0.3, linetype = "dotted")  +
  geom_segment(aes(x = 1.55, y = -1, xend = 1.55, yend = 0.285), size = 0.3, linetype = "dashed") +
  geom_segment(aes(x = 1.48, y = 0, xend = 1.627, yend = 0), size = 0.3,
               arrow = arrow(ends = 'both', length = unit(x = 7, units = 'pt')))
print(LiF)
savepdf(LiF, name = 'diagrCalibLi999', h = 4, w = 6)

## TRM ERROR ALEATORIO seeds 13 30 28
set.seed(28)
errAle <- vector()
for (i in seq(1,10, 2)) {errAle <- c(errAle, mean(rnorm(i))); print(rnorm(i))}
#plot(abs(errAle), main = ii)
errAle <- data.frame(x = as.factor(seq(1,10, 2)), y = abs(errAle))
p <- ggplot(data = errAle, aes(x = x, y = y)) + theme_bw() + geom_bar(stat = "identity", aes(fill = y)) +
  scale_fill_continuous(high = "#164952", low = "#34B1C9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), legend.position = "none") +
  scale_y_continuous(limits = c(0, 2), breaks = NULL) +
  labs(x = 'Réplicas promediadas', y = 'Error aleatorio en valor absoluto')
print(p)
savepdf(p, name = 'errorAleatorio', h = 4, w = 6)

