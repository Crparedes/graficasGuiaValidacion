source('functions.R')

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

