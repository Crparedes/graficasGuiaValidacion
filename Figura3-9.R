source('functions.R')
## 
m <- 10
R.cor <- seq(0.999, 0.5, -0.01)
t.cal <- (R.cor * sqrt(m - 2)) / (sqrt(1 - R.cor ^ 2))

plot(R.cor, t.cal)
library(scales)

(RechT <- ggplot(data = data.frame(R = R.cor, Tcal = t.cal), aes(x = R, y = t.cal)) +
  theme_bw() + 
  labs(y = expression(t['cal']), x = 'Coeficiente de correlación R') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  geom_function(fun = function (x) return((x * sqrt(m - 2)) / (sqrt(1.000 - x ^ 2))), 
                color = '#34B1C9', size = 1) +
  scale_y_continuous(limits = c(0, 14), n.breaks = 11, expand = c(0, 0), oob=rescale_none) +
  scale_x_reverse(limits = c(1, 0.3), labels = function(x) sprintf("%.2f", x), 
                  breaks = seq(1, 0.3, - 0.05), expand = c(0, 0)) +
  geom_hline(aes(yintercept = qt(0.9999, 9, FALSE)), size = 0.35, linetype = "dashed", color = '#164952') + 
  geom_hline(aes(yintercept = qt(0.999, 9, FALSE)), size = 0.35, linetype = "dashed", color = '#164952') + 
  geom_hline(aes(yintercept = qt(0.99, 9, FALSE)), size = 0.35, linetype = "dashed", color = '#164952') + 
  geom_hline(aes(yintercept = qt(0.95, 9, FALSE)), size = 0.35, linetype = "dashed", color = '#164952') +
  annotate("text", x = 0.65, y = 13.5, 
           label = expression(paste(H[0], ": No existe correlación entre X y Y")), size = 3.5) + 
  annotate("text", x = 0.605, y = qt(0.9999, 9, FALSE) + 0.3, label = "Niveles de confianza 99.99 %", size = 3) + 
  annotate("text", x = 0.526, y = qt(0.999, 9, FALSE) + 0.3, label = "99.9 %", size = 3) + 
  annotate("text", x = 0.518, y = qt(0.99, 9, FALSE) + 0.3, label = "99 %", size = 3) + 
  annotate("text", x = 0.518, y = qt(0.95, 9, FALSE) + 0.3, label = "95 %", size = 3)
  # geom_segment(aes(x = 1.627, y = -1, xend = 1.627, yend = 0.285), size = 0.3, linetype = "dotted") +
  # geom_segment(aes(x = 1.48, y = -1, xend = 1.48, yend = 0.285), size = 0.3, linetype = "dotted")  +
  # geom_segment(aes(x = 1.55, y = -1, xend = 1.55, yend = 0.285), size = 0.3, linetype = "dashed") +
  # geom_segment(aes(x = 1.48, y = 0, xend = 1.627, yend = 0), size = 0.3,
  #              arrow = arrow(ends = 'both', length = unit(x = 7, units = 'pt')))
)
savepdf(RechT, name = 'VariacionValorTcontraR', h = 4, w = 6)

# asd -------------------------------------------------------------------------------------------------------------

