source('functions.R')

## descriptores estadísticos
Uniform <- seq(9, 21, length.out = 600)
samples <- vector()
set.seed(0)
for (i in 1:200) samples <- c(samples, mean(sample(x = Uniform, size = 5, replace = TRUE)))

(p1 <- ggplot(data = data.frame(x = Uniform)) + geom_histogram(aes(x = x), binwidth = 0.8, colour = "black", fill = "#97d7e4") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "black"),
                     axis.text.y = element_text(color = "black"), legend.position = "none") +
  scale_x_continuous(limits = c(9, 20.8), breaks = seq(9, 21, 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 54), breaks = NULL) + 
  labs(x = 'Valor', y = 'Frecuencia'))

(p2 <- ggplot(data = data.frame(x = samples)) + geom_histogram(aes(x = x, y = ..density..), binwidth = 0.8, colour = "black", fill = "#97d7e4") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "black"),
                     axis.text.y = element_text(color = "black"), legend.position = "none") +
  scale_x_continuous(limits = c(9, 20.8), breaks = seq(9, 21, 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.35), breaks = NULL) + 
  labs(x = 'Valor', y = '') + 
  geom_density(aes(x = x), fill = "#97d7e4", alpha = 0.4))

savepdf(list(p1, p2), name = 'limiteCentral', h = 4, w = 4)

