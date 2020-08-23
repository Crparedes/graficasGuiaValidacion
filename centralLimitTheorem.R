source('functions.R')

## descriptores estadísticos
Uniform <- seq(9, 21, length.out = 600)
samples <- vector()
set.seed(0)
for (i in 1:100) samples <- c(samples, mean(sample(x = Uniform, size = 5, replace = TRUE)))

(p1 <- ggplot(data = data.frame(x = Uniform)) + geom_histogram(aes(x = x), binwidth = 0.8, colour = "black", fill = "#34b1c9") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "black"),
                     axis.text.y = element_text(color = "black"), legend.position = "none") +
  scale_x_continuous(limits = c(10, 20.4), breaks = seq(10, 21, 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)))

(p2 <- ggplot(data = data.frame(x = samples)) + geom_histogram(aes(x = x), binwidth = 0.8, colour = "black", fill = "#34b1c9") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text.x = element_text(color = "black"),
                     axis.text.y = element_text(color = "black"), legend.position = "none") +
  scale_x_continuous(limits = c(10, 20.4), breaks = seq(10, 21, 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks = NULL))

savepdf(list(p1, p2), name = 'limiteCentral', h = 4, w = 3)

