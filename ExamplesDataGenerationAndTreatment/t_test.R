set.seed(27)
(HgTot <- round(rnorm(8, 470, 30), 0)) #g kg^-1
mu <- 500
t.test(x = HgTot, mu = mu, alternative = 'less')

library(ggplot2)

(p5 <- ggplot(data.frame(x = c(0.9, 1.1) * range(c(HgTot, mu))), aes(x = x)) + 
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_x_continuous(name = 'Series 1', expand = c(0, 0)) + 
    scale_y_continuous(name = NULL, expand = expansion(mult = c(0, 0.1), add = c(0, 0)), breaks = NULL) +
    geom_area(stat = "function", fun = dnorm, args = list(mean = mean(HgTot), sd = sd(HgTot)), 
              fill = "#97d7e4", alpha = 0.6))


      