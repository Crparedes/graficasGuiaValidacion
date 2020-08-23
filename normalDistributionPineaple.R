source('functions.R')

set.seed(0) # DOI: 10.21577/0103-5053.20200015
pineapleAs <- rnorm(10000, 794.1, 794.1 * 0.0231)
(p1 <- ggplot(data = data.frame(x = pineapleAs), aes(x = x)) + 
    geom_histogram(aes(y = ..density..), binwidth = 6, colour = "black", fill = "white") + 
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_color_manual(values = c(rep('white', 15), 'black', rep('white', 6))) + 

        stat_function(fun = dnorm, args = list(mean = 794.1, sd = 794.1 * 0.0231)) +
    geom_area(stat = "function", fun = dnorm, args = list(mean = 794.1, sd = 794.1 * 0.0231), fill = "#97d7e4", alpha = 0.6) +
    scale_x_continuous(breaks = seq(730, 850, 20)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 0.023), breaks = NULL) + 
    labs(x = expression(paste('Concentración de arsénico en piña (', mu, 'g k', g^{-1}, ')')), y = 'Densidad de distribución')) #+
#    geom_segment(aes(x = 0))

savepdf(p1, name = 'ejem_distrib', h = 4, w = 6)

