source('functions.R')


# Asimetría
(p6 <- ggplot(data.frame(x = seq(0, 1, 0.1)), aes(x = x)) + 
    geom_area(stat = "function", fun = dbeta, args = list(shape1 = 2, shape2 = 8), fill = "#97d7e4", alpha = 0.5, xlim = c(0, 1)) +
    geom_function(fun = dbeta, args = list(shape1 = 2, shape2 = 8))  +
    geom_area(stat = "function", fun = dbeta, args = list(shape1 = 8, shape2 = 2), fill = "#97d7e4", alpha = 0.5, xlim = c(0, 1)) +
    geom_function(fun = dbeta, args = list(shape1 = 8, shape2 = 2)) +
    #geom_text(aes(x = 0.3, y = 2.5))
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_x_continuous(name = NULL, breaks = NULL) + 
    scale_y_continuous(name = NULL, expand = c(0, 0), limits = c(0, 3.6), breaks = NULL) +
    annotate(geom = 'text', x = 0.85, y = 1.8, label = '(+)', size = 9) +
    annotate(geom = 'text', x = 0.15, y = 1.8, label = '(-)', size = 9))

  
  # curtosis
(p7 <-   ggplot(data.frame(x = seq(-5, 5, 1)), aes(x = x)) + 
    geom_area(stat = "function", fun = dlogis, args = list(scale = .5), fill = "#97d7e4", alpha = 0.3, xlim = c(-5, 5)) +
    geom_area(stat = "function", fun = dlogis, fill = "#97d7e4", alpha = 0.3, xlim = c(-5, 5)) +
    geom_area(stat = "function", fun = dlogis, args = list(scale = 2), fill = "#97d7e4", alpha = 0.3, xlim = c(-5, 5)) +
    geom_function(fun = dlogis, args = list(scale = .5)) +
    geom_function(fun = dlogis) + geom_function(fun = dlogis, args = list(scale = 2)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_x_continuous(name = NULL, breaks = NULL, expand = c(0, 0)) + 
    scale_y_continuous(name = NULL, expand = c(0, 0), limits = c(0, 0.6), breaks = NULL) +
    annotate(geom = 'text', x = 2.1, y = 0.4, label = 'Leptocúrtica', size = 8) +
    annotate(geom = 'text', x = 2.4, y = 0.17, label = 'Meso~', size = 8) +
    annotate(geom = 'text', x = 4, y = 0.1, label = 'Plato~', size = 8))
  

savepdf(list(p6, p7), name = 'miniPlots_descriptores', h = 3, w = 6)
