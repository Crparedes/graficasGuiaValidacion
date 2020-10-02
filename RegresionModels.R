source('functions.R')
#Serie de datos
set.seed(1)
df <- data.frame(x = 0:4, y = (0:4)*2.5 + rnorm(5, 0, 1.6), ybar1 = 1)
df$y[1] <- 0
plot(y ~ x, data = df)
abline(lm(y~x, data = df))

# OLS

(p1 <- ggplot(df, aes(x = x, y = y)) + geom_point() +
    #geom_area(stat = "function", fun = dnorm, args = list(mean = 0.5, sd = 0.17), fill = "#97d7e4", alpha = 0.3, xlim = c(0, 1)) + 
    geom_smooth(method = "lm", formula = 'y ~ x', se = FALSE, color = 'black', size = 0.5) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") 
    #scale_x_continuous(name = NULL, expand = c(0, 0), breaks = NULL) + 
    #scale_y_continuous(name = NULL, expand = c(0, 0), limits = c(-1, 12), breaks = NULL) +
    #annotate(geom = "segment", x = 0.5, xend = 0.5, y = 0, yend = 2.35) +
    #annotate(geom = "text", x = 0.52, y = 2.48, size = 9, label = bquote(bar(x)))
    )

#Mediana
(p2 <- ggplot(data.frame(x = seq(0, 1, 1)), aes(x = x)) + 
    geom_area(stat = "function", fun = dbeta, args = list(shape1 = 2, shape2 = 8), fill = "#97d7e4", alpha = 0.3, xlim = c(0, 1)) + 
    geom_function(fun = dbeta, args = list(shape1 = 2, shape2 = 8)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_x_continuous(name = NULL, expand = c(0, 0), breaks = NULL) + 
    scale_y_continuous(name = NULL, expand = c(0, 0), limits = c(0, 4), breaks = NULL) +
    annotate(geom = "segment", x = (2-1/3)/(2+8-2/3), xend = (2-1/3)/(2+8-2/3), y = 0, yend = 3.255) +
    annotate(geom = "text", x = 0.23, y = 3.36, size = 9, label = "Me"))

#Varianza
(p3 <- ggplot(data.frame(x = seq(0, 1, 1)), aes(x = x)) + 
    geom_area(stat = "function", fun = dnorm, args = list(mean = 0.5, sd = 0.17), fill = "#97d7e4", alpha = 0.3, xlim = c(0, 1)) + 
    geom_function(fun = dnorm, args = list(mean = 0.5, sd = .17)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_x_continuous(name = NULL, expand = c(0, 0), breaks = NULL) + 
    scale_y_continuous(name = NULL, expand = c(0, 0), limits = c(0, 2.8), breaks = NULL) +
    annotate(geom = "segment", x = 0.2, xend = 0.8, y = 2.4, yend = 2.4, arrow = arrow(ends = "both")) +
    annotate(geom = "segment", x = c(0.2, 0.8), xend = c(0.2, 0.8), y = c(0.486, 0.486), yend = c(2.5, 2.5)) +
    annotate(geom = "text", x = 0.513, y = 2.6, size = 9, label = expression(S^2)))

#Desv.Estandar
(p4 <- ggplot(data.frame(x = seq(0, 1, 1)), aes(x = x)) + 
    geom_area(stat = "function", fun = dnorm, args = list(mean = 0.5, sd = 0.17), fill = "#97d7e4", alpha = 0.2, xlim = c(0, 1)) + 
    geom_area(stat = "function", fun = dnorm, args = list(mean = 0.5, sd = 0.17), fill = "#97d7e4", alpha = 0.7, xlim = c(0.16, 0.84)) +
    geom_area(stat = "function", fun = dnorm, args = list(mean = 0.5, sd = 0.17), fill = "#97d7e4", xlim = c(0.33, 0.67)) +
    geom_function(fun = dnorm, args = list(mean = 0.5, sd = .17)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_x_continuous(name = NULL, expand = c(0, 0), breaks = NULL) + 
    scale_y_continuous(name = NULL, expand = c(0, 0), limits = c(0, 3.4), breaks = NULL) +
    annotate(geom = "segment", x = c(0.33, 0.16), xend = c(0.67, 0.84), y = c(2.4, 2.95), yend = c(2.4, 2.95), 
             arrow = arrow(ends = "both")) +
    annotate(geom = "segment", x = 0.5, xend = 0.5, y = 0, yend = 2.35, linetype = "dotted") +
    annotate(geom = "segment", x = c(0.33, 0.67), xend = c(0.33, 0.67), y = 0, yend = 2.5, linetype = "dashed") +
    annotate(geom = "segment", x = c(0.16, 0.84), xend = c(0.16, 0.84), y = 0, yend = 3.05, linetype = "dashed") +
    annotate(geom = "text", x = 0.5, y = c(2.7, 3.15), size = 8, 
             label = c(expression(paste(bar(x), ' ± s = 66%')), expression(paste(bar(x), ' ± 2s = 95%')))))

#desviación estándar relativa
(p5 <- ggplot(data.frame(x = seq(0, 1, 1)), aes(x = x)) + 
    geom_area(stat = "function", fun = dnorm, args = list(mean = 0.4, sd = 0.044), fill = "#97d7e4", alpha = 0.3, xlim = c(0, 1)) + 
    geom_area(stat = "function", fun = dnorm, args = list(mean = 0.8, sd = 0.088), fill = "#97d7e4", alpha = 0.3, xlim = c(0, 1)) + 
    geom_function(fun = dnorm, args = list(mean = 0.4, sd = .044)) +
    geom_function(fun = dnorm, args = list(mean = 0.8, sd = .088)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(color = "black"),
                       axis.text.y = element_text(color = "black"), legend.position = "none") +
    scale_x_continuous(name = NULL, expand = c(0, 0), limits = c(0.2, 1), breaks = NULL) + 
    scale_y_continuous(name = NULL, expand = c(0, 0), limits = c(0, 10), breaks = NULL) +
#    annotate(geom = "segment", x = 0.2, xend = 0.8, y = 2.4, yend = 2.4, arrow = arrow(ends = "both")) +
#    annotate(geom = "segment", x = c(0.2, 0.8), xend = c(0.2, 0.8), y = c(0.486, 0.486), yend = c(2.5, 2.5)) +
    annotate(geom = "text", x = 0.7, y = 6, size = 9, label = expression(paste('RS', D[1], ' = RS', D[2], ' = 10%'))))

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
  

#savepdf(list(p1, p2, p3, p4, p5, p6, p7), name = 'miniPlots_descriptores', h = 3, w = 6)
