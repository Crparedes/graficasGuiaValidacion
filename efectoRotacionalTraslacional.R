source('functions.R')
Conc <- seq(0, 0.5, 0.1)
Sg_0 <- Conc * 0.8
Sg_r <- Conc * 1.2
Sg_t <- Conc * 0.8 + 0.2
df <- data.frame(Conc, Sg_0, Sg_r, Sg_t)

p_rot <- ggplot(data = df, aes(x = Conc, y = Sg_0)) + geom_point() + 
  geom_point(aes(y = Sg_r), color = '#34B1C9') +
  theme_bw() + 
  labs(y = 'Señal', x = 'Mensurando') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_y_continuous(limits = c(-1, 1)) + scale_x_continuous(limits = c(-1, 3)) +
  coord_cartesian(xlim = c(0, 0.55), ylim = c(0, 0.62), expand = 0) +
  geom_smooth(method = 'lm', formula = y ~ x, size = 0.4, se = FALSE, color = 'black') +
  geom_smooth(method = 'lm', formula = y ~ x, size = 0.4, se = FALSE, aes(y = Sg_r), color = '#34B1C9') +
  geom_curve(aes(x = Conc + 0.005, y = Sg_0 + 0.01, xend = Conc + 0.005, yend = Sg_r), size = 0.5, curvature = 0.5, 
             arrow = arrow(length = unit(seq(0.005, 0.04, length.out = 6), "npc")), color = '#34B1C9')
print(p_rot)

p_tra <- ggplot(data = df, aes(x = Conc, y = Sg_0)) + geom_point() + 
  geom_point(aes(y = Sg_t), color = '#34B1C9') +
  theme_bw() + 
  labs(y = 'Señal', x = 'Mensurando') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_y_continuous(limits = c(-1, 1)) + scale_x_continuous(limits = c(-1, 3)) +
  coord_cartesian(xlim = c(0, 0.55), ylim = c(0, 0.62), expand = 0) +
  geom_smooth(method = 'lm', formula = y ~ x, size = 0.4, se = FALSE, color = 'black') +
  geom_smooth(method = 'lm', formula = y ~ x, size = 0.4, se = FALSE, aes(y = Sg_t), color = '#34B1C9') +
  geom_segment(aes(x = Conc, y = Sg_0 + 0.01, xend = Conc, yend = Sg_t - 0.01), size = 0.5, 
               arrow = arrow(length = unit(0.03, "npc")), color = '#34B1C9')
print(p_tra)

savepdf(list(p_rot, p_tra), name = 'efectoInterf', h = 6, w = 6)
