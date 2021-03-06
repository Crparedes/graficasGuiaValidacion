library(ggplot2)
savepdf <- function(p, name, h = 7/1.5, w = 9/1.5, res = NULL) {
  pdf(paste0(name, '.pdf'), height = h, width = w)
  print(p)
  dev.off()
}

savepdfs <- function(p, name, h = 7/1.5, w = 9/1.5, res = NULL) {
  pdf(paste0(name, '.pdf'), height = h, width = w)
  for(i in 1:length(p)) print(p)
  dev.off()
}