setwd("~/Desktop/Teaching/Sta101_F14/Slides/(4) MT1 - Review/figures/num_data")
library(openintro)

set.seed(123)
d = rbeta(950, 4, 1.5)*100
d = c(d, rep(10, 50))

pdf("rel_hist.pdf", width = 7, height = 4)
par(mar = c(2,2.2,0.5,1), cex.axis = 1.5)
hist(d, col = COL[1], main = "", axes = FALSE)
axis(1)
axis(2, at = seq(0, 200, 50), labels = seq(0, 0.20, 0.05))
abline(h = seq(25, 200, 25), col = "gray", lty = 3)
dev.off()


pdf("boxplot.pdf", width = 8, height = 4)
par(mar = c(2.2,0.5,0.5,0.5), cex.axis = 1.5)
boxplot(d, col = COL[1], ylim = c(-2,102), horizontal = TRUE)
abline(v = seq(0,100,10), col = "gray", lty = 3)
dev.off()

