setwd("~/Desktop/Teaching/Sta101_F14/Slides/(4) MT1 - Review/figures/ht_curves")
library(openintro)

mu = 100
xbar = 95
s = 30
n = 100
se = s / sqrt(n)


pdf("a.pdf", height = 3, width = 5)
par(mar=c(2,0,0,0))
normTail(mu, s, L = 95, U = 105, col = COL[1], cex.axis = 1.5)
dev.off()

pdf("b.pdf", height = 3, width = 5)
par(mar=c(2,0,0,0))
normTail(mu, se, L = 95, col = COL[1], cex.axis = 1.5)
dev.off()

pdf("c.pdf", height = 3, width = 5)
par(mar=c(2,0,0,0))
normTail(mu, se, L = 95, U = 105, col = COL[1], cex.axis = 1.5)
dev.off()

pdf("d.pdf", height = 3, width = 5)
par(mar=c(2,0,0,0))
normTail(xbar, se, L = 90, U = 100, col = COL[1], cex.axis = 1.5)
dev.off()




