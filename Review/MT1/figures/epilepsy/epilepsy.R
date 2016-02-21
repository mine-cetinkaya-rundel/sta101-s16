library(openintro)

z = qnorm(.97)

pdf("epilepsy.pdf", height = 3, width = 5)
par(mar=c(2,0,0,0), mgp=c(2.7,0.7,0), cex.axis = 1.25)
normTail(L = -1*z, U = z, col = COL[1], axes = FALSE)
axis(1, at = c(-3, paste(round(-1*z, 2)), paste(round(z, 2)), 3))
text(x = 0, y = 0.15, "0.94", cex = 1.5, col = COL[4])
text(x = -2.3, y = 0.025, "0.03", cex = 1.5, col = COL[4])
text(x = 2.3, y = 0.025, "0.03", cex = 1.5, col = COL[4])
dev.off()
