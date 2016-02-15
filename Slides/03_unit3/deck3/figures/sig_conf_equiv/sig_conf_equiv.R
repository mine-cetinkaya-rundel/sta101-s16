library(openintro)

pdf("CL95_twosided.pdf", height = 3.5, width = 5)
par(mar=c(2,0,0,0), mgp=c(2.7,0.7,0), cex.axis = 1.25)
normTail(L = -1.96, U = 1.96, col = COL[1], axes = FALSE)
axis(1, at = c(-3, -1.96, 1.96, 3), label = c(NA,  -1.96, 1.96, NA))
text(x = 0, y = 0.15, "0.95", cex = 1.5, col = COL[4])
text(x = -2.5, y = 0.05, "0.025", cex = 1.5, col = COL[4])
text(x = 2.5, y = 0.05, "0.025", cex = 1.5, col = COL[4])
dev.off()

pdf("CL95_onesided.pdf", height = 3.5, width = 5)
par(mar=c(2,0,0,0), mgp=c(2.7,0.7,0), cex.axis = 1.25)
normTail(L = -1.96, U = 1.96, axes = FALSE)
polygon(x = c(1.96, seq(1.96, 3.5, 0.01)),
        y = c(0, dnorm(seq(1.96, 3.5, 0.01))),
        col = COL[1])
axis(1, at = c(-3, -1.96, 1.96, 3), label = c(NA,  -1.96, 1.96, NA))
text(x = 0, y = 0.15, "0.95", cex = 1.5, col = COL[4])
text(x = -2.5, y = 0.05, "0.025", cex = 1.5, col = COL[6])
text(x = 2.5, y = 0.05, "0.025", cex = 1.5, col = COL[4])
dev.off()
