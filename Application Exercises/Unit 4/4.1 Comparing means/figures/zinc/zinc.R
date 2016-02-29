library(openintro)

zinc = read.csv("zinc.csv")

pdf("zinc_bottom_hist.pdf", height = 3, width = 4)
par(mar=c(4,2.5,2,1), cex.lab = 1.25, cex.axis = 1.25)
hist(zinc$bottom, col = COL[1], xlab = "", main = "zinc concentration, bottom")
dev.off()

pdf("zinc_surface_hist.pdf", height = 3, width = 4)
par(mar=c(4,2.5,2,1), cex.lab = 1.25, cex.axis = 1.25)
hist(zinc$surface, col = COL[1], xlab = "", main = "zinc concentration, surface")
dev.off()

pdf("zinc_diff_hist.pdf", height = 3, width = 4)
par(mar=c(4,2.5,2,1), cex.lab = 1.25, cex.axis = 1.25)
hist(zinc$difference, col = COL[1], xlab = "", main = "difference in zinc concentrations\n(bottom - surface)")
dev.off()

mean(zinc$bottom)
mean(zinc$surface)
mean(zinc$difference)

round(sd(zinc$bottom), 4)
round(sd(zinc$surface), 4)
round(sd(zinc$difference), 4)
