library(openintro)

pdf("nonlinear.pdf", width = 5, height = 6)
set.seed(1113)
x = (-50:49)/2
e = rnorm(100, 0, 100)
y = 3*x^2 + 2*x + e
lmPlot(x, y, col = COL[1,2])
dev.off()

pdf("heteroscedastic.pdf", width = 5, height = 6)
set.seed(1113)
x = (-50:49)/2
e = c(rnorm(30, 0, 1000), rnorm(30, 0, 500), rnorm(20, 0, 200), rnorm(20, 0, 200))
y = 20 + 2*x + e
lmPlot(x, y, col = COL[1,2])
dev.off()