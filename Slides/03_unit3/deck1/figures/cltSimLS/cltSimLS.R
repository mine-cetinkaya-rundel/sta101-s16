# load libraries
library(openintro)

# set seed
set.seed(85479)

# genera distribution
a  = rbeta(10000, 3.5, 2)
b = a * 94

# population
pdf("cltSimLS_pop.pdf", height = 3.5, width = 5)
par(mar=c(4,0,0,0), mgp=c(2.7,0.7,0), las = 1)
densityPlot(b, bw = 5, from = 0, to = 101, col = COL[5], fadingBorder = "66", histo = "faded", xlab = "", axes = FALSE, ylab = "")
axis(1, cex.axis = 1.5)
dev.off()

# sample
set.seed(2452)
samp = sample(b, size = 500)

pdf("cltSimLS_samp.pdf", height = 3, width = 5)
par(mar=c(2,1,1,0), mgp=c(2.7,0.7,0), las = 1)
hist(samp, col = COL[1], main = "Plot B", ylab = "", xlab = "", axes = FALSE, cex.main = 1.5)
axis(1, cex.axis = 1.5)
dev.off()

# sampling, n = 18
set.seed(24524)
sampling_18 = rep(0, 500)
n = 18

for(i in 1:500){
  	temp <- sample(b, n)
   	sampling_18[i] <- mean(temp)
   	}

pdf("cltSimLS_n18.pdf", height = 3, width = 5)
par(mar=c(2,1,1,0), mgp=c(2.7,0.7,0), las = 1)
hist(sampling_18, col = COL[1], main = "Plot C", ylab = "", xlab = "", axes = FALSE, cex.main = 1.5)
axis(1, cex.axis = 1.5)
dev.off()

# sampling, n = 81
set.seed(3563)
sampling_81 = rep(0, 500)
n = 81

for(i in 1:500){
  	temp <- sample(b, n)
   	sampling_81[i] <- mean(temp)
   	}

pdf("cltSimLS_n81.pdf", height = 3, width = 5)
par(mar=c(2,1,1,0), mgp=c(2.7,0.7,0), las = 1)
hist(sampling_81, col = COL[1], main = "Plot A", ylab = "", xlab = "", axes = FALSE, cex.main = 1.5)
axis(1, cex.axis = 1.5)
dev.off()
