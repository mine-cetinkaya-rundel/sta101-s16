# load libraries
library(openintro)

# load pop data
d = read.csv("duke_games.csv", h = T)

# pop hist
pdf("hist_duke_games_pop.pdf", width = 5, height = 3)
par(mar = c(3.2,2,1.5,0.5), mgp = c(2,1,0))
hist(d$games, col = COL[1], main = "", xlab = "number of games attended", ylab = "", cex.axis = 1.25)
dev.off()

# function for sampling
sampling <- function(pop, n, nsim = 10000, seed){
  samp_mean = rep(NA, nsim)
  set.seed(seed)
  for(i in 1:nsim){
    temp = sample(pop, n)
    samp_mean[i] = mean(temp)
  }
  return(samp_mean)
}

# sampling dists with various sample sizes
samp_mean_10 = sampling(d$games, 10, seed = 1234)
samp_mean_30 = sampling(d$games, 30, seed = 2345)
samp_mean_70 = sampling(d$games, 70, seed = 3456)

# plots of sampling dists
pdf("hist_duke_games_sampling10.pdf", width = 5, height = 3)
par(mar = c(3.2,2,1.5,0.5), mgp = c(2,1,0))
hist(samp_mean_10, col = COL[1], main = "", xlab = "sample means from samples of n = 10", ylab = "", cex.axis = 1.25)
dev.off()

pdf("hist_duke_games_sampling30.pdf", width = 5, height = 3)
par(mar = c(3.2,2,1.5,0.5), mgp = c(2,1,0))
hist(samp_mean_30, col = COL[1], main = "", xlab = "sample means from samples of n = 30", ylab = "", cex.axis = 1.25)
dev.off()

pdf("hist_duke_games_sampling70.pdf", width = 5, height = 3)
par(mar = c(3.2,2,1.5,0.5), mgp = c(2,1,0))
hist(samp_mean_70, col = COL[1], main = "", xlab = "sample means from samples of n = 70", ylab = "", cex.axis = 1.25)
dev.off()
