library(openintro)

zinc = read.csv("zinc.csv")

pdf("zinc_diff_hist.pdf", height = 3, width = 4)
par(mar=c(4,2.5,1,1), cex.lab = 0.9, cex.axis = 0.9)
hist(zinc$difference, col = COL[1], main = "", xlab = "difference in zinc concentrations (bottom - surface)")
dev.off()




# horror
horror = movies[movies$genre == "Horror",]
hist(horror$audience_score)
median(horror$audience_score)

pdf("horror_dot.pdf", height = 2, width = 5)
par(mar=c(4,0.5,0,0.5))
BHH2::dotPlot(horror$audience_score, pch = 19, xlab = "audience scores")
dev.off()

write.csv(horror, file = "horror.csv", row.names = F)

horror$audience_score

set.seed(23)
cat(bs1 = sample(horror$audience_score, size = 20, replace = TRUE), sep = ", ")
cat(bs2 = sample(horror$audience_score, size = 20, replace = TRUE), sep = ", ")
cat(bs3 = sample(horror$audience_score, size = 20, replace = TRUE), sep = ", ")

set.seed(364)
horror_boot_med = rep(NA, 100)
for(i in 1:100){
  temp = sample(horror$audience_score, size = 20, replace = TRUE)
  horror_boot_med[i] = median(temp)
}

pdf("horror_boot_med_dot.pdf", height = 2, width = 5)
par(mar=c(4,0.5,0,0.5))
BHH2::dotPlot(horror_boot_med, pch = 19, xlab = "bootstrap medians", axes = FALSE)
axis(1, at = seq(29,59,1), label = c(NA, 30, rep(NA,4), 35, rep(NA,4), 40, rep(NA,4), 45, rep(NA,4), 50, rep(NA,4), 55, rep(NA,4)))
dev.off()

pdf("horror_boot_med_dot_soln.pdf", height = 2, width = 5)
par(mar=c(4,0.5,0,0.5))
BHH2::dotPlot(horror_boot_med, pch = 19, xlab = "bootstrap medians")
axis(1, at = seq(29,59,1), label = c(NA, 30, rep(NA,4), 35, rep(NA,4), 40, rep(NA,4), 45, rep(NA,4), 50, rep(NA,4), 55, rep(NA,4)))
abline(v = c(37.5, 52.0), col = COL[1])
dev.off()

mean(horror_boot_med)
sd(horror_boot_med)

# comedy
comedy = movies[movies$genre == "Comedy",]
hist(comedy$audience_score)
mean(comedy$audience_score)

pdf("comedy_hist.pdf", height = 2, width = 5)
par(mar=c(4,2,0,0.5))
hist(comedy$audience_score, col = COL[1], xlab = "audience scores", main = "")
dev.off()

set.seed(364)
comedy_boot_mean = rep(NA, 200)
for(i in 1:200){
  temp = sample(comedy$audience_score, size = 54, replace = TRUE)
  comedy_boot_mean[i] = mean(temp)
}

pdf("comedy_boot_mean_dot.pdf", height = 2, width = 5)
par(mar=c(4,0.5,0,0.5))
BHH2::dotPlot(comedy_boot_mean, pch = 19, xlab = "bootstrap means")
axis(1, at = seq(48,57,1))
dev.off()

mean(comedy_boot_mean)
sd(comedy_boot_mean)
