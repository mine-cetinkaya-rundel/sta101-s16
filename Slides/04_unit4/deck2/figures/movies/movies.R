library(openintro)
library(BHH2)

load("movies.Rdata")

# horror
horror = movies[movies$genre == "Horror",]
hist(horror$audience_score)
median(horror$audience_score)

pdf("horror_dot.pdf", height = 2, width = 5)
par(mar=c(4,0.5,0,0.5))
BHH2::dotPlot(horror$audience_score, pch = 19, xlab = "audience scores")
dev.off()

i = 1:20
cbind(i, horror[,c(1,2)])

pdf("horror_hist.pdf", height = 2, width = 5)
par(mar=c(4,2.5,0,0.5))
hist(horror$audience_score, col = COL[1], xlab = "audience scores", main = "")
dev.off()

write.csv(horror, file = "horror.csv", row.names = F)

horror$audience_score

set.seed(23)
bs1_sampled = sample(1:20, size = 20, replace = TRUE)
cbind(i, horror[bs1_sampled,c(1,2)])
cat(sort(horror$audience_score[bs1_sampled]), sep = ", ")
median(horror$audience_score[bs1_sampled])

set.seed(123)
bs2_sampled = sample(1:20, size = 20, replace = TRUE)
cbind(i, horror[bs2_sampled,c(1,2)])
cat(sort(horror$audience_score[bs2_sampled]), sep = ", ")
median(horror$audience_score[bs2_sampled])

set.seed(1234)
bs3_sampled = sample(1:20, size = 20, replace = TRUE)
cbind(i, horror[bs3_sampled,c(1,2)])
cat(sort(horror$audience_score[bs3_sampled]), sep = ", ")
median(horror$audience_score[bs3_sampled])



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

# test

pdf("horror_boot_med_test_dot.pdf", height = 2, width = 5)
par(mar=c(4,0.5,0,0.5))
horror_boot_med_test = horror_boot_med - (mean(horror_boot_med) - 40)
BHH2::dotPlot(horror_boot_med_test, pch = 19, xlab = "bootstrap medians", axes = FALSE)
axis(1, at = seq(24,55,1), label = c(NA, 25, rep(NA,4), 30, rep(NA,4), 35, rep(NA,4), 40, rep(NA,4), 45, rep(NA,4), 50, rep(NA,4), 55))
dev.off()

sum(horror_boot_med_test >= 43.5)

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
