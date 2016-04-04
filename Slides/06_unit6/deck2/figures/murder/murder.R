murder = read.csv("murder.csv")

library(openintro)

plot(murder, pch = 19, col = COL[1,2])

# plot: annual_murders_per_mil vs. perc_pov
pdf("annual_murders_per_mil_perc_pov.pdf", width = 5, height = 4)
par(mar = c(4,4,1,1))
plot(murder$annual_murders_per_mil ~ murder$perc_pov, pch = 19, col = COL[1], xlab = "% in poverty", ylab = "annual murders per million")
dev.off()

# cor: annual_murders_per_mil vs. perc_pov
cor(murder$annual_murders_per_mil, murder$perc_pov)

# plot: annual_murders_per_mil_population

pdf("annual_murders_per_mil_population.pdf", width = 5, height = 4)
par(mar = c(4,4,1,1))
plot(murder$annual_murders_per_mil ~ murder$population, pch = 19, col = COL[1], xlab = "population", ylab = "annual murders per million")
dev.off()

# cor: annual_murders_per_mil vs. population
cor(murder$annual_murders_per_mil, murder$population)

# residuals: annual_murders_per_mil vs. perc_pov
m1 = lm(annual_murders_per_mil ~ perc_pov, data = murder)
summary(m1)

pdf("annual_murders_per_mil_perc_pov_res.pdf", width = 7, height = 4)
par(mar = c(4,4,1,1))
plot(murder$annual_murders_per_mil ~ murder$perc_pov, pch = 19, col = COL[1], xlab = "% in poverty", ylab = "annual murders per million")
for(i in 1:nrow(murder)){
  lines(x = c(murder$perc_pov[i], murder$perc_pov[i]), y = c(murder$annual_murders_per_mil[i], m1$fitted[i]), col = COL[2], lwd = 2)
}
abline(m1, col = COL[4], lwd = 2)
dev.off()

# summary stats

round(mean(murder$annual_murders_per_mil),2)
round(sd(murder$annual_murders_per_mil),2)

round(mean(murder$perc_pov),2)
round(sd(murder$perc_pov),2)

round(cor(murder$annual_murders_per_mil, murder$perc_pov), 2)

# prediction + extrapolation

pdf("annual_murders_per_mil_perc_pov_wide.pdf", width = 10, height = 3)
par(mar = c(4,4,1,1))
plot(murder$annual_murders_per_mil ~ murder$perc_pov, pch = 19, col = COL[1,3], xlim = c(0, 60), ylim = c(-40,100), xlab = "% in poverty", ylab = "annual murders per million")
abline(lm(murder$annual_murders_per_mil ~ murder$perc_pov), col = COL[1], lwd = 2)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
# pred
x1 = data.frame(perc_pov = 20)
p1 = predict(m1, newdata = x1)
lines(x = c(20,20), y = c(0, p1), col = COL[2], lwd = 2)
lines(x = c(20,-5), y = c(p1,p1), col = COL[2], lwd = 2)
# extra
x2 = data.frame(perc_pov = 50)
p2 = predict(m1, newdata = x2)
lines(x = c(50,50), y = c(0, p2), col = COL[4], lwd = 2)
lines(x = c(50,-5), y = c(p2,p2), col = COL[4], lwd = 2)
# intercept
points(x = 0, y = m1$coefficients[1], col = COL[4], cex = 1.5)
dev.off()