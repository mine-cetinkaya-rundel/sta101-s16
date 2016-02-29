# load packages -----------------------------------------------------

library(openintro)

# problem 1: find cutoff --------------------------------------------

s = 2.2
n = 150
se = sqrt((s^2 / n) + (s^2 / n))
mu = 0
delta = 0.5

t_star = qt(0.95, df = n-1)
cutoff = t_star * se

pdf("power_t_star_clicker.pdf", height = 3, width = 5)
par(mar = c(2,0.5,0,0.5))
normTail(df = n-1, U = t_star, col = COL[1], cex.axis = 1.25, axes = FALSE)
axis(1, at = -3:3, labels = NA)
axis(1, at = t_star, labels = "t* = ?")
text(x = 2.2, y = 0.1, "0.05", col = COL[4], cex = 1.5)
dev.off()

pdf("power_t_star_obs.pdf", height = 3, width = 5)
par(mar = c(3,0.5,0,0.5), mgp = c(2, 1.8, 0))
normTail(m = mu, s = se, df = n-1, U = cutoff, col = COL[1], cex.axis = 1.25, axes = FALSE)
axis(1, at = c(mu-3*se, mu, cutoff, mu+3*se), 
     labels = c(NA, "0\n", "t = 1.66\nx_new - x_old = 0.42", NA))
text(x = 0.6, y = 0.3, "0.05", col = COL[4], cex = 1.5)
dev.off()

# problem 2: plot power ---------------------------------------------

pdf("power.pdf", height = 3, width = 10)
par(mar = c(2,0.5,0,0.5))

normTail(m = mu, s = se, df = n-1, U = cutoff, col = COL[1], cex.axis = 1.25, axes = FALSE,
         xlim = c(mu-3.2*se, (mu+0.5)+3.2*se))
axis(1, at = c(-4, mu, cutoff, 4), 
     labels = c(NA, "0", "0.42", NA))

normTail(m = mu+0.5, s = se, df = n-1, U = cutoff, col = COL[2,3], add = TRUE, axes = FALSE)
axis(1, at = c(0, mu+0.5, 4), labels = c(NA, mu+0.5, NA))

dev.off()

# calculate power ---------------------------------------------------

t_cutoff = (cutoff - (mu+delta)) / se

pt(t_cutoff, df = n - 1, lower.tail = FALSE)

#pnorm(cutoff, mean = mu, sd = se, lower.tail = FALSE)

# calculate n -------------------------------------------------------

s = 2.2
mu = 0
delta = 0.5

ns = 10:1000
power = rep(NA, length(ns))

for(i in 10:1000){
  n = i
  t_star = qt(0.95, df = n-1)
  se = sqrt((s^2 / n) + (s^2 / n))
  cutoff = t_star * se
  t_cutoff = (cutoff - (mu+delta)) / se
  power[i-9] = pt(t_cutoff, df = n-1, lower.tail = FALSE)
}

which_n = which.min(abs(power - 0.9))
power[which_n]
power[which_n + 1]
ns[which_n + 1]

pdf("power_curve.pdf", height = 3, width = 7)
par(mar = c(3,3,0.5,0))
plot(power ~ ns, type = "l", xlab = "n", lwd = 3, col = "gray")
abline(h = 0.9, col = "red")
abline(v = ns[325], col = "blue")
dev.off()


