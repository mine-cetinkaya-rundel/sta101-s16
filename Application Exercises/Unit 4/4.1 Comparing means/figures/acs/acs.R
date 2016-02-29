library(openintro)
library(xtable)

load("acs.RData")

emp = acs[acs$employment == "employed",]
hist(emp$income)

pdf("sal_gen_box.pdf", height = 3, width = 8)
par(mar=c(3,3,1,0))
boxPlot(emp$income, fact = emp$gender, col = COL[1,3], horiz = TRUE)
dev.off()

by(emp$income, emp$gender, summary)
by(emp$income, emp$gender, sd)
by(emp$income, emp$gender, length)