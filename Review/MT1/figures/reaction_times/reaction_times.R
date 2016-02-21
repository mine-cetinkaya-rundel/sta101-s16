setwd("~/Desktop/Teaching/Sta 104 - Su13/Exams/Midterm/figures/reaction_times")

rm(list=ls())

library(openintro)

d <- read.table("reaction_times.txt", header=T, quote="\"")
names(d) = "reaction_times"

pdf("reaction_times_boxplot.pdf", width = 6.5, height = 3)
par(mar = c(3,0.5,2,0.5))
boxplot(d$reaction_times, horizontal = TRUE, col = COL[1,3],
        main = "Dominant hand reaction distances")
dev.off()

round(mean(d$reaction_times), 2)
round(sd(d$reaction_times), 2)
round(length(d$reaction_times), 2)
round(median(d$reaction_times), 2)
round(IQR(d$reaction_times), 2)

round(summary(d$reaction_times), 2)

