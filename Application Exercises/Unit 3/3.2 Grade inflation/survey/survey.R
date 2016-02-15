setwd("~/Desktop/Teaching/Sta101_F14/Application exercise/3.2 Hypothesis tests - single mean/survey")
library(openintro)

survey <- read.csv("~/Desktop/Teaching/Sta101_F14/survey/survey_sep1.csv")

names(survey)

d = survey[survey$gpa != 0,]
d = d[!is.na(d$gpa),]

nrow(d)
mean(d$gpa)
sd(d$gpa)

pdf("hist_gpa.pdf", height = 3, width = 5)
par(mar=c(4.2,2,2,0))
hist(d$gpa, col = COL[1], main = "", xlab = "gpa")
dev.off()
