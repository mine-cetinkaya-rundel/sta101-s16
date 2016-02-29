library(openintro)
library(xtable)

load("evals.RData")

pdf("evals_rank.pdf", height = 1.5, width = 5)
par(las = 1, mar = c(2,6,0,0))
boxPlot(evals$score, fact = evals$rank, col = COL[1,2], horiz = TRUE)
dev.off()

by(evals$score, evals$rank, summary)
round(by(evals$score, evals$rank, sd),2)
by(evals$score, evals$rank, length)

source("http://stat.duke.edu/~mc301/R/inference.R")

inference(evals$score, evals$rank, type = "ht", method = "theoretical", est = "mean")

xtable(anova(lm(evals$score ~ evals$rank)))
