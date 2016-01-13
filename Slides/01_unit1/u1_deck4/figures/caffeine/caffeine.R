setwd("~/Desktop/Teaching/Sta 104 - Su13/Slides/(1) Unit 1/Lec 3/figures/caffeine")

library(Lock5Data)
library(xtable)
data(CaffeineTaps)

xtable(CaffeineTaps)

pdf("caffeinetaps_box.pdf", width = 8, height = 5)
boxplot(CaffeineTaps$Taps ~ CaffeineTaps$Group)
dev.off()

by(CaffeineTaps$Taps, CaffeineTaps$Group, summary)
by(CaffeineTaps$Taps, CaffeineTaps$Group, sd)
by(CaffeineTaps$Taps, CaffeineTaps$Group, IQR)


source("http://stat.duke.edu/courses/Spring13/sta101.001/labs/inference.R")

pdf("caffeinetaps_mean_ht.pdf", width = 10, height = 4)
set.seed(125)
inference(CaffeineTaps$Taps, CaffeineTaps$Group, est = "mean", type = "ht", alternative = "greater", null = 0, method = "simulation", nsim = 100, drawlines = FALSE)
dev.off()

pdf("caffeinetaps_median_ht.pdf", width = 10, height = 4)
set.seed(135)
inference(CaffeineTaps$Taps, CaffeineTaps$Group, est = "median", type = "ht", alternative = "greater", null = 0, method = "simulation", nsim = 100, drawlines = FALSE)
dev.off()
