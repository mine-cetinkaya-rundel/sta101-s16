setwd("~/Desktop/Teaching/Sta 104 - Su13/Exams/Midterm/figures/boxplots")

rm(list=ls())

library(openintro)

sec1 = c(50,60,70,80,90); sd(sec1)
sec2 = c(47,64,70,76,82); sd(sec2)
d = c(sec1,sec2)
e = as.factor(c(rep("Section 1",length(sec1)), rep("Section 2",length(sec2))))
e = factor(e, levels = c("Section 2", "Section 1"))

png("boxplot_mt_score_pubpol_course_sections.png", width = 6.5, height = 3)
par(mar = c(3,5.5,2,0.5))
boxplot(d ~ e, horizontal = TRUE, axes = FALSE, ylim = c(45,90), 
        main = "Distributions of midterm scores for all students\nin two different sections of a public policy course",
        col = COL[1,3])
axis(1, at = seq(45,90,5), cex.axis = 1.25)
axis(2, at = c(1,2), labels = levels(e), cex.axis = 1.25, las = 1)
dev.off()

