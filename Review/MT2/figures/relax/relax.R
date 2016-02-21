library(openintro)

setwd("/Users/mine/Desktop/Teaching/Sta 101 - S12/Exams/Midterm 2/figures/relax")

d = read.spss("/Users/mine/Desktop/Teaching/Sta 101 - S12/Lectures/data/gss2010.sav", to.data.frame = TRUE)

inference(d$hlthdays, type = "ci", method = "theoretical", est = "mean")