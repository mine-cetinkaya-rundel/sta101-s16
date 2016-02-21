library(openintro)

setwd("/Users/mine/Desktop/Teaching/Sta 101 - S12/Exams/Midterm 2/figures/shooting")

ideology = c(rep("conservative", 238), rep("moderate", 345), rep("liberal", 155))
race = c(rep("not a factor", 64), rep("small factor", 54), rep("major factor", 80), rep("not sure", 40),
	rep("not a factor", 43), rep("small factor", 79), rep("major factor", 179), rep("not sure", 44),
	rep("not a factor", 17), rep("small factor", 13), rep("major factor", 99), rep("not sure", 26)
)
ideology = factor(ideology, levels = c("conservative", "moderate", "liberal"))
race = factor(race, levels = c("not a factor", "small factor", "major factor", "not sure"))

chisq.test(ideology, race, correct = FALSE)