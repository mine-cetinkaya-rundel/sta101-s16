setwd("~/Desktop/Teaching/Sta 101 - F12/Lecture slides/(9) Final review/figures/anova")

source("http://stat.duke.edu/courses/Fall12/sta101.001/labs/inference.R")

teller = read.table("~/Desktop/Teaching/Sta 101 - F12/Lecture slides/(9) Final review/figures/anova/Ch28_Tellers.txt", h = T)

inference(teller$Time, as.factor(teller$Teller), est="mean", type = "ht", method="theoretical",alternative="greater")

dl = read.table("~/Desktop/Teaching/Sta 101 - F12/Lecture slides/(9) Final review/figures/anova/Ch28_Downloading.txt", h = T, sep = "\t")

inference(dl$TimeSec, as.factor(dl$TimeOfDay), est="mean", type = "ht", method="theoretical",alternative="greater")
