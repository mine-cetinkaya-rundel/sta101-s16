# load libraries
library(openintro)

# load pop data
pop <- read.csv("no_drinks_drunk.csv")

# pop hist
pdf("hist_no_drinks_drunk_pop.pdf", width = 5, height = 3)
par(mar = c(3.2,2,1.5,0.5), mgp = c(2,1,0))
hist(pop$no_drinks_drunk, col = COL[1], main = "", xlab = "number of drinks to get drunk", ylab = "", cex.axis = 1.25)
dev.off()

# code for creating histogram from submited results
all_content = readLines("/Applications/iclicker Mac v6.4.1/Classes/DASI-Sta101-F14/SessionData/[filename].csv")
skip_content = all_content[-(1:5)]
collected = read.csv(textConnection(skip_content), header = FALSE, stringsAsFactors = FALSE)
sampling = collected[,(ncol(collected)-2)] #-2 or -1?
hist(sampling)

mean(sampling, na.rm = TRUE)
sd(sampling, na.rm = TRUE)
