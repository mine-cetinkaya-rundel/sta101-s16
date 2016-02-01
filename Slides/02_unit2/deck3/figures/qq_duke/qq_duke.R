library(openintro)  # for colors
library(XML)        # for reading HTML table
library(stringr)    # for data cleanup

# URLs for 2000s and 1990s players
url_2000 <- "http://goduke.statsgeek.com/basketball-m/players/decade.php?decade=2000"
url_1990 <- "http://goduke.statsgeek.com/basketball-m/players/decade.php?decade=1990"

# read HTML tables for these years
d_2000 <- readHTMLTable(url_2000, which = 5, skip.rows = c(1:4), header = TRUE)
d_1990 <- readHTMLTable(url_1990, which = 5, skip.rows = c(1:4), header = TRUE)

# combine two tables
d <- rbind(d_1990, d_2000)

# replace capital accented A
d <- data.frame(lapply(d, function(x) str_replace(x, "\u00C2", "")))

# remove players with no number (and no height)
d <- d[d$No. != 0,]

# parse height data from "feet-inches" format to two separate columns
heights <- data.frame(do.call('rbind', strsplit(as.character(d$Ht),'-',fixed=TRUE)))
names(heights) = c("feet", "inches")
heights$feet = as.numeric(as.character(heights$feet))
heights$inches = as.numeric(as.character(heights$inches))

# calculate height in incheas
heights$all_inches <- (heights$feet)*12 + heights$inches

# assign heights back to original dataset
d$height = heights$all_inches

# calculate mean and SD
m = mean(d$height)
s = sd(d$height)

# histogram and QQ plot
pdf("qq_duke.pdf", width = 10, height = 4)
par(mfrow=c(1,2), mar = c(4.1,4.2,0.5,0.5), cex.axis = 1.5, cex.lab = 1.5)
# histogram
hist(d$height,freq=FALSE,main="",xlab="height (in.)",ylab="", col = COL[1], ylim = c(0,0.115), axes = FALSE)
axis(1)
x <- seq(min(d$height)-5, max(d$height)+5, 0.01)
y <- dnorm(x, m, s)
lines(x, y, col=COL[6], lwd=1.5)
# qqplot
qqnorm(d$height,axes=FALSE, col = COL[1,2], main = "", pch = 19)
axis(1)
axis(2)
qqline(d$height, col = COL[1])
dev.off()
