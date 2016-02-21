# load packages -----------------------------------------------------

library(ggplot2)
library(dplyr)

# load inference function -------------------------------------------

load(url("https://stat.duke.edu/~mc301/R/fun/inference.RData"))

# load data ---------------------------------------------------------

gift = read.csv("gifted.csv")

# plot mother and father iq -----------------------------------------

fatheriq_hist <- ggplot(data = gift, aes(x = fatheriq)) +
  geom_histogram(fill = "#8FDEE1", binwidth = 5) +
  xlab("Father's IQ") +
  xlim(c(100, 135))

ggsave(filename = "fatheriq_hist.pdf", plot = fatheriq_hist, height = 3, width = 5)

summary(gift$fatheriq)

motheriq_hist <- ggplot(data = gift, aes(x = motheriq)) +
  geom_histogram(fill = "#8FDEE1", binwidth = 5) +
  xlab("Mother's IQ") +
  xlim(c(100, 135))

ggsave(filename = "motheriq_hist.pdf", plot = motheriq_hist, height = 3, width = 5)

summary(gift$motheriq)

# calculate iqdiff --------------------------------------------------

gift <- gift %>%
  mutate(iqdiff = fatheriq - motheriq)

# calculate iqdiff --------------------------------------------------

iqdiff_boot <- inference(y = iqdiff, data = gift, type = "ci", method = "simulation", 
          statistic = "mean", boot_method = "se", seed = 2653)

# plot iqdiff_boot --------------------------------------------------

iqdiff_boot_hist <- ggplot(data = data.frame(iqdiff_boot), aes(x = sim_dist)) +
  geom_histogram() +
  ylab("") +
  xlab("bootstrap means") +
  ggtitle("Bootstrap Distribution")

ggsave(filename = "iqdiff_boot_hist.pdf", plot = iqdiff_boot_hist, height = 2.5, width = 7)

# bootstrap quantiles -----------------------------------------------

lower <- c(0.01, 0.025, 0.05, 0.10, 0.20)
upper <- rev(1 - lower)

round(quantile(iqdiff_boot$sim_dist, probs = c(lower, upper)), 2)

# ht for difference between motheriq and fatheriq -------------------

inference(y = iqdiff, data = gift, type = "ht", method = "theoretical", 
          statistic = "mean", null = 0, alternative = "twosided")

# histogram of counting age ----------------------------------------- 

count_hist <- ggplot(data = gift, aes(x = count)) +
  geom_histogram(fill = "#8FDEE1", binwidth = 2) +
  xlab("age when the child first counted to 10 successfully (months)")

ggsave(filename = "count_hist.pdf", plot = count_hist, height = 3, width = 5)