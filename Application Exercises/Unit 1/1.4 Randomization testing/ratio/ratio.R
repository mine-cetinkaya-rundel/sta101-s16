# load packages -----------------------------------------------------
library(openintro)
library(dplyr)
library(xtable)

# load data ---------------------------------------------------------
d = read.csv("teach_stu_rat.csv")

# format data -------------------------------------------------------
# round student-faculty ratio
# relabel public / private

d$rat = round(d$studentfacultyratio, 2)
d$type[d$Private == 1] = "private"
d$type[d$Private == 0] = "public"
d$type = factor(d$type, levels = c("private", "public"))

# summary stats -----------------------------------------------------
sum_tab <- d %>%
  group_by(type) %>%
  summarize(mean = round(mean(rat), 2), SD = round(sd(rat), 2), n = n())

print(xtable(sum_tab), include.rownames = FALSE)

# side-by-side box plots of ratio vs. type ---------------------------
ratio_box <- ggplot(data = d, aes(x = factor(type), y = rat)) +
  geom_boxplot() +
  xlab("Type of school") +
  ylab("Student to faculty ratio")

ggsave(ratio_box, file = "ratio_box.pdf", width = 6, height = 3)

# load inference function -------------------------------------------

download.file("https://stat.duke.edu/~mc301/R/inference.RData", destfile = "inference.RData")
load("inference.RData")

# randomization -----------------------------------------------------

rand_dist = inference(d$rat, d$type, est = "mean", type = "ht", method = "simulation", null = 0, alternative = "greater", order = c("public","private"), nsim = 100, simdist = TRUE, seed = 123456)
rand_dist[which.max(rand_dist)] = 4.1

rand_df <- data.frame(rand_dist = rand_dist)

rand_dist_dot <- ggplot(data = rand_df, aes(x = rand_dist)) +
  geom_dotplot(binwidth = 0.2) +
  xlab("Distribution of simulated differences (public - private)") +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(rand_dist_dot, file = "rand_dist_dot.pdf", width = 5, height = 2)
