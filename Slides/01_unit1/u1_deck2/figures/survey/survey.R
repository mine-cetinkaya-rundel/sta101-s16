# load libraries ----------------------------------------------------
library(openintro)  # for colors
library(ggplot2)    

# load data ---------------------------------------------------------
d <- read.csv("surveyS15_1_13.csv")

# view data ---------------------------------------------------------
str(d)

# histogram of age at first kiss ------------------------------------
# very low numbers -> bad question? romantic kiss or any kiss?

first_kiss_hist <- qplot(first_kiss, data = d,
      binwidth = 2,
      main = "How old were you when you had your first kiss?",
      xlab = "age at first kiss", ylab = "")

ggsave("hist_first_kiss.pdf", first_kiss_hist, width = 5, height = 3)


# dotplot of fb visits ----------------------------------------------
# at higher numbers people are rounding

fb_visits_dot <- ggplot(d, aes(x = fb_visits_per_day)) + 
  geom_dotplot(binwidth = 5) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("FB visits / day") +
  ylab("") +
  ggtitle("How many times do you go on Facebook per day?")
  
ggsave("dot_fb_visits_per_day.pdf", fb_visits_dot, width = 5, height = 3)

