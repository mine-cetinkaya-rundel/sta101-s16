# load libraries ----------------------------------------------------
library(ggplot2)
library(openintro)

# openintro colors --------------------------------------------------
data(COL)

# load data ---------------------------------------------------------
d <- read.csv("surveyS15_univ_app.csv")

# histogram of university applications ------------------------------

hist_uni_app <- ggplot(d, aes(x = university_applications)) +
  geom_histogram(binwidth = 2, fill = COL[1,2], col = COL[1]) +
  scale_y_continuous(breaks = seq(0, 30, 2)) +
  scale_x_continuous(breaks = seq(0, 24, 4), limits = c(0, 24)) +
  xlab("Number of university applications") +
  ylab("Frequency")
                     
ggsave(hist_uni_app, file = "hist_university_applications.pdf", 
       width = 7, height = 4)

# summary stats -----------------------------------------------------
# note: no NAs
summary(d$university_applications)
length(d$university_applications)

