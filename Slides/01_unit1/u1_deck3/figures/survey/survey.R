# load libraries
library(openintro)  # for colors
library(ggplot2)

# load data
d <- read.csv("surveyS15_1_13.csv")

# view data
str(d)

# recode relationship status & class year ---------------------------
d$class_year = factor(d$class_year, levels = c("First-year", "Sophomore", "Junior", "Senior"))

d$relationship_status = factor(d$relationship_status, levels = c("yes", "no", "it's complicated"))

# seg bar plot relationship status & class year ---------------------

seg_bar_relstat_year <- ggplot(d, aes(x = class_year, fill = relationship_status)) + 
  geom_bar() +
  xlab("Class year") +
  ggtitle("Relationship status vs. class year")

ggsave("seg_bar_relstat_year.pdf", seg_bar_relstat_year, width = 6, height = 3)


# mosaic plot relationship status & class year ----------------------

pdf("mosaic_relstatus_class.pdf", width = 6, height = 3)
par(mar = c(0.2,0,2,0.5), mgp = c(2,1,0))
mosaicplot(table(d$class_year, d$relationship_status), las = 1, main = "Relationship status vs. class year", cex.axis = 1, col = c("#F57670", "#1BB940", "#649EFC"))
dev.off()

# drinks vs. vegetarian ---------------------------------------------

box_drinks_veg <- qplot(x = factor(vegetarian), y = nights_drinking, data = d, 
                        geom = "boxplot",
                        main = "Nights drinking/week vs. vegetarianism",
                        xlab = "vegetarian", ylab = "nights drinking")

ggsave("box_drinks_veg.pdf", box_drinks_veg, width = 5, height = 3)
