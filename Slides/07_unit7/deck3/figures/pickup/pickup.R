# load packages -----------------------------------------------------

library(xtable)
library(ggplot2)
library(dplyr)
library(gridExtra)

# load data ---------------------------------------------------------

pu_allyrs <- read.csv("pickup.csv")

# all: scatterplot, no line -----------------------------------------

p_allyrs <- ggplot(data = pu_allyrs, aes(x = year, y = price)) +
  geom_point()

ggsave(p_allyrs, height = 4, width = 7, file = "pu_price_year_scat_allyrs.pdf")

# subset for 1992 onwards -------------------------------------------

pu <- pu_allyrs %>%
  filter(year >= 1992)

# 1992 and newer: scatterplot, no line ------------------------------

p_1992new <- ggplot(data = pu, aes(x = year, y = price)) +
  geom_point()

ggsave(p_1992new, height = 4, width = 7, file = "pu_price_year_scat_noline.pdf")

# no log: scatterplot, with line ------------------------------------

m1 <- lm(price ~ year, data = pu)

p_1992new_line <- ggplot(data = pu, aes(x = year, y = price)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  ggtitle("Price vs. Year")

ggsave(p_1992new_line, height = 4, width = 5, file = "pu_price_year_scat.pdf")

# no log: residuals -------------------------------------------------

p_m1_res_fit <- ggplot(data = m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted values")
  
ggsave(p_m1_res_fit, height = 4, width = 5, file = "pu_price_year_res.pdf")

# log: scatterplot, with line ---------------------------------------

m2 <- lm(log(price) ~ year, data = pu)

p_log_line <- ggplot(data = pu, aes(x = year, y = log(price))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  ggtitle("Log(price) vs. Year")

ggsave(p_log_line, height = 4, width = 5, file = "pu_price_year_scat_log.pdf")

# log: residuals ----------------------------------------------------

p_m2_res_fit <- ggplot(data = m2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted values")

ggsave(p_m2_res_fit, height = 4, width = 5, file = "pu_price_year_res_log.pdf")

# log model summary -------------------------------------------------

xtable(summary(m2), digits = 3)
