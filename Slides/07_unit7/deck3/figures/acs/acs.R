# load packages -----------------------------------------------------

library(xtable)
library(ggplot2)
library(dplyr)
library(gridExtra)

# load data ---------------------------------------------------------

load(url("https://stat.duke.edu/~mc301/data/acs.RData"))

# subset data -------------------------------------------------------

acs_emp <- acs %>%
  filter(employment == "employed", income > 0)

acs_emp %>%
  select(employment) %>%
  table()

# drop levels -------------------------------------------------------

acs_emp <- droplevels(acs_emp)

acs_emp %>%
  select(employment) %>%
  table()

# non-log model -----------------------------------------------------

m_full = lm(income ~ hrs_work + race + age + gender + citizen, 
            data = acs_emp)

xtable(summary(m_full), digits = 2)

# diagnostic plots for non-log model --------------------------------

p_res_fit <- qplot(data = m_full, y = .resid, x = .fitted, geom = "point") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted values")

p_res_hist <- qplot(data = m_full, x = .resid, geom = "histogram") +
  xlab("Residuals") +
  ggtitle("Histogram of residuals")

p_res_qq <- qplot(data = m_full, sample = .resid, stat = "qq") +
  ggtitle("Normal probability plot of residuals")

p_res_order <- qplot(data = m_full, y = .resid) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Residuals") +
  xlab("Order of data collection") +
  ggtitle("Residuals vs. Order of data collection")

pdf("diag.pdf", width = 10, height = 7)
grid.arrange(p_res_fit, p_res_hist,
             p_res_qq, p_res_order, ncol = 2)
dev.off()

# log model ---------------------------------------------------------

m_full_log = lm(log(income) ~ hrs_work + race + age + gender + citizen, 
                data = acs_emp)

p_res_fit_log <- qplot(data = m_full_log, y = .resid, x = .fitted, geom = "point") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted values")

p_res_hist_log <- qplot(data = m_full_log, x = .resid, geom = "histogram") +
  xlab("Residuals") +
  ggtitle("Histogram of residuals")

p_res_qq_log <- qplot(data = m_full_log, sample = .resid, stat = "qq") +
  ggtitle("Normal probability plot of residuals")

p_res_order_log <- qplot(data = m_full_log, y = .resid) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Residuals") +
  xlab("Order of data collection") +
  ggtitle("Residuals vs. Order of data collection")

pdf("diag_log.pdf", width = 10, height = 7)
grid.arrange(p_res_fit_log, p_res_hist_log,
             p_res_qq_log, p_res_order_log, ncol = 2)
dev.off()