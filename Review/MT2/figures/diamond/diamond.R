# load packages -----------------------------------------------------

library(ggplot2)
library(dplyr)

# load inference function -------------------------------------------

load(url("https://stat.duke.edu/~mc301/R/fun/inference.RData"))

# load data ---------------------------------------------------------

data(diamonds)

# subset data -------------------------------------------------------

set.seed(869)

diamond_samp <- diamonds %>%
  sample_n(150)

# make clarity two-level factor -------------------------------------

diamond_samp <- diamond_samp %>%
  mutate(cl = ifelse(clarity == "IF", "IF", "not IF"))

# bootstrap interval for clarity ------------------------------------

cl_boot <- inference(y = cl, data = diamond_samp, success = "IF", 
          statistic = "proportion", type = "ci", method = "simulation", 
          conf_level = 0.90, boot_method = "se",
          nsim = 100, seed = 1245)

# plot bootstrap interval for clarity -------------------------------

cl_boot_dot <- ggplot(data = data.frame(cl_boot), aes(x = sim_dist)) +
  geom_dotplot() +
  ylab("") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave(filename = "cl_boot_dot.pdf", plot = cl_boot_dot, height = 4, width = 6)

# hand calculate bootstrap se interval for clarity ------------------

m = round(mean(cl_boot$sim_dist), 3)
s = round(sd(cl_boot$sim_dist), 3)

# rounded
m + c(-1,1) * 1.65 * s

# exact
mean(cl_boot$sim_dist) + c(-1,1) * qnorm(0.95) * sd(cl_boot$sim_dist)

# relevel clarity ---------------------------------------------------

diamond_samp <- diamond_samp %>%
  mutate(clar = ifelse(clarity %in% c("FL","IF","VVS1","VVS2"), "VS-", "VS+"))

# relevel color -----------------------------------------------------

diamond_samp <- diamond_samp %>%
  mutate(col = ifelse(color %in% c("D","E","F"), "colorless", "near colorless"))

# ht for clar vs. col -----------------------------------------------

cl_col_rand <- inference(y = col, x = clar, data = diamond_samp, success = "colorless", 
               statistic = "proportion", type = "ht", method = "simulation", 
               null = 0, alternative = "twosided", boot_method = "se",
               seed = 28294)


# plot ht rand dist clar vs. col ------------------------------------

clar_col_ht_hist <- ggplot(data = data.frame(cl_col_rand), aes(x = sim_dist)) +
  geom_histogram() +
  xlab("simulated difference in means") +
  ggtitle("null distribution") +
  ylab("")

ggsave(filename = "clar_col_ht_hist.pdf", plot = clar_col_ht_hist, height = 3, width = 7)


