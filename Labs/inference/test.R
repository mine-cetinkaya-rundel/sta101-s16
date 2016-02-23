load(url("https://stat.duke.edu/~mc301/data/nc.RData"))
load(url("https://stat.duke.edu/~mc301/R/fun/inference.RData"))

# ci_single_mean_theo
inference(y = weight, data = nc, statistic = "mean", method = "theoretical", type = "ci")

# ci_single_mean_sim
inference(y = weight, data = nc, statistic = "mean", method = "simulation", type = "ci", 
          boot_methoprop,d = "se")

inference(y = weight, data = nc, statistic = "mean", method = "simulation", type = "ci", 
          boot_method = "perc")

# ci_single_median_sim
inference(y = weight, data = nc, statistic = "median", method = "simulation", type = "ci", 
          boot_method = "se")

inference(y = weight, data = nc, statistic = "median", method = "simulation", type = "ci", 
          boot_method = "perc")

# ci_single_prop_sim
inference(y = mature, data = nc, success = "mature mom", statistic = "proportion", 
          method = "simulation", type = "ci", boot_method = "perc")

# ci_single_prop_theo
inference(y = mature, data = nc, success = "mature mom", statistic = "proportion", 
          method = "simulation", type = "ci", boot_method = "perc")

# ci_two_mean_sim
inference(y = weight, x = habit, data = nc, statistic = "mean", method = "simulation", 
          type = "ci", boot_method = "se")

inference(y = weight, x = habit, data = nc, statistic = "mean", method = "simulation", 
          type = "ci", boot_method = "perc")

# ci_two_mean_theo
inference(y = weight, x = habit, data = nc, statistic = "mean", method = "theoretical", 
          type = "ci")

# ci_two_med_sim
inference(y = weight, x = habit, data = nc, statistic = "median", method = "simulation", 
          type = "ci", boot_method = "se")

inference(y = weight, x = habit, data = nc, statistic = "median", method = "simulation", 
          type = "ci", boot_method = "perc")

# ci_two_prop_sim
inference(y = mature, x = habit, data = nc, statistic = "proportion", method = "simulation", 
          type = "ci", boot_method = "se", success = "mature mom")

inference(y = mature, x = habit, data = nc, statistic = "proportion", method = "simulation", 
          type = "ci", boot_method = "perc", success = "mature mom")

# ci_two_prop_theo
inference(y = mature, x = habit, data = nc, statistic = "proportion", method = "theoretical", 
          type = "ci", boot_method = "se", success = "mature mom")

# ht_single_mean_sim
inference(y = weeks, data = nc, statistic = "mean", method = "simulation", 
          type = "ht", null = 38, alternative = "twosided")

# ht_single_mean_theo
inference(y = weeks, data = nc, statistic = "mean", method = "theoretical", 
          type = "ht", null = 38, alternative = "twosided")

# ht_single_med_sim
inference(y = weeks, data = nc, statistic = "median", method = "simulation", 
          type = "ht", null = 40, alternative = "twosided")

# ht_single_prop_sim
inference(y = habit, data = nc, success = "smoker", statistic = "proportion", 
          method = "simulation", type = "ht", null = 0.1, alternative = "twosided")

# ht_single_prop_theo
inference(y = habit, data = nc, success = "smoker", statistic = "proportion", 
          method = "theoretical", type = "ht", null = 0.1, alternative = "twosided")

# ht_two_mean_sim
inference(y = weight, x = habit, data = nc, statistic = "mean", 
          method = "simulation", type = "ht", null = 0, alternative = "twosided")

# ht_two_mean_theo
inference(y = weight, x = habit, data = nc, statistic = "mean", 
          method = "theoretical", type = "ht", null = 0, alternative = "twosided")

# ht_two_median_sim
inference(y = weight, x = habit, data = nc, statistic = "median", 
          method = "simulation", type = "ht", null = 0, alternative = "twosided",
          order = c("smoker", "nonsmoker"))

# ht_two_prop_sim
inference(y = habit, x = mature, data = nc, success = "smoker", statistic = "proportion", 
          method = "simulation", type = "ht", null = 0, alternative = "twosided", nsim = 200)

# ht_two_prop_theo
inference(y = habit, x = mature, data = nc, success = "smoker", statistic = "proportion", 
          method = "theoretical", type = "ht", null = 0, alternative = "twosided", nsim = 200)


# ht_many_mean_theo
set.seed(5274)
nc$fake_group = factor(sample(c("gr1", "gr2", "gr3"), size = nrow(nc), replace = TRUE))
res <- inference(y = weight, x = fake_group, data = nc, statistic = "mean", 
          method = "theoretical", type = "ht", alternative = "greater", sig_level = 0.4,
          show_summ_stats = FALSE, show_var_types = FALSE)

# ht_many_prop_theo
set.seed(1752)
nc$fake_group2 = factor(sample(c("a1", "b2", "c3"), size = nrow(nc), replace = TRUE))
inference(y = fake_group2, x = fake_group, data = nc, statistic = "proportion", 
                 method = "theoretical", type = "ht", alternative = "greater")

# ht_many_prop_sim
inference(y = fake_group2, x = fake_group, data = nc, statistic = "proportion", 
          method = "simulation", type = "ht", alternative = "greater")
