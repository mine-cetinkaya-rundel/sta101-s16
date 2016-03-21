load(url("https://stat.duke.edu/~mc301/R/fun/inference.RData"))

n_veg = 8
n_nonveg = 75

sta101 = data.frame(veg = c(rep("yes", n_veg), rep("no", n_nonveg)))

inference(y = veg, data = sta101, success = "yes", 
          statistic = "proportion", type = "ht", 
          null = 0.08, alternative = "twosided", 
          method = "simulation", seed = 10292015)

inference(y = veg, data = sta101, success = "yes", 
          statistic = "proportion", type = "ci", 
          method = "simulation", boot_method = "se", seed = 10302015)
