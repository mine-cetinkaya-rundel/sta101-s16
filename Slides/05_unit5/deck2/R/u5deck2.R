# read and subset data for males
slasher <- read.csv("https://stat.duke.edu/~mc301/data/slasher.csv")
slasher_m <- slasher %>%
  filter(gender == "male")

# load inference function
load(url("https://stat.duke.edu/~mc301/R/fun/inference.RData"))

# run the hypothesis test
inference(y = outcome, x = sexual_activity, data = slasher_m,
          success = "survival", statistic = "proportion", 
          type = "ht", null = 0, alternative = "twosided", 
          method = "simulation", seed = 66613)