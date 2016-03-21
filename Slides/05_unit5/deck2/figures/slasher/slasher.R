# load packages -----------------------------------------------------

library(dplyr)

# create data from Welsh paper --------------------------------------

slasher <- data.frame(gender = c(rep("female", 83+139), rep("male", 74+189)),
                      sexual_activity = c(rep("present", 11+72), rep("absent", 39+100),
                                          rep("present", 7+67), rep("absent", 28+161)),
                      outcome = c(rep("survival", 11), rep("death", 72),
                                  rep("survival", 39), rep("death", 100),
                                  rep("survival", 7), rep("death", 67),
                                  rep("survival", 28), rep("death", 161)))

# check data --------------------------------------------------------

slasher_f <- filter(slasher, gender == "female")
slasher_m <- filter(slasher, gender == "male")

table(slasher_m$sexual_activity, slasher_m$outcome)
table(slasher_f$sexual_activity, slasher_f$outcome)

# write data --------------------------------------------------------

write.csv(slasher, file = "slasher.csv", row.names = FALSE)

# load data ---------------------------------------------------------

slasher <- read.csv("https://stat.duke.edu/~mc301/data/slasher.csv")
slasher_m <- slasher %>%
  filter(gender == "male")
slasher_f <- slasher %>%
  filter(gender == "female")

load(url("https://stat.duke.edu/~mc301/R/fun/inference.RData"))

inference(y = outcome, x = sexual_activity, data = slasher_m,
          success = "survival", statistic = "proportion", 
          type = "ht", null = 0, alternative = "twosided", 
          method = "simulation", seed = 66613)


