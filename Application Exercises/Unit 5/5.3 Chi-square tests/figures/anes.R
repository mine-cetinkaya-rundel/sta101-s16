library(openintro)
library(xtable)
library(dplyr)
library(ggplot2)

# load data ---------------------------------------------------------

# load(url("http://bit.ly/dasi_anes_data"))
load("anes.RData")

# rename levels of sample_region variable ---------------------------

levels(anes$sample_region) <- c("Northeast", "North Central", "South", "West")

# subset for data with complete cases for the two variables considered -----

anes <- anes %>%
  filter(!is.na(sample_region) , !is.na(presapp_track))

# take a random sample ----------------------------------------------

set.seed(12345)
anes_samp <- anes[sample(1:nrow(anes), size = 500, replace = FALSE),]
tab <- table(anes_samp$sample_region, anes_samp$presapp_track)
xtable(addmargins(tab), digits = 0)

# segmented bar plot -------------------------------------------------

anes_mosaic <- ggplot(data = anes_samp, aes(x = sample_region, fill = presapp_track)) +
  geom_bar(position = "fill") +
  ylab("") +
  xlab("region") +
  theme_bw() +
  scale_fill_grey(start = 0.4, end = 0.8) +
  guides(fill = guide_legend(title = "track"))

ggsave(filename = "anes_mosaic.pdf", plot = anes_mosaic, height = 3, width = 6.5)

# GOF chisq test ----------------------------------------------------

census_dist = c(0.18, 0.22, 0.37, 0.23)

anes_gof <- chisq.test(table(anes_samp$sample_region), p = census_dist)

# indep chisq test --------------------------------------------------

chisq.test(table(anes_samp$presapp_track, anes_samp$sample_region))

# using inference function

load(url("https://stat.duke.edu/~mc301/R/fun/inference.RData"))

inference(x = sample_region, y = presapp_track, data = anes_samp,
          statistic = "proportion", alternative = "greater",
          type = "ht", method = "theoretical")
