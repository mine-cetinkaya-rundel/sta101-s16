setwd("~/Desktop/Teaching/Sta 101 - F13/Exams/Midterm/figures/blood_pressure")

rm(list=ls())

library(openintro)

calcium = c(7,-4,18,17,-3,-5,1,10,11,-2)
placebo = c(-1,12,-1,-3,3,-5,5,2,-11,-1,-3)

sort(calcium)
sort(placebo)

bp = c(calcium,placebo)
gr = c(rep("calcium",length(calcium)), rep("placebo",length(placebo)))

source("http://stat.duke.edu/courses/Spring13/sta101.001/labs/inference.R")

simdist = inference(bp, gr, est = "median", type = "ht", alternative = "twosided", 
          method = "simulation", nsim = 100, seed = 173, drawlines = FALSE, null = 0,simdist = TRUE)

pdf("blood_pressue_calcium_placebo_randomization.pdf", width = 7, height = 4)

par(mar=c(4,0.5,4,0.5))
radius  <- 0.0249
cex     <- 1
seed    <- 1
stacks  <- dotPlotStack(simdist, radius=radius, addDots=FALSE, pch=19, col=COL[1], cex=1.25, seed=seed)
plot(0, type="n", xlab="median_calcium - median_placebo", axes=FALSE, 
     ylab="", xlim=c(-11, 11), ylim=c(0, quantile(stacks[[3]], 0.994)),
     main = "Randomization distribution - difference in median change\nbetween blood pressure in calcium and placebo groups")
dotPlotStack(simdist, radius=radius, pch=19, col=COL[1], cex=cex, seed=seed)
abline(h=0)
abline(v = c(-10,-5,0,5,10), lty = 3)
axis(1)

dev.off()




dotPlotStack <- function(x, radius=1, seed=1, addDots=TRUE, ...){
  set.seed(seed)
  x <- sample(x)
  y <- rep(NA, length(x))
  y[1] <- 1
  for(i in 2:length(x)){
    add <- TRUE
    for(s in seq(radius, i*radius, radius/20)){
      these <- 1:(i-1)
      dx    <- (x[i] - x[these])^2
      dy    <- (s - y[these])^2
      if(any(dx+dy < radius^2) || !add){
        next
      } else {
        if(addDots){
          points(x[i], s, ...)
        }
        y[i] <- s
        add  <- FALSE
      }
    }
  }
  invisible(list(max(y)+radius, x, y))
}
