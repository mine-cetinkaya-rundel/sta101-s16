
## Set n to something some positive number then run the code below.
n = 5

make.uniform <- function(n) {
    u = seq(-1*n, n, by=1)
    u = c(u,u)
    u
}

make.other <- function(n, i) {
    # i on outside.
    inside  = 2 * (2*n+1) - 2*i
    x = c(rep(-n, i), rep(n,i), rep(0, inside))
    x
}

the.bound <- function(n) {
    (n+1) * (2*n+1) / (3 * n)
}

var.uniform <- function(n) {
    (n * (n+1) / 3) * ( (2*(2*n+1)) / (2*(2*n+1)-1) )
}

var.other <- function(n, i) {
    ( i * n^2 / (2*n+1) ) * ( (2*(2*n+1)) / (2*(2*n+1)-1) )
}

u = make.uniform(n)
mean.u = mean(u)
sd.u   = sd(u)
Q1.u   = quantile(u, 0.25)
Q3.u   = quantile(u, 0.75)

mean.x = rep(0, 2*n+2)
sd.x   = rep(0, 2*n+2)
Q1.x   = rep(0, 2*n+2)
Q3.x   = rep(0, 2*n+2)

for(i in 0:(2*n+1)) {
    x = make.other(n,i)
    mean.x[i+1] = mean(x)
    sd.x  [i+1] = sd(x)
    Q1.x  [i+1] = quantile(x, 0.25)
    Q3.x  [i+1] = quantile(x, 0.75)
    
}

par(mfrow=c(1,3))
breaks = seq(-1*n-0.5, n+0.5, by=1)
for(i in 0:(2*n+1) ) {    
    x = make.other(n,i)
    hist(u, breaks=breaks, ylim=c(0,2*(2*n+1)), main="Histogram of uniform")
    for (j in 1:15) { abline(h=j, col="#222222AA", lty=2) }
    axis(side=1, at=(-1*n):n)
    hist(x, breaks=breaks, ylim=c(0,2*(2*n+1)), main="Histogram of other")
    for (j in 1:15) { abline(h=j, col="#222222AA", lty=2) }
    axis(side=1, at=(-1*n):n)
    plot(0:(2*n+1) , sd.x, type="l", main="sd uniform and sd other", col=2, ylab="Standard Deviation", xlab="Height of outside blocks")
    abline(h=sd.u, col=1)
    points(c(i,i), c(sd.u, sd.x[i+1]), col=c(1,2), pch=19)
    legend("topleft", legend=c("Uniform", "Other"), lty=1, col=c(1,2))
    cat("height of outside blocks:", i, "\n")
    # cat("mean u:", mean.u   , "sd(u):", sd.u   , "Q1(u):", Q1.u, "Q3(u)", Q3.u, "\n")
    # cat("mean x:", mean.x[i+1], "sd(x):", sd.x[i+1], "Q1(x):", Q1.x[i+1], "Q3(x):", Q3.x[i+1], "\n")
    cat("mean u:", mean.u     , "sd(u):", sd.u     , "\n")
    cat("mean x:", mean.x[i+1], "sd(x):", sd.x[i+1], "\n")
    readline("PRESS ENTER:")
}
