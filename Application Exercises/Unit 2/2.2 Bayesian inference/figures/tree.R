# load packages -----------------------------------------------------
library(openintro)

# test 1 ------------------------------------------------------------

pdf("test1.pdf", width = 7, height = 3)
treeDiag(main = c("Drug user?", "Test result"),
         p1 = c(0.05, 0.95), 
         p2 = list(c(0.97, 0.03), c(0.08, 0.92)),
         out2 = c("Positive", "Negative"),
         showSol = TRUE)
dev.off()

# test 2 ------------------------------------------------------------

pdf("test2.pdf", width = 7, height = 3)
treeDiag(main = c("Drug user?", "Test result"),
         p1 = c(0.39, 0.61), 
         p2 = list(c(0.97, 0.03), c(0.08, 0.92)),
         out2 = c("Positive", "Negative"),
         showSol = TRUE)
dev.off()
