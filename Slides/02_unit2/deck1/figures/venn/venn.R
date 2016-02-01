# load packages -----------------------------------------------------
library(openintro)  # for colors
library(VennDiagram)# for dotplot

# non-disjoint ------------------------------------------------------
pdf("venn_non_disjoint.pdf", height = 3, width = 5)
draw.pairwise.venn(0.3, 0.4, 0.02, fill = c(COL[1], COL[2]), 
                   category = c("B", "A"), cex = 1.5, ext.length = 0.85, 
                   ext.dist = -0.2, margin = 0)
dev.off()


# disjoint ----------------------------------------------------------
pdf("venn_disjoint.pdf", height = 3, width = 5)
par(mar = c(0,0,0,0))
draw.pairwise.venn(0.3, 0.4, 0, fill = c(COL[1], COL[2]), 
                   category = c("B", "A"), cex = 1.5, margin = 0)
dev.off()
