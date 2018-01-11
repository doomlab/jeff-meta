##original graph with corrected numbers.

met_dat = read.csv("revised meta.csv")
met_data = revised_meta
met_data2 = revised_meta2

View(met_dat)

study = cbind(
  c("Study",  "PTG", "   Gebler & Maercker 1", "   Gebler & Maercker 2", "   Kallay & Baban",
    "   Slavin-Spenny et al.", "   Smith et al.", "   Ulrich et al.", "   Lancaster et al.",
    "   Lu et al. 1", "   Lu et al. 2", "QOL", "   Possemato et al.", "   Craft et al. 1",
    "   Craft et al. 2", "   Lu et al. 1", "   Lu et al. 2", "   Gellaitry et al. 1", "   Gellaitry et al. 2", 
    "   Gellaitry et al. 3", "Average", "PTG Average", "QOL Average"),
  
  c("Lower", NA, "-0.64", "-0.56", "-0.11", "-0.13", "0.08", "-0.03", "-0.36",
    "-0.25","-0.43", NA,  "1.37", "0.26", "-0.61", "-0.27", "-0.43",
    "-0.12", "-0.40","-0.19", "-0.16", "-0.27", "-0.05"),
  
  c("d",  NA, "0.01", "0.09", "0.18", "0.21",  "0.17", "0.26", "-0.07", 
    "0.21", "-0.02", NA, "2.06", "0.70", "-0.23", "0.19", "-0.02", 
    "0.21", "-0.08", "0.13", "0.21", "0.12", "0.37"),
    
  c("Upper",  NA, "0.67", "0.75", "0.47", "0.53", "0.27", "0.55", "0.23", "0.66",
    "0.47", NA, "2.74", "1.12", "0.16", "0.64", "0.47", "0.53", "0.24",
    "0.45", "0.63", "0.51", "0.79"))

plot1 = forestplot(study,
                   met_data2$effect.size,
                   met_data2$lower.limit,
                   met_data2$upper.limit,
                   zero = 0,
                   cex  = 2,
                   lineheight = "auto",
                   xlab = "Effect Size",
                   is.summary = c(T, T, rep(FALSE,9),TRUE, F, F, rep(FALSE,8),  F),
                   hrzl_lines = list("2" = gpar(lty = 1)),
                   vertices = T,
                   graph.pos = 2)
plot1


