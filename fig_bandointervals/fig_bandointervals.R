require(burnr)


data(pgm)


pgm_comp <- composite(pgm, comp_name = "PGM")
pgm_interv <- intervals(pgm_comp)


svg("fig_bandointervals.svg", width = 5.5, height = 3)
par(mfrow = c(1, 2))
lab <- "Fire interval (yr)"
hist(pgm_interv$intervals, xlab = lab, main = "")
boxplot(pgm_interv$intervals, ylab = lab)
dev.off()
