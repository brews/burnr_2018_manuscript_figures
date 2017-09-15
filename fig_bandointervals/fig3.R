require(burnr)

data(pgm)

pgm_comp <- composite(pgm, comp_name = "PGM")
pgm_interv <- intervals(pgm_comp)

svg("fig3.svg", width = 6.5, height = 4, )
par(mfrow = c(1, 2))
lab <- "Fire interval (yr)"
hist(pgm_interv$intervals, xlab = lab, main = "")
boxplot(pgm_interv$intervals, ylab = lab)
dev.off()
