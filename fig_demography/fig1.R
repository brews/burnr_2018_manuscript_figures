require(burnr)

data(lgr2)
data(lgr2_meta)


svg("fig1a.svg", width = 6.5, height = 3)
plot(lgr2)
dev.off()


svg("fig1b.svg", width = 6.5, height = 2.75)
plot(lgr2, ylabels = FALSE,
     color_group = lgr2_meta$SpeciesID,
     color_id = lgr2_meta$TreeID, 
     composite_rug = TRUE, injury_event = TRUE)
dev.off()


library(ggplot2)

svg("fig1c.svg", width = 6.5, height = 2.75)
p <- plot_demograph(sort(lgr2, decreasing=TRUE),
             color_group = lgr2_meta$SpeciesID, 
             color_id = lgr2_meta$TreeID, 
             composite_rug = TRUE, injury_event = TRUE,
             ylabels = FALSE, plot_legend = TRUE)
p <- (p + annotate('rect', alpha = 0.2, 
                   xmin = 1861, xmax = 1880,
                   ymin = 2.5, ymax = 15.5)
        # + guides(col = guide_legend(ncol = 2))
        + theme(legend.position = c(0.25, 0.4), 
                legend.direction = 'horizontal'))
print(p)
dev.off()
