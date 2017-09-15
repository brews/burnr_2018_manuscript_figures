require(burnr)


FHX_PATH <- "../data/bando.fhx"


EXCLUDE_EVENTS <- c("null", "recorder", "pith", "bark", 
                    "inner", "outer", "estimate")
EVENT_LEVELS <- c("dormant", "early", "middle", "late", "latewd", "unknown", 
                  "null", "recorder", "pith", "bark", "inner", "outer", 
                  "estimate")


d_fhx <- read_fhx(FHX_PATH)

# Create table with count of events
d_events <- data.frame("year" = d_fhx$year, 
                       "event" = as.character(d_fhx$rec_type),
                       stringsAsFactors = FALSE)
# Selecting a small range of years
d_events <- subset(d_events, year %in% 1716:1748)
d_events$event <- sapply(d_events$event, 
                             function(x) strsplit(x, '_')[[1]][1])
d_events$event <- ordered(d_events$event, levels = EVENT_LEVELS)


x <- table(d_events, exclude = EXCLUDE_EVENTS, dnn = c("year", "event"))
x <- x[rowSums(x) > 1, ]  # Only have years with more than 4 events.
years <- as.numeric(unique(rownames(x)))


# Labels and colors for plot
season_labels <- c("Dormant", "Early earlwood", "Middle earlywood",
                   "Late earlywood", "Latewood", "Unknown")
seasonal_colors <- c("#d62728", "#ff7f0e", "#bcbd22", "#2ca02c", "#1f77b4", "#7f7f7f")


# Plot into PDF
svg('fig4.svg', width = 6, height = 4)

m <- matrix(c(1:10, rep(11, 5)),
            nrow = 3, ncol = 5, byrow = TRUE)

layout(mat = m, heights = c(0.4,0.4,0.2))

for (yr in years) {
    par(mar = c(1,1,1,1))
    x_sub <- x[rownames(x) == yr, ]
    n = sum(x_sub)
    title_str <- paste0(yr, " (n=", n, ")")
    wgt <- 1 - 2.71828^(-0.25 * sum(x_sub))
    pie(x_sub, main = title_str, col = seasonal_colors, labels = "",
        init.angle = 90, radius = wgt, border = 1)
}
frame()
legend(x = "top", inset = 0, title="Fire event position", legend = season_labels, 
       col = seasonal_colors, lwd = 5, cex = 1, horiz = FALSE, ncol = 3, bty = 'n')

dev.off()
par(op)
