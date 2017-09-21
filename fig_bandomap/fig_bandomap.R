# 2017-08-30
# Figure 2 example

require(burnr)
require(rgdal)
require(ggmap)
require(broom)  # Needed to plot polygons in ggmap.
require(rgeos)  # Needed to create polygon centroids for labels.
require(ggrepel)  # Package for centroid labels.

# Fix Stamen terrain tiles bug in ggmap. May not need in future.
source("stamenmap_fix.R")


TREEMETA_PATH <- "../data/treemetadata.csv"
BURNBLOCK_PATH <- "../data/burnblock_polygons.geojson"
FHX_PATH <- "../data/bando.fhx"


# Read in burn block polygons from GeoJSON file. If you're using shapefiles, 
# these are just as easily import with the same function.
burnblocks <- readOGR(BURNBLOCK_PATH)
burnblocks <- spTransform(burnblocks, CRS("+init=epsg:4326 +proj=longlat"))

# Burn block centroids used to create quick labels in map plot
burnblocks_centroids <- data.frame(rgeos::gCentroid(burnblocks, byid = TRUE, 
                                               id = burnblocks$blockid))

# Read in tree-sample metadata
treemeta_raw <- read.csv(TREEMETA_PATH)
# Convert UTM to latlon so can compoare with burnblock polygons
tree_proj <- SpatialPointsDataFrame(cbind(treemeta_raw$Easting, treemeta_raw$Northing), 
                              data = treemeta_raw,
                              proj4string = CRS("+proj=utm +zone=13S"))
treemeta <- spTransform(tree_proj, CRS("+init=epsg:4326 +proj=longlat")) 


# Read fhx data.
all_fhx <- read_fhx(FHX_PATH)


# Further subset treemeta and all_fhx to series with many scars.
bando_stats <- series_stats(all_fhx, func_list = list(nscar = count_scar))
many_scars <- subset(bando_stats, nscar >= 6)
fhx_manyscars <- get_series(all_fhx, as.character(many_scars$series))
treemeta_manyscars <- subset(treemeta, SampleID %in% many_scars$series)


# Create a left, bottom, right, top bounding box - delimits map edges.
location <- list()
location$left <- -106.47
location$right <- -106.25
location$top <- 35.87
location$bottom <- 35.69


# Put it all together into a map.
map <- get_map(location = unlist(location), source = "stamen", 
               maptype = "terrain-background")
# Output as SVG in current working directly.
svg("fig_bandomap_a.svg", width = 5.5, height = 3.5)
p <- ggmap(map)
p <- (p + geom_polygon(data = fortify(burnblocks), 
                       aes(long, lat, group = group),
                       fill = "orange", color = "#d95f02", alpha = 0.2)
        + geom_point(aes(x = coords.x1, y = coords.x2), 
                     data = data.frame(treemeta_manyscars), 
                     alpha = 0.75, color = "#7570b3")
        + geom_text_repel(aes(x = x, y = y, label = rownames(burnblocks_centroids)), 
                    data = burnblocks_centroids, size = 4)
        + xlab('')
        + ylab('')
      )
print(p)
dev.off()


# Need to gather some stats to annotate next fire demography chart.
unique_blocks <- as.vector(unique(treemeta_manyscars$blockid))
block_stats <- data.frame("blockid" = unique_blocks, "median_interval" = NA)

for (block in unique_blocks) {

  # Subset samples in target burn block.
  block_series <- subset(treemeta_manyscars, blockid == block)$SampleID
  block_fhx <- get_series(fhx_manyscars, as.vector(block_series))

  # Pre-supression
  block_fhx <- get_year(block_fhx, seq(1650, 1899))

  # Use subset to develop unfiltered composite
  subset_composite <- composite(block_fhx,
                                filter_prop = 0, filter_min_rec = 0, 
                                comp_name = block)
  # Use composite to create fire intervals so we can find median interval.
  subset_intervals <- intervals(subset_composite)
  interval_median <- median(subset_intervals)
  block_stats[block_stats$blockid == block, ]$median_interval <- interval_median

}

# Additional columns so that we can use this data in our next plot.
block_stats$label <- paste(block_stats$median_interval,"yrs")
block_stats$x <- 1600  # The year (x-axis) every which we want text.
block_stats$facet_group <- block_stats$blockid


# Remove least populated blocks. Other blocks are our "select", choice, burn blocks.
blockstocrop <- c(3, 7, 12)
treemeta_manyscars_select <- subset(treemeta_manyscars, 
                                    !(blockid %in% blockstocrop))
fhx_manyscars_select <- get_series(fhx_manyscars, 
                                   as.character(treemeta_manyscars_select$SampleID))
block_stats <- subset(block_stats, !(blockid %in% blockstocrop))


# Now for some plotting.
# Output as SVG to current working directly.
svg("fig_bandomap_b.svg", width = 5.5, height = 3.5)
# Start with generic demographic plot, facetted on burnblocks.
p <- plot_demograph(sort(fhx_manyscars_select),
                    facet_group = treemeta_manyscars_select$blockid,
                    facet_id = treemeta_manyscars_select$SampleID,
                    facet_type = "wrap",
                    ylabels = FALSE,
                    yearlims = c(1550, 2010), 
                    plot_legend = FALSE,
                    event_size= c(3, 2, 1.5))

# Now add our annotations to the default demographic plot.
p <- (p + geom_text(aes(y = Inf, x = x, label = label), data = block_stats, 
                    size = 2.75, vjust = 1.2, hjust = 0.5)
# Needed so that we have only 3 plots row in final graphic.
        + scale_x_continuous(breaks = seq(1600, 2000, 100), 
                             labels = c(1600, '', 1800, '', 2000))
        + facet_wrap(~ facet_group, ncol = 3, scales = "free_y")
        + theme(strip.text = element_text(size = 8), 
                strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm")))
      )
print(p)
dev.off()

