# 2017-07-12
# Fix use of Stamen terrain tiles in ggmap.

require(ggmap)

get_stamenmap <- function (bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, 
                   top = 30.14344), zoom = 10, maptype = c("terrain", "terrain-background", 
                                                           "terrain-labels", "terrain-lines", "toner", "toner-2010", 
                                                           "toner-2011", "toner-background", "toner-hybrid", "toner-labels", 
                                                           "toner-lines", "toner-lite", "watercolor"), crop = TRUE, 
          messaging = FALSE, urlonly = FALSE, color = c("color", "bw"), 
          force = FALSE, where = tempdir(), ...) 
{
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)
  if ("bbox" %in% argsgiven) {
    if (!(is.numeric(bbox) && length(bbox) == 4)) {
      stop("bounding box improperly specified.  see ?get_openstreetmap", 
           call. = F)
    }
  }
  if ("zoom" %in% argsgiven) {
    if (!(is.numeric(zoom) && length(zoom) == 1 && zoom == 
          round(zoom) && zoom >= 0 && zoom <= 18)) {
      stop("scale must be a postive integer 0-18, see ?get_stamenmap.", 
           call. = F)
    }
  }
  if ("messaging" %in% argsgiven) 
    stopifnot(is.logical(messaging))
  if ("urlonly" %in% argsgiven) 
    stopifnot(is.logical(urlonly))
  if ("checkargs" %in% argsgiven) {
    .Deprecated(msg = "checkargs argument deprecated, args are always checked after v2.1.")
  }
  maptype <- match.arg(maptype)
  color <- match.arg(color)
  if (is.null(names(bbox))) 
    names(bbox) <- c("left", "bottom", "right", "top")
  if (maptype %in% c("terrain", "terrain-background", "watercolor")) {
    filetype <- "png"
  }
  else {
    filetype <- "png"
  }
  fourCorners <- expand.grid(lon = c(bbox["left"], bbox["right"]), 
                             lat = c(bbox["bottom"], bbox["top"]))
  fourCorners$zoom <- zoom
  row.names(fourCorners) <- c("lowerleft", "lowerright", "upperleft", 
                              "upperright")
  fourCornersTiles <- apply(fourCorners, 1, function(v) LonLat2XY(v[1], 
                                                                  v[2], v[3]))
  xsNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, 
                                                        function(df) df$X)))))
  numXTiles <- length(xsNeeded)
  ysNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, 
                                                        function(df) df$Y)))))
  numYTiles <- length(ysNeeded)
  tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
  if (nrow(tilesNeeded) > 40) {
    message(paste0(nrow(tilesNeeded), " tiles needed, this may take a while ", 
                   "(try a smaller zoom)."))
  }
  base_url <- "http://tile.stamen.com/"
  base_url <- paste(base_url, maptype, "/", zoom, sep = "")
  urls <- paste(base_url, apply(tilesNeeded, 1, paste, collapse = "/"), 
                sep = "/")
  urls <- paste(urls, filetype, sep = ".")
  if (messaging) 
    message(length(urls), " tiles required.")
  if (urlonly) 
    return(urls)
  count <- 0
  nTiles <- nrow(tilesNeeded)
  listOfTiles <- lapply(split(tilesNeeded, 1:nrow(tilesNeeded)), 
                        function(v) {
                          v <- as.numeric(v)
                          get_stamenmap_tile(maptype, zoom, v[1], v[2], force = force, 
                                             messaging = messaging)
                        })
  map <- ggmap:::stitch(listOfTiles)
  if (!crop) {
    attr(map, "source") <- "stamen"
    attr(map, "maptype") <- maptype
    attr(map, "zoom") <- zoom
    return(map)
  }
  if (crop) {
    mbbox <- attr(map, "bb")
    size <- 256 * c(length(xsNeeded), length(ysNeeded))
    slon <- seq(mbbox$ll.lon, mbbox$ur.lon, length.out = size[1])
    slat <- vector("double", length = 256 * length(ysNeeded))
    for (k in seq_along(ysNeeded)) {
      slat[(k - 1) * 256 + 1:256] <- sapply(as.list(0:255), 
                                            function(y) {
                                              XY2LonLat(X = xsNeeded[1], Y = ysNeeded[k], 
                                                        zoom, x = 0, y = y)$lat
                                            })
    }
    slat <- rev(slat)
    keep_x_ndcs <- which(bbox["left"] <= slon & slon <= bbox["right"])
    keep_y_ndcs <- sort(size[2] - which(bbox["bottom"] <= 
                                          slat & slat <= bbox["top"]))
    croppedmap <- map[keep_y_ndcs, keep_x_ndcs]
  }
  croppedmap <- as.raster(croppedmap)
  class(croppedmap) <- c("ggmap", "raster")
  attr(croppedmap, "bb") <- data.frame(ll.lat = bbox["bottom"], 
                                       ll.lon = bbox["left"], ur.lat = bbox["top"], ur.lon = bbox["right"])
  attr(croppedmap, "source") <- "stamen"
  attr(croppedmap, "maptype") <- maptype
  attr(croppedmap, "zoom") <- zoom
  croppedmap
}
assignInNamespace("get_stamenmap",get_stamenmap,ns="ggmap")

get_stamenmap_tile <- function (maptype, zoom, x, y, force = FALSE, messaging = TRUE, 
          where = tempdir()) 
{
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
                                                                     round(x)) < tol
  stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
  stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
  stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
  if (maptype %in% c("terrain", "terrain-background", "watercolor")) {
    filetype <- "jpg"
  }
  else {
    filetype <- "png"
  }
  url <- paste0(paste0(c("http://tile.stamen.com", maptype, 
                         zoom, x, y), collapse = "/"), ".", filetype)
  tile <- ggmap:::file_drawer_get(url)
  if (!is.null(tile) && !force) 
    return(tile)
  tmp <- tempfile()
  download.file(url, destfile = tmp, quiet = !messaging, mode = "wb")
  if (TRUE) 
    message(paste0("Map from URL : ", url))
  if (maptype %in% c("terrain", "terrain-background", "watercolor")) {
    tile <- png::readPNG(tmp)
  }
  else {
    tile <- png::readPNG(tmp)
  }
  if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
                     "terrain-labels", "terrain-lines")) {
    tile <- t(apply(tile, 1:2, function(x) rgb(x[1], x[2], 
                                               x[3], x[4])))
  }
  else {
    tile <- t(apply(tile, 2, rgb))
  }
  lonlat_upperleft <- XY2LonLat(x, y, zoom)
  lonlat_lowerright <- XY2LonLat(x, y, zoom, 255, 255)
  bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
            right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
  bb <- data.frame(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
                   ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
  class(tile) <- c("ggmap", "raster")
  attr(tile, "bb") <- bb
  ggmap:::file_drawer_set(url, tile)
  tile
}
assignInNamespace("get_stamenmap_tile",get_stamenmap_tile,ns="ggmap")
