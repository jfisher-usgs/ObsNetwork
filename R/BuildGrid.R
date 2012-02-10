BuildGrid <- function(x, file, x.var, y.var, compute.chull=FALSE, dx=0.03, dy=dx,
                      projargs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") {

  if (missing(x)) {
    x <- read.table(file, header=TRUE, sep="\t", fill=TRUE,
                    strip.white=TRUE, blank.lines.skip=TRUE,
                    allowEscapes=TRUE, flush=TRUE)
    x <- x[, make.names(c(x.var, y.var), unique=TRUE)]
  } else {
    x <- coordinates(x)
  }

  # Rename variable names
  names(x) <- c("x", "y")

  # Compute convex hull
  if (compute.chull)
    x <- x[chull(x), ]

  # Convert to spatial points
  ply <- SpatialPoints(x)
  proj4string(ply) <- CRS(projargs)
  new.projargs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ply <- spTransform(ply, CRS(new.projargs))

  # Determine grid limits
  xlim <- extendrange(ply@coords[, 1])
  ylim <- extendrange(ply@coords[, 2])

  # Determine grid intervals
  xseq <- seq(xlim[1], dx * ceiling(xlim[2] / dx), by=dx)
  yseq <- seq(ylim[1], dy * ceiling(ylim[2] / dy), by=dy)

  # Form grid
  grd <- expand.grid(x=xseq, y=yseq)
  coordinates(grd) <- as.formula("~x+y")
  proj4string(grd) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  gridded(grd) <- TRUE

  # Eliminate grid celss lying outside spatial domain polygon
  is.in.ply <- as.logical(point.in.polygon(grd@coords[, 1], grd@coords[, 2],
                                           ply@coords[, 1], ply@coords[, 2]))
  grd <- grd[is.in.ply]

  grd
}
