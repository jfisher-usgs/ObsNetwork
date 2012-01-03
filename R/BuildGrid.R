BuildGrid <- function(file, lng.var, lat.var, dx=0.03, dy=dx,
               projargs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") {

  # Read data from file
  ply <- read.table(file, header=TRUE, sep="\t", fill=TRUE,
                    strip.white=TRUE, blank.lines.skip=TRUE,
                    allowEscapes=TRUE, flush=TRUE)

  # Reduce date frame size
  ply <- ply[, c(lng.var, lat.var)]

  # Rename variable names
  names(ply) <- c("longitude", "latitude")

  # Convert to spatial points
  ply <- SpatialPoints(ply)
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
  grd <- expand.grid(longitude=xseq, latitude=yseq)
  coordinates(grd) <- as.formula("~ longitude + latitude")
  proj4string(grd) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  gridded(grd) <- TRUE

  # Eliminate grid celss lying outside spatial domain polygon
  is.in.ply <- as.logical(point.in.polygon(grd@coords[, 1], grd@coords[, 2],
                                           ply@coords[, 1], ply@coords[, 2]))
  grd <- grd[is.in.ply]

  grd
}
