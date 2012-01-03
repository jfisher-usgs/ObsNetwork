ReadObservations <- function(file, site.var, obs.var, acc.var, lng.var, lat.var,
                             alt.var, dt.var, dt.fmt="%Y%m%d",
                             projargs="+proj=longlat +datum=NAD83") {

  # Read data from file
  obs <- read.table(file=file, header=TRUE, sep="\t", fill=TRUE,
                    strip.white=TRUE, blank.lines.skip=TRUE,
                    allowEscapes=TRUE, flush=TRUE)

  # Reduce date frame size
  obs <- obs[, c(lng.var, lat.var, site.var, dt.var, obs.var, acc.var, alt.var)]

  # Rename variable names
  names(obs) <- c("longitude", "latitude", "site", "datetime", "observation",
                  "accuracy", "altitude")

  # Convert coordinate reference system
  coordinates(obs) = as.formula("~ longitude + latitude")
  proj4string(obs) <- CRS(projargs)
  new.projargs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  obs <- spTransform(obs, CRS(new.projargs))

  # Force approriate classes
  obs$site <- as.factor(obs$site)
  obs$datetime <- as.POSIXct(as.character(obs$datetime), format=dt.fmt)
  obs$observation <- as.numeric(obs$observation)
  obs$accuracy <- as.numeric(obs$accuracy)

  obs
}
