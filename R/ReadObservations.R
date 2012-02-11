ReadObservations <- function(file, x.var, y.var, site.var, obs.var, acy.var,
                             dt.var, dt.lim, dt.fmt="%Y-%m-%d %H:%M",
                             projargs="+proj=longlat +datum=NAD83") {

  # Read data from file
  d <- read.table(file=file, header=TRUE, sep="\t", fill=TRUE,
                  strip.white=TRUE, blank.lines.skip=TRUE,
                  allowEscapes=TRUE, flush=TRUE)

  # Reduce date frame size
  d <- d[, make.names(c(x.var, y.var, site.var, obs.var, acy.var, dt.var),
                      unique=TRUE)]

  # Rename variable names
  var.names <- c("x", "y", "site", "observation", "accuracy", "datetime")
  names(d) <- var.names

  # Force approriate classes
  d$site <- as.numeric(d$site)
  d$observation <- as.numeric(d$observation)
  d$accuracy <- as.numeric(d$accuracy)
  d$datetime <- as.POSIXct(as.character(d$datetime), format=dt.fmt)

  # Remove NA values for site and observation
  is.valid.rec <- !(is.na(d$x) | is.na(d$y) | is.na(d$site) |
                    is.na(d$observation) | is.na(d$datetime))
  d <- d[is.valid.rec, ]

  # Determine start and end times for averaging
  if (missing(dt.lim))
    dt.lim <- range(d$datetime, na.rm=TRUE)
  else
    dt.lim <- as.POSIXct(dt.lim, format=dt.fmt)

  # Build output table
  site <- unique(d$site)
  n <- length(site)
  x <- y <- acy <- sdv <- avg <- rep(NA, n)
  for (i in 1:n) {
    rec <- which(d$site == site[i])
    d.rec <- d[rec, ]
    idx <- which(d.rec$datetime >= dt.lim[1] & d.rec$datetime <= dt.lim[2])
    if (length(idx) == 0) {
      site[i] <- NA
      next
    }
    x[i] <- d.rec$x[1]
    y[i] <- d.rec$y[1]
    sdv[i] <- sd(d.rec$observation, na.rm=TRUE)

    d.idx <- d.rec[idx, ]
    acy[i] <- mean(d.idx$accuracy, na.rm=TRUE)
    avg[i] <- mean(d.idx$observation, na.rm=TRUE)
  }
  obs <- as.data.frame(list(x=x, y=y, site=site, observation=avg,
                            accuracy=acy, std.dev=sdv))
  obs <- obs[!is.na(site), ]

  # Set coordinate reference system
  coordinates(obs) = as.formula("~x+y")
  proj4string(obs) <- CRS(projargs)
  new.projargs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  obs <- rgdal::spTransform(obs, CRS(new.projargs))

  obs
}
