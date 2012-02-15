ReadObservations <- function(file, x.var, y.var, site.var, alt.var, hole.var,
                             lev.var, acy.var, dt.var, dt.lim,
                             dt.fmt="%Y-%m-%d %H:%M",
                             projargs="+proj=longlat +datum=NAD83") {

  # Read data from file
  d <- read.table(file=file, header=TRUE, sep="\t", fill=TRUE,
                  strip.white=TRUE, blank.lines.skip=TRUE,
                  allowEscapes=TRUE, flush=TRUE)

  # Reduce date frame size
  d <- d[, make.names(c(x.var, y.var, site.var, alt.var, hole.var,
                        lev.var, acy.var, dt.var), unique=TRUE)]

  # Rename variable names
  var.names <- c("x", "y", "site", "alt", "hole", "lev", "acy", "dt")
  names(d) <- var.names

  # Force approriate classes
  d$site <- as.numeric(d$site)
  d$alt  <- as.numeric(d$alt)
  d$hole <- as.numeric(d$hole)
  d$lev  <- as.numeric(d$lev)
  d$acy  <- as.numeric(d$acy)
  d$dt   <- as.POSIXct(as.character(d$dt), format=dt.fmt)

  # Remove NA values
  is.valid.rec <- !(is.na(d$x) | is.na(d$y) | is.na(d$site) | is.na(d$alt) |
                    is.na(d$lev) | is.na(d$dt))
  d <- d[is.valid.rec, ]

  # Determine start and end times for averaging
  if (missing(dt.lim))
    dt.lim <- range(d$dt, na.rm=TRUE)
  else
    dt.lim <- as.POSIXct(dt.lim, format=dt.fmt)

  # Build output table
  site <- unique(d$site)
  n <- length(site)
  x <- y <- alt <- hole <- acy.avg <- lev.sd <- lev.avg <- rep(NA, n)
  for (i in 1:n) {
    rec <- which(d$site == site[i])
    d.rec <- d[rec, ]
    idx <- which(d.rec$dt >= dt.lim[1] & d.rec$dt <= dt.lim[2])
    if (length(idx) == 0) {
      site[i] <- NA
      next
    }
    x[i]    <- d.rec$x[1]
    y[i]    <- d.rec$y[1]
    alt[i]  <- d.rec$alt[1]
    hole[i] <- d.rec$hole[1]
    lev.sd[i] <- sd(d.rec$lev, na.rm=TRUE)

    d.idx <- d.rec[idx, ]
    acy.avg[i] <- mean(d.idx$acy, na.rm=TRUE)
    lev.avg[i] <- median(d.idx$lev, na.rm=TRUE)
  }
  obs <- as.data.frame(list(x=x, y=y, site=site, alt=alt, hole=hole,
                            lev=lev.avg, acy=acy.avg, sd=lev.sd))
  obs <- obs[!is.na(site), ]

  # Set coordinate reference system
  coordinates(obs) = as.formula("~x+y")
  proj4string(obs) <- CRS(projargs)
  obs <- rgdal::spTransform(obs, CRS("+proj=longlat +datum=NAD83"))

  obs
}
