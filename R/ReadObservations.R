ReadObservations <- function(file, x.var, y.var, site.var, net.var, alt.var,
                             hole.var, lev.var, acy.var, dt.var, dt.lim,
                             dt.fmt="%Y-%m-%d %H:%M") {

  # Read table file
  obs <- read.table(file=file, header=TRUE, sep="\t", fill=TRUE,
                    strip.white=TRUE, blank.lines.skip=TRUE,
                    allowEscapes=TRUE, flush=TRUE, stringsAsFactors=FALSE)

  # Reduce size of data frame
  obs <- obs[, c(x.var, y.var, site.var, net.var, alt.var, hole.var, lev.var,
                 acy.var, dt.var)]

  # Rename variable names
  var.names <- c("x", "y", "site", "net", "alt", "hole", "lev", "acy", "dt")
  names(obs) <- var.names

  # Force approriate classes
  obs$x    <- as.numeric(obs$x)
  obs$y    <- as.numeric(obs$y)
  obs$site <- as.numeric(obs$site)
  obs$alt  <- as.numeric(obs$alt)
  obs$hole <- as.numeric(obs$hole)
  obs$lev  <- as.numeric(obs$lev)
  obs$acy  <- as.numeric(obs$acy)
  obs$dt   <- as.POSIXct(as.character(obs$dt), format=dt.fmt)

  # Remove NA values
  is.valid.rec <- !(is.na(obs$site) | is.na(obs$alt) | is.na(obs$lev) |
                    is.na(obs$dt))
  obs <- obs[is.valid.rec, ]

  # Determine start and end times for averaging
  if (missing(dt.lim)) {
    dt.lim <- range(obs$dt, na.rm=TRUE)
  } else {
    dt.lim <- as.POSIXct(dt.lim, format=dt.fmt)
  }

  # Build output data frame
  site <- unique(obs$site)
  d <- as.data.frame(site, stringsAsFactors=FALSE)
  n <- nrow(d)
  d[, c("x", "y", "alt", "hole", "acy", "sd", "lev", "net", "alt.lev")] <- NA
  for (i in 1:n) {
    rec <- which(obs$site == d$site[i])
    obs.rec <- obs[rec, ]
    idx <- which(obs.rec$dt >= dt.lim[1] & obs.rec$dt <= dt.lim[2])
    if (length(idx) == 0) {
      d$site[i] <- NA
      next
    }
    d$x[i]    <- obs.rec$x[1]
    d$y[i]    <- obs.rec$y[1]
    d$alt[i]  <- obs.rec$alt[1]
    d$hole[i] <- obs.rec$hole[1]
    d$net[i]  <- obs.rec$net[1]

    d$sd[i] <- sd(obs.rec$lev, na.rm=TRUE)

    obs.idx <- obs.rec[idx, ]
    d$acy[i] <- mean(obs.idx$acy, na.rm=TRUE)
    d$lev[i] <- median(obs.idx$lev, na.rm=TRUE)
  }
  d$alt.lev <- d$alt - d$lev

  d <- d[!is.na(site), ]

  coordinates(d) = ~x+y

  idxs <- zerodist(d, zero=0.0, unique.ID=FALSE)
  if (nrow(idxs) > 0)
    stop()

  d
}
