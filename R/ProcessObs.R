ProcessObs <- function(d, x.var, y.var, projargs, site.var, net.var,
                       var1.var, var2.var, acy.var, dt.var, dt.lim,
                       dt.fmt="%Y-%m-%d %H:%M") {

  # Reduce size of data frame
  d <- d[, c(x.var, y.var, site.var, net.var, var1.var, var2.var, acy.var,
             dt.var)]

  # Rename variable names
  var.names <- c("x", "y", "site", "net", "var1", "var2", "acy", "dt")
  names(d) <- var.names

  # Force approriate classes
  d$x    <- as.numeric(d$x)
  d$y    <- as.numeric(d$y)
  d$site <- as.numeric(d$site)
  d$var1 <- as.numeric(d$var1)
  d$var2 <- as.numeric(d$var2)
  d$acy  <- as.numeric(d$acy)
  d$dt   <- as.POSIXct(as.character(d$dt), format=dt.fmt)

  # Remove NA values
  is.valid.rec <- !(is.na(d$site) | is.na(d$var1) | is.na(d$var2) | is.na(d$dt))
  d <- d[is.valid.rec, ]

  # Determine start and end times for averaging
  if (missing(dt.lim)) {
    dt.lim <- range(d$dt, na.rm=TRUE)
  } else {
    dt.lim <- as.POSIXct(dt.lim, format=dt.fmt)
  }

  # Build output data frame
  site <- unique(d$site)
  obs <- as.data.frame(site, stringsAsFactors=FALSE)
  n <- nrow(obs)
  obs[, c("x", "y", "mapid", "var1", "var2", "acy", "sd", "net")] <- NA
  for (i in 1:n) {
    obs$mapid[i] <- i

    rec <- which(d$site == obs$site[i])
    d.rec <- d[rec, ]
    idx <- which(d.rec$dt >= dt.lim[1] & d.rec$dt <= dt.lim[2])
    if (length(idx) == 0) {
      obs$site[i] <- NA
      next
    }

    obs$x[i]    <- d.rec$x[1]
    obs$y[i]    <- d.rec$y[1]
    obs$var2[i] <- d.rec$var2[1]
    obs$net[i]  <- d.rec$net[1]

    obs$sd[i] <- sd(d.rec$var1, na.rm=TRUE)

    d.idx <- d.rec[idx, ]
    obs$acy[i]  <- mean(d.idx$acy, na.rm=TRUE)
    obs$var1[i] <- median(d.idx$var1, na.rm=TRUE)
  }

  obs <- obs[!is.na(site), ]

  coordinates(obs) = ~x+y

  idxs <- zerodist(obs, zero=0.0, unique.ID=FALSE)
  if (nrow(idxs) > 0)
    stop()

  # Add projection
  proj4string(obs) <- CRS(projargs)

  obs
}
