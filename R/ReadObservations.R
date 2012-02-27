ReadObservations <- function(file, x.var, y.var, site.var, net.var,
                             var1.var, var2.var, acy.var, dt.var, dt.lim,
                             dt.fmt="%Y-%m-%d %H:%M", sep=",") {

  # Read table file
  obs <- read.table(file=file, header=TRUE, sep=sep, fill=TRUE,
                    strip.white=TRUE, blank.lines.skip=TRUE,
                    allowEscapes=TRUE, flush=TRUE, stringsAsFactors=FALSE)

  # Reduce size of data frame
  obs <- obs[, c(x.var, y.var, site.var, net.var, var1.var, var2.var,
                 acy.var, dt.var)]

  # Rename variable names
  var.names <- c("x", "y", "site", "net", "var1", "var2", "acy", "dt")
  names(obs) <- var.names

  # Force approriate classes
  obs$x     <- as.numeric(obs$x)
  obs$y     <- as.numeric(obs$y)
  obs$site  <- as.numeric(obs$site)
  obs$var1  <- as.numeric(obs$var1)
  obs$var2  <- as.numeric(obs$var2)
  obs$acy   <- as.numeric(obs$acy)
  obs$dt    <- as.POSIXct(as.character(obs$dt), format=dt.fmt)

  # Remove NA values
  is.valid.rec <- !(is.na(obs$site) | is.na(obs$var1) | is.na(obs$var2) |
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
  d[, c("x", "y", "mapid", "var1", "var2", "acy", "sd", "net")] <- NA
  for (i in 1:n) {
    d$mapid[i] <- i

    rec <- which(obs$site == d$site[i])
    obs.rec <- obs[rec, ]
    idx <- which(obs.rec$dt >= dt.lim[1] & obs.rec$dt <= dt.lim[2])
    if (length(idx) == 0) {
      d$site[i] <- NA
      next
    }

    d$x[i]    <- obs.rec$x[1]
    d$y[i]    <- obs.rec$y[1]
    d$var2[i] <- obs.rec$var2[1]
    d$net[i]  <- obs.rec$net[1]

    d$sd[i] <- sd(obs.rec$var1, na.rm=TRUE)

    obs.idx <- obs.rec[idx, ]
    d$acy[i]  <- mean(obs.idx$acy, na.rm=TRUE)
    d$var1[i] <- median(obs.idx$var1, na.rm=TRUE)
  }

  d <- d[!is.na(site), ]

  coordinates(d) = ~x+y

  idxs <- zerodist(d, zero=0.0, unique.ID=FALSE)
  if (nrow(idxs) > 0)
    stop()

  d
}
