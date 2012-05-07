ProcessObservations <- function(d, x.var, y.var, projargs, siteno.var,
                                sitenm.var, var1.var, acy.var, dt.var, dt.lim,
                                dt.fmt="%Y-%m-%d %H:%M", net.var=NULL,
                                mapid.var=NULL, var2.var=NULL) {

  # Set column and variable names
  col.names <- c(x.var, y.var, siteno.var, sitenm.var, var1.var, acy.var, dt.var)
  var.names <- c("x", "y", "siteno", "sitenm", "var1", "acy", "dt")
  if (!is.null(net.var)) {
    col.names <- c(col.names, net.var)
    var.names <- c(var.names, "net")
  }
  if (!is.null(mapid.var)) {
    col.names <- c(col.names, mapid.var)
    var.names <- c(var.names, "mapid")
  }
  if (!is.null(var2.var)) {
    col.names <- c(col.names, var2.var)
    var.names <- c(var.names, "var2")
  }

  # Reduce size of data frame
  d <- d[, col.names]
  names(d) <- var.names

  # Force approriate classes
  d$x      <- as.numeric(d$x)
  d$y      <- as.numeric(d$y)
  d$siteno <- as.numeric(d$siteno)
  d$var1   <- as.numeric(d$var1)
  d$acy    <- as.numeric(d$acy)
  d$dt     <- as.POSIXct(as.character(d$dt), format=dt.fmt)
  if (!is.null(var2.var))
    d$var2 <- as.numeric(d$var2)

  # Remove records with missing values
  is.valid.rec <- !(is.na(d$siteno) | is.na(d$var1) | is.na(d$dt))
  d <- d[is.valid.rec, ]

  # Determine start and end times for averaging
  if (missing(dt.lim))
    dt.lim <- range(d$dt, na.rm=TRUE)
  else
    dt.lim <- as.POSIXct(dt.lim, format=dt.fmt)

  # Build output data frame
  siteno <- unique(d$siteno)
  obs <- as.data.frame(siteno, stringsAsFactors=FALSE)

  var.names <- c("x", "y", "sitenm", "mapid")
  if (!is.null(net.var))
    var.names <- c(var.names, "net")
  var.names <- c(var.names, "var1", "acy", "sd")
  if (!is.null(var2.var))
    var.names <- c(var.names, "var2")

  obs[, var.names] <- NA
  for (i in 1:nrow(obs)) {
    obs$mapid[i] <- i

    rec <- which(d$siteno == obs$siteno[i])
    d.rec <- d[rec, ]
    idx <- which(d.rec$dt >= dt.lim[1] & d.rec$dt <= dt.lim[2])
    if (length(idx) == 0) {
      obs$siteno[i] <- NA
      next
    }

    obs$x[i] <- d.rec$x[1]
    obs$y[i] <- d.rec$y[1]
    obs$sitenm[i] <- d.rec$sitenm[1]
    if (!is.null(net.var))
      obs$net[i] <- d.rec$net[1]
    if (!is.null(mapid.var))
      obs$mapid[i] <- obs$mapid[1]
    if (!is.null(var2.var))
      obs$var2[i] <- d.rec$var2[1]

    obs$sd[i] <- sd(d.rec$var1, na.rm=TRUE)
    d.idx <- d.rec[idx, ]
    obs$acy[i]  <- mean(d.idx$acy, na.rm=TRUE)
    obs$var1[i] <- median(d.idx$var1, na.rm=TRUE)
  }

  obs <- obs[!is.na(obs$siteno), ]
  coordinates(obs) = ~x+y
  idxs <- zerodist(obs, zero=0.0, unique.ID=FALSE)
  if (nrow(idxs) > 0) {
    print(cbind(obs$sitenm[idxs[, 1]], obs$sitenm[idxs[, 2]]))
    stop("duplicate coordinates not permitted")
  }

  # Add projection
  proj4string(obs) <- CRS(projargs)

  obs
}
