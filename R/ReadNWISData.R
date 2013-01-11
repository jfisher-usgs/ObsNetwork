ReadNWISData <- function(file, dt.lim, dt.fmt="%Y-%m-%d %H:%M", sep="\t") {
  
  # Read data
  d <- read.table(file=file, sep=sep, header=TRUE, fill=TRUE, flush=TRUE,
                strip.white=TRUE, blank.lines.skip=TRUE, allowEscapes=TRUE, 
                stringsAsFactors=FALSE)
  
  # Reformat station name
  fun <- function(i) {
    x <- unlist(strsplit(i, " "))
    paste(x[x != ""], collapse=" ")
  }
  d$STATION_NM <- as.character(sapply(d$STATION_NM, fun))
  
  # Convert date-time variable to POSIX class
  d$LEV_DT <- as.POSIXct(as.character(d$LEV_DT), format="%Y-%m-%d %H:%M:%S")
  
  # Convert units from feet to meters
  vars.ft <- c("ALT_VA", "ALT_ACY_VA", "HOLE_DEPTH_VA", "WELL_DEPTH_VA", "LEV_VA")
  vars.ft <- vars.ft[vars.ft %in% names(d)]
  for (i in seq(along=vars.ft)) {
    d[, vars.ft[i]] <- as.numeric(d[, vars.ft[i]]) * 0.3048
  }
  
  # Level accuracy code, in meters
  code <- as.character(d$LEV_ACY_CD)
  accy <- rep(NA, length(code))
  accy[code == "0"] <- 1    * 0.3048
  accy[code == "1"] <- 0.1  * 0.3048
  accy[code == "2"] <- 0.01 * 0.3048
  d$LEV_ACY_VA <- accy
  
  # Coordinate accuracy code, convert to arcsecond
  if ("COORD_ACY_CD" %in% names(d)) {
    code <- as.character(d$COORD_ACY_CD)
    accy <- rep(NA, length(code))
    accy[code == "H"] <-  0.01
    accy[code == "1"] <-  0.1
    accy[code == "5"] <-  0.5
    accy[code == "S"] <-  1
    accy[code == "R"] <-  3
    accy[code == "F"] <-  5
    accy[code == "T"] <- 10
    accy[code == "M"] <- 60
    d$COORD_ACY_VA <- accy
  }
  
  # Coordinate method code
  if ("COORD_METH_CD" %in% names(d)) {
    code <- as.character(d$COORD_METH_CD)
    meth <- rep(NA, length(code))
    meth[code == "D"] <- "DGPS"
    meth[code == "G"] <- "GPS"
    meth[code == "L"] <- "LORAN"
    meth[code == "M"] <- "map"
    meth[code == "S"] <- "survey"
    meth[code == "U"] <- "unknown"
    d$COORD_METH_CD <- meth
  }
  
  # Altitude method code
  if ("ALT_METH_CD" %in% names(d)) {
    code <- as.character(d$ALT_METH_CD)
    meth <- rep(NA, length(code))
    meth[code == "A"] <- "altimeter"
    meth[code == "D"] <- "DGPS"
    meth[code == "G"] <- "GPS"
    meth[code == "L"] <- "LORAN"
    meth[code == "M"] <- "map"
    meth[code == "R"] <- "reported"
    meth[code == "U"] <- "unknown"
    d$ALT_METH_CD <- meth
  }
  
  # Level method code
  if ("LEV_METH_CD" %in% names(d)) {
  code <- as.character(d$LEV_METH_CD)
    meth <- rep(NA, length(code))
    meth[code == "A"] <- "airline"
    meth[code == "B"] <- "analog"
    meth[code == "C"] <- "calibrated airline"
    meth[code == "E"] <- "estimated"
    meth[code == "G"] <- "pressure gage"
    meth[code == "H"] <- "calibrated press. gage"
    meth[code == "L"] <- "geophysical"
    meth[code == "M"] <- "manometer"
    meth[code == "N"] <- "non-rec.gage"
    meth[code == "R"] <- "reported"
    meth[code == "S"] <- "steel tape"
    meth[code == "T"] <- "electric tape"
    meth[code == "V"] <- "calibrated elec. tape"
    meth[code == "Z"] <- "other"
    d$LEV_METH_CD <- meth
  }
  
  # Determine water level elevation, in meters
  d$ALT_LEV_VA <- d$ALT_VA - d$LEV_VA
  
  # Determine accuracy of water level elevation
  d$ALT_LEV_ACY_VA <- d$ALT_ACY_VA + d$LEV_ACY_VA
  
  # Check for invalid records
  invalid.rec <- which(is.na(d$LEV_DT) | is.na(d$ALT_LEV_VA))
  if (length(invalid.rec) > 0) {
    d <- d[-invalid.rec, ]
    warning("Number of records removed because of NA values: ", 
            length(invalid.rec))
    if (nrow(d) == 0)
      stop("data frame empty")
  }
  
  # Determine start and end times for averaging
  if (missing(dt.lim)) {
    dt.lim <- range(d$LEV_DT, na.rm=TRUE)
  } else {
    dt.lim <- as.POSIXct(dt.lim, format=dt.fmt)
  }

  # Initialize output data table
  vars <- c("x", "y", "siteno", "var1", "acy", "sd",
            "mapno", "network", "nrec.por", "nrec",
            "alt.va", "alt.acy.va", "lev.acy.va",
            "coord.acy.va", "coord.meth.cd", "alt.meth.cd", "lev.meth.cd", 
            "sitenm")
  sitenos <- unique(d$SITE_NO)
  m <- length(sitenos) # rows
  n <- length(vars)    # columns
  dd <- as.data.frame(matrix(NA, nrow=m, ncol=n, dimnames=list(1:m, vars)))
  
  dd$siteno <- sitenos # site numbers
  dd$mapno <- 1:m # default map numbers
  
  # Loop through site numbers
  for (i in 1:m) {
    siteno <- dd$siteno[i]
    
    # Create new table from records corresponding to current site number
    rec <- which(d$SITE_NO == siteno)
    d.rec <- d[rec, ]
    
    dd$nrec.por[i]   <- nrow(d.rec)          # number of records in period-of-record
    dd$sitenm[i]     <- d.rec$STATION_NM[1]  # site name
    dd$x[i]          <- d.rec$DEC_LONG_VA[1] # decimal longitude
    dd$y[i]          <- d.rec$DEC_LAT_VA[1]  # decimal latitude
    dd$alt.va[i]     <- d.rec$ALT_VA[1]      # land-surface reference point elev.
    dd$alt.acy.va[i] <- d.rec$ALT_ACY_VA[1]  # accuracy of referece point
    
    if ("NETWORK" %in% names(d)) 
      dd$network[i] <- d.rec$NETWORK[1]
    if ("MAP_NO" %in% names(d))
      dd$mapno[i] <- d.rec$MAP_NO[1]
    if ("COORD_ACY_VA" %in% names(d))
      dd$coord.acy.va[i] <- d.rec$COORD_ACY_VA[1]
    
    if ("COORD_METH_CD" %in% names(d))
      dd$coord.meth.cd[i] <- paste(unique(na.omit(d.rec$COORD_METH_CD)), collapse=", ")
    if ("ALT_METH_CD" %in% names(d))
      dd$alt.meth.cd[i] <- paste(unique(na.omit(d.rec$ALT_METH_CD)), collapse=", ")
    if ("LEV_METH_CD" %in% names(d))
      dd$lev.meth.cd[i] <- paste(unique(na.omit(d.rec$LEV_METH_CD)), collapse=", ")
    
    dd$sd[i] <- sd(d.rec$ALT_LEV_VA, na.rm=TRUE) # standard deviation of water-level elev.
    
    # Create new table from records corresponding to date-time limits
    rec.lim <- which(d.rec$LEV_DT >= dt.lim[1] & d.rec$LEV_DT <= dt.lim[2])
    if (length(rec.lim) == 0)
      next
    d.rec.lim <- d.rec[rec.lim, ]
    
    dd$nrec[i]       <- nrow(d.rec.lim)
    dd$lev.acy.va[i] <- mean(d.rec.lim$LEV_ACY_VA, na.rm=TRUE)     # accuracy of water level
    dd$var1[i]       <- median(d.rec.lim$ALT_LEV_VA, na.rm=TRUE)   # water-level elev.
    dd$acy[i]        <- mean(d.rec.lim$ALT_LEV_ACY_VA, na.rm=TRUE) # accuracy of water-level elev.
  }
  
  # Convert data frame to spatial data frame
  require(sp)
  coordinates(dd) = ~x+y
  idxs <- zerodist(dd, zero=0.0, unique.ID=FALSE)
  if (nrow(idxs) > 0) {
    print(cbind(dd$sitenm[idxs[, 1]], dd$sitenm[idxs[, 2]]))
    stop("duplicate coordinates not permitted")
  }

  # Add projection
  projargs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  proj4string(dd) <- CRS(projargs)

  invisible(dd)
}
