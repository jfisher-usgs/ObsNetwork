RemoveSites <- function(nsites=10, niters=200, pop.size=200, dx=0.03, dy=dx) {

  # Load required packages
  require(gstat)
  require(rgdal)
  require(genalg)

  # Read site data from file
  f <- file.path(getwd(), "inst", "extdata", "ObservationData.txt")
  obs <- read.table(file=f, header=TRUE, sep="\t", fill=TRUE, strip.white=TRUE,
                    blank.lines.skip=TRUE, allowEscapes=TRUE, flush=TRUE)

  # Calculate water-level elevation
  obs[, "WL_elev"] <- obs[, "Altitude"] - obs[, "WL"]

  # Specify coordinate variables
  coordinates(obs) = ~Longitude+Latitude
  proj4string(obs) <- CRS("+proj=longlat +datum=NAD83")
  obs <- spTransform(obs,
                     CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

  # Read polygon data from file
  f <- file.path(getwd(), "inst", "extdata", "SpatialDomain.ply")
  pol <- read.table(f, header=FALSE, sep=" ", col.names=c("x", "y"), skip=3)

  # Create a grid onto which to interpolate
  x.range <- extendrange(pol$x)
  y.range <- extendrange(pol$y)
  grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=dx),
                     y=seq(from=y.range[1], to=y.range[2], by=dy))
  coordinates(grd) <- ~x+y
  proj4string(grd) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  gridded(grd) <- TRUE

  # Eliminate grid points outside polygon
  is.in.pol <- as.logical(point.in.polygon(grd@coords[, 1], grd@coords[, 2],
                                           pol$x, pol$y))
  grd <- grd[is.in.pol]

  # Make gstat object
  g <- gstat(id="WL_elev", formula=WL_elev~1, data=obs)

  # Calculate variogram
  v <- variogram(g)
  v.fit <- fit.variogram(v, model=vgm(model='Lin'))

  # Plot variogram
  # plot(v, model=v.fit, xlab="Lag distance", ylab="Semivariance",
  #      col="black", pch=21)

  # Update gstat object with variogram model
  g <- gstat(g, id="WL_elev", model=v.fit)

  # Perform ordinary kriging prediction
  p.grd <- predict(g, newdata=grd)
  p.obs <- predict(g, newdata=obs)


  # Correlation
  corr <- matrix(NA, nrow=nrow(obs), ncol=2,
                 dimnames=list(1:nrow(obs), c("measured", "estimated")))
  for (i in 1:nrow(obs)) {
    d <- obs[-i, ]
    g <- gstat(id="WL_elev", formula=WL_elev~1, data=d, model=v.fit)
    p <- predict(g, newdata=obs[i, ])
    corr[i, ] <- c(obs[i, ]$WL_elev, p$WL_elev.pred)
  }
  x <- corr[, "measured"]
  y <- corr[, "estimated"]
  # plot(x, y, asp=1, xlab="Measured values", ylab="Estimated values")
  # lines(x=range(x), y=range(x), col="gray")

  x <- corr[, "estimated"]
  y <- corr[, "measured"] - corr[, "estimated"]
  # plot(x, y, xlab="Estimated values", ylab="Estimation error")
  # lines(x=range(x), y=c(0, 0), col="gray")

  # Genetic alogrithm

  # Calculate objectives
  CalcObjective <- function(idxs) {
    d <- obs[-idxs, ]
    g <- gstat(id="WL_elev", formula=WL_elev~1, data=d, model=v.fit)
    p <- predict(g, newdata=grd)
    obj.err <- mean(sqrt(abs(p$WL_elev.var)))
    p <- predict(g, newdata=obs[idxs, ])
    obj.obs <- sum((p$WL_elev.pred - p.obs$WL_elev.pred[idxs])^2)
    c(obj.err, obj.obs)
  }

  # Evaluate objective function in GA
  Evaluate <- function(string) {
    idxs <- round(string, digits=0)
    if (length(unique(idxs)) < length(idxs))
      return(1E15)
    objs <- CalcObjective(idxs)
    sum(objs)
  }

  # Monitoring progress at end of each iteration in GA
  Monitor <- function(obj) {
    n <- obj$iter
    idxs <- GetIdxsForBestSolution(obj)
    objs <- CalcObjective(idxs)
    obj.values[n, ] <<- c(objs, sum(objs))

    x <- 1:n
    y1 <- obj.values[x, "err"]
    y2 <- obj.values[x, "obs"]

    par(mar=c(5, 4, 3, 4))
    plot(x, y1, type="o", pch=22, col="black", bg="blue",
         xlab="Iteration", ylab="Prediction error")
    par(new=TRUE)
    plot(x, y2, type="o", pch=21, col="black", bg="red",
         xaxt="n", yaxt="n", xlab="", ylab="")
    axis(4)
    mtext("Observation error", side=4, line=2.8)
    legend("topright", c("Prediction error", "Observation error"),
           pch=c(22, 21), pt.bg=c("blue", "red"), inset=0.02)
  }

  # Get indexes for best GA solution
  GetIdxsForBestSolution <- function(rbga.results) {
    filter <- rbga.results$evaluations == min(rbga.results$evaluations)
    best.obj.count <- sum(rep(1, rbga.results$popSize)[filter])
    if (best.obj.count > 1)
      best.solution <- rbga.results$population[filter, ][1, ]
    else
      best.solution <- rbga.results$population[filter, ]
    idxs <- sort(round(best.solution, digits=0))
    idxs
  }

  # Run GA

  obj.values <- matrix(NA, nrow=niters, ncol=3,
                       dimnames=list(1:niters, c("err", "obs", "tot")))
  ans <- rbga(stringMin=rep(1, nsites), stringMax=rep(length(obs), nsites),
              popSize=pop.size, iters=niters, verbose=TRUE,
              monitorFunc=Monitor, evalFunc=Evaluate)
  summary.rbga(ans, echo=TRUE)
  idxs <- GetIdxsForBestSolution(ans)
  obs.rm <- obs[idxs, ]

  # Plot prediction and prediction error

  d <- obs[-idxs, ]
  g <- gstat(id="WL_elev", formula=WL_elev~1, data=d, model=v.fit)
  p <- predict(g, newdata=grd)
  p.grd$WL_elev.pred.new <- p$WL_elev.pred
  p.grd$WL_elev.se <- sqrt(abs(p.grd$WL_elev.var))
  p.grd$WL_elev.se.new <- sqrt(p$WL_elev.var)

  obs.pts <- list("sp.points", obs, pch=4, col="black", cex=0.5, which=2)
  obs.pts.rm <- list("sp.points", obs[idxs, ], pch=4, col="black",
                     cex=0.5, which=1)

  plot1 <- spplot(p.grd, zcol=c("WL_elev.pred.new", "WL_elev.pred"),
                  names.attr=c("Prediction with site removal", "Prediction"),
                  col.regions=terrain.colors(100), cuts=99,
                  colorkey=list(width=1), sp.layout=list(obs.pts, obs.pts.rm),
                  contour=TRUE, labels=FALSE, pretty=TRUE, col="gray",
                  par.settings=list(strip.background=list(col="#F0F0F0")))

  plot2 <- spplot(p.grd, zcol=c("WL_elev.se.new", "WL_elev.se"),
                  names.attr=c("Prediction error with site removal",
                               "Prediction error"),
                  col.regions=heat.colors(100), cuts=99,
                  colorkey=list(width=1), sp.layout=list(obs.pts, obs.pts.rm),
                  col="gray",
                  par.settings=list(strip.background=list(col="#F0F0F0")))

  x11()
  print(plot1)
  x11()
  print(plot2)

  invisible(list(obs=obs, obs.rm=obs.rm, obj.values=obj.values))
}
