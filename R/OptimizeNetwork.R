OptimizeNetwork <- function(pts, grd, ply, network, nsites, vg.model,
                            formula, nmax=Inf, xlim=bbox(grd)[1, ],
                            ylim=bbox(grd)[2, ], grd.fact=1, niters=200,
                            pop.size=200, obj.weights=c(1, 1, 1, 1),
                            rtn.kr=TRUE, mutation.chance=NA, elitism=NA,
                            zero.to.one.ratio=NA, suggestions=NULL) {

  # Additional functions (subroutines)

  # Calculate objective functions
  CalcObj <- function(idxs) {
    newdata <- rbind(grd.pts, pts[idxs, "var2"])

    # Perform kriging
    kr <- gstat::krige(formula=formula, locations=pts[-idxs, ], newdata=newdata,
                       model=vg.model, nmax=nmax, debug.level=0)

    pred <- kr[(ngrd.pts + 1):length(kr), ]$var1.pred
    se <- sqrt(abs(kr[1:ngrd.pts, ]$var1.var))

    obj.1 <- mean(se)
    obj.2 <- sqrt(sum((pred - pts$var1[idxs])^2) / nsites)
    obj.3 <- mean(pts$sd[idxs])
    obj.4 <- mean(pts$acy[-idxs])

    obj.1 <- obj.1 * obj.weights[1]
    obj.2 <- obj.2 * obj.weights[2]
    obj.3 <- obj.3 * obj.weights[3]
    obj.4 <- obj.4 * obj.weights[4]
    c(obj.1, obj.2, obj.3, obj.4)
  }

  # Evaluate objective function in GA
  EvalFun <- function(string) {
    if (sum(string) != nsites) {
      ncalls.penalty <<- ncalls.penalty + 1L
      return(1e15)
    }
    idxs <- which(as.logical(string))
    objs <- CalcObj(idxs)
    sum(objs, na.rm=TRUE)
  }

  # Get best GA solution
  GetBestSolution <- function(rbga.results) {
    filter <- rbga.results$evaluations == min(rbga.results$evaluations)
    best.obj.count <- sum(rep(1, rbga.results$popSize)[filter])
    if (best.obj.count > 1)
      best.solution <- rbga.results$population[filter, ][1, ]
    else
      best.solution <- rbga.results$population[filter, ]
    best.solution
  }

  # Monitor progress at end of each GA iteration
  MonitorFun <- function(obj) {
    best.solution <- GetBestSolution(obj)
    idxs <- which(as.logical(best.solution))
    objs <- CalcObj(idxs)
    obj.values[obj$iter, ] <<- c(objs, sum(objs, na.rm=TRUE))
    PlotObjValues()
  }

  # Plot status of objectives
  PlotObjValues <- function() {
    obj.values <- na.omit(obj.values)
    n <- nrow(obj.values)
    m <- ncol(obj.values)
    x <- 1:n
    xlim <- c(0, n + 1)
    for (i in 1:m) {
      y <- obj.values[, i]
      ylim <- range(pretty(extendrange(y)))
      plot(x, y, xlim=xlim, ylim=ylim, xaxs="i", yaxs="i",
           type="n", xaxt="n", tcl=tcl, ylab=labs[i])
      axis(3, tcl=tcl, labels=FALSE)
      axis(4, tcl=tcl, labels=FALSE)
      axis(1, tcl=tcl, labels=(i == m))
      points(x, y, type="o", pch=21, col=pal[i], bg=pal[i])
      txt <- paste(format(y[n]), "    ")
      mtext(txt, side=3, line=-2, adj=1, cex=0.75)
      if (i < m & is.weighted[i]) {
        txt <- paste("Weighted by", format(obj.weights[i]))
        mtext(txt, side=4, line=0.5, cex=0.75, col="dark gray")
      }
    }
  }


  # Main program

  # Transform points and polygon projection and datum
  crs <- CRS(proj4string(grd))
  pts <- spTransform(pts, crs)
  if (!missing(ply))
    ply <- spTransform(ply, crs)

  # Save original vector of site numbers
  orig.siteno <- pts$siteno

  # Bring network sites to initial rows of data table
  if (missing(network)) {
    nsites.in.network <- length(pts)
  } else {
    is.net <- pts$net == network
    nsites.in.network <- sum(is.net)
    if (nsites.in.network == 0)
      stop("network not in observation table")
    pts <- rbind(pts[is.net, ], pts[!is.net, ])
  }

  # Initialize chromosome
  if (is.null(suggestions)) {
    idxs <- sample(1:nsites.in.network, nsites, replace=FALSE)
    suggestions <- rep(0L, nsites.in.network)
    suggestions[idxs] <- 1L
    suggestions <- t(suggestions)
  }
  
  # Initialize number of calls to penalty function
  ncalls.penalty <- 0L
  
  # Set default for zero to one ratio
  if(is.na(zero.to.one.ratio))
    zero.to.one.ratio <- floor(nsites.in.network / nsites) - 1
  
  # Crop grid to axis limits
  x <- coordinates(grd)[, 1]
  y <- coordinates(grd)[, 2]
  is.in.lim <- x >= xlim[1] & x <= xlim[2] & y >= ylim[1] & y <= ylim[2]
  grd <- grd[is.in.lim, ] # NA's values outside of limits

  # Crop grid to polygon
  if (!missing(ply))
    grd$var2 <- grd$var2 * overlay(grd, ply)

  # Reduce grid resolution
  # TODO(jfisher): prevent raster() from removing all but first field
  if (grd.fact > 1)
    grd.mod <- as(aggregate(raster(grd), fact=grd.fact, fun=mean, expand=TRUE,
                            na.rm=TRUE), 'SpatialGridDataFrame')
  else
    grd.mod <- grd

  # Convert grid to data frame
  grd.pts <- as(grd.mod, "SpatialPointsDataFrame")
  coordnames(grd.pts) <- c("x", "y")
  ngrd.pts <- length(grd.pts)

  # Initialize matrix of objective values
  nobjs <- length(obj.weights)
  obj.values <- matrix(NA, nrow=niters, ncol=nobjs + 1,
                       dimnames=list(1:niters, c(paste("obj", 1:nobjs, sep="."),
                                                 "total")))

  # Check validity of objective weights
   if (!inherits(obj.weights, c("numeric", "integer")) |
       length(obj.weights) != nobjs)
    stop("problem with objective weights")
   is.weighted <- obj.weights != 1

  # Set plot attributes
  windows(width=8, height=(nobjs + 1) * 2)
  op <- par(mfrow=c(nobjs + 1, 1), oma=c(3, 2, 2, 2), mar=c(1, 4, 0, 2))
  tcl <- 0.50 / (6 * par("csi"))
  pal <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
  labs <- NULL
  labs[1] <- "Mean standard error"
  labs[2] <- "Root-mean-square error"
  labs[3] <- "Mean standard deviaiton"
  labs[4] <- "Mean measurement error"
  labs[5] <- "Fitness score"

  # Run GA
  elapsed.time <- system.time({
    rbga.ans <- rbga.bin(size=nsites.in.network,
                         popSize=pop.size,
                         iters=niters,
                         mutationChance=mutation.chance,
                         elitism=elitism,
                         zeroToOneRatio=zero.to.one.ratio,
                         monitorFunc=MonitorFun,
                         evalFunc=EvalFun,
                         showSettings=FALSE,
                         verbose=FALSE,
                         suggestions=suggestions)
  })
  summary.rbga(rbga.ans, echo=TRUE)
  best.solution <- GetBestSolution(rbga.ans)
  rm.idxs <- which(as.logical(best.solution)) # index from modified points
  rm.pts <- pts[rm.idxs, ]
  is.rm.idx <- orig.siteno %in% rm.pts$siteno # index from unmodified points

  # Reset graphics parameters
  par(op)

  # Final kriging
  if (rtn.kr) {
    kr <- krige(formula=formula, locations=pts[-rm.idxs, ], newdata=grd,
                model=vg.model, debug.level=0)
    kr$var1.se <- sqrt(kr$var1.var) # standard error
  } else {
    kr <- NULL
  }

  # Report elapsed time for running optimization
  elapsed.time <- as.numeric(elapsed.time['elapsed']) / 3600
  cat("\nElapsed time:", format(elapsed.time), "hours\n")

  # Determine and report how many times the final solution was repeated
  ans.rep <- 0L
  for (i in niters:1) {
    if (!identical(obj.values[i, ], obj.values[niters, ]))
      break
    ans.rep <-  ans.rep + 1L
  }
  cat("\nNumber of times final solution was repeated:", ans.rep, "\n")
  
  # Report number of calls to penalty function
  cat("\nNumber of calls to penalty function:", format(ncalls.penalty), "\n")

  # Return optimized sites to remove
  invisible(list(rm.pts=rm.pts, is.rm.idx=is.rm.idx, obj.values=obj.values,
                 ans.rep=ans.rep, elapsed.time=elapsed.time, 
                 ncalls.penalty=ncalls.penalty, kr=kr,
                 best.solution=t(best.solution), rbga.ans=rbga.ans))
}
