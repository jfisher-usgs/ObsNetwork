RunGA <- function(obs, v.fit, grd, nsites=10, niters=200, pop.size=200) {

  # Additional functions (subroutines)

  # Calculate objective functions
  CalcObj <- function(idxs) {
    newdata <- SpatialPoints(rbind(coordinates(grd), coordinates(obs[idxs, ])),
                             proj4string=CRS(as.character(projargs)))
    est <- krige(as.formula("observation~1"), obs[-idxs, ], newdata,
                 model=v.fit, debug.level=0)
    est.se <- sqrt(abs(est[1:ngrd, ]$var1.var))
    est.pred <- est[(ngrd + 1):length(est), ]$var1.pred
    obj.1 <- mean(est.se)
    obj.2 <- sqrt(sum((est.pred - obs$observation[idxs])^2))
    obj.3 <- mean(obs$accuracy[-idxs])
    c(obj.1, obj.2, obj.3)
  }

  # Evaluate objective function in GA
  EvalFun <- function(string) {
    idxs <- round(string, digits=0)
    if (length(unique(idxs)) < length(idxs))
      return(1e15)
    objs <- CalcObj(idxs)
    sum(objs)
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

  # Monitor progress at end of each iteration in GA
  MonitorFun <- function(obj) {
    idxs <- GetIdxsForBestSolution(obj)
    objs <- CalcObj(idxs)
    obj.values[obj$iter, ] <<- c(objs, sum(objs))
    PlotObjValues()
  }

  # Plot status of objective values
  PlotObjValues <- function() {
    obj.values <- na.omit(obj.values)
    x <- 1:nrow(obj.values)
    xlim <- c(0, max(x) + 1)
    for (i in 1:(nobjs + 1)) {
      y <- obj.values[, i]
      ylim <- range(pretty(extendrange(y)))
      plot(x, y, xlim=xlim, ylim=ylim, xaxs="i", yaxs="i",
           type="o", pch=22, col=pal[i], bg=pal[i],
           xaxt="n", ylab=ylabs[i], tcl=tcl)
      axis(3, tcl=tcl, labels=FALSE)
      axis(4, tcl=tcl, labels=FALSE)
      axis(1, tcl=tcl, labels=(i == nobjs + 1))
    }
  }


  # Main program

  # Calculate length of grid
  ngrd <- length(grd)

  # Set default projection
  projargs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  # Initialize matrix of objective values
  nobjs <- 3
  obj.values <- matrix(NA, nrow=niters, ncol=nobjs + 1,
                       dimnames=list(1:niters, c(paste("obj", 1:nobjs, sep="."),
                                                 "total")))

  # Intialize plot and its common variables
  windows(width=8, height=(nobjs + 1) * 2)
  op <- par(mfrow=c(nobjs + 1, 1), oma=c(3, 2, 2, 2), mar=c(1, 5, 0, 2))
  tcl <- 0.50 / (6 * par("csi"))
  ylabs <- c("Prediction error", "Observation error", "Measurement error",
             "Total error")
  pal <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")

  # Run GA
  elapsed.time <- system.time({
    ans <- rbga(stringMin=rep(1, nsites), stringMax=rep(length(obs), nsites),
                popSize=pop.size, iters=niters, verbose=TRUE,
                monitorFunc=MonitorFun, evalFunc=EvalFun)
  })
  summary.rbga(ans, echo=TRUE)
  rm.idxs <- GetIdxsForBestSolution(ans)
  rm.obs <- obs[rm.idxs, ]

  # Reset graphics parameters
  par(op)

  # Report elapsed time for running optimization
  elapsed.time <- as.numeric(elapsed.time['elapsed']) / 3600
  txt <- paste("\nTime required for optimal solution:",
               format(elapsed.time), "hours\n\n")
  cat(txt)

  # Return optimized site indexes to remove
  invisible(list(rm.idxs=rm.idxs, rm.obs=rm.obs, obj.values=obj.values,
                 elapsed.time=elapsed.time, ans=ans))
}
