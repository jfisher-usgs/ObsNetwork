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
    obj.2 <- sum((est.pred - obs$observation[idxs])^2)
    c(obj.1, obj.2)
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
    n <- obj$iter
    idxs <- GetIdxsForBestSolution(obj)
    objs <- CalcObj(idxs)
    obj.values[n, ] <<- c(objs, sum(objs))

    x <- 1:n
    y1 <- obj.values[x, "obj.1"]
    y2 <- obj.values[x, "obj.2"]

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


  # Main program

  # Calculate length of grid
  ngrd <- length(grd)

  # Set default projection
  projargs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  # Initialize matrix of objective values
  obj.values <- matrix(NA, nrow=niters, ncol=3,
                       dimnames=list(1:niters, c("obj.1", "obj.2", "total")))

  # Intialize plot for monitoring function
  x11()
  op <- par(mar=c(5, 4, 3, 4))

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
