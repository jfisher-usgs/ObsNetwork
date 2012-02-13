RunGA <- function(obs, v.fit, grd, nsites, niters=200, pop.size=200,
                  obj.weights=c(1, 1, 1)) {

  # Additional functions (subroutines)

  # Calculate objective functions
  CalcObj <- function(idxs) {
    newdata <- SpatialPoints(rbind(coordinates(grd), coordinates(obs[idxs, ])),
                             proj4string=CRS(as.character(projargs)))
    est <- krige(as.formula("observation~x+y"), obs[-idxs, ], newdata,
                 model=v.fit, debug.level=0)
    est.se <- sqrt(abs(est[1:ngrd, ]$var1.var))
    est.obs <- est[(ngrd + 1):length(est), ]$var1.pred
    obj.1 <- mean(est.se)
    obj.2 <- sqrt(sum((est.obs - obs$observation[idxs])^2) / length(idxs))
    obj.3 <- mean(obs$accuracy[-idxs])
#   obj.4 <- sum(obs$sd[-idxs])
    obj.1 <- obj.1 * obj.weights[1]
    obj.2 <- obj.2 * obj.weights[2]
    obj.3 <- obj.3 * obj.weights[3]
#   obj.4 <- obj.4 * obj.weights[4]
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
      if (i < m & is.weights) {
        txt <- paste("Values weighted by", format(obj.weights[i]))
        mtext(txt, side=4, line=0.5, cex=0.75, col="dark gray")
      }
    }
  }


  # Main program

  # Calculate length of grid
  ngrd <- length(grd)

  # Set default projection
  projargs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  # Initialize matrix of objective values
  nobjs <- 3L
  obj.values <- matrix(NA, nrow=niters, ncol=nobjs + 1,
                       dimnames=list(1:niters, c(paste("obj", 1:nobjs, sep="."),
                                                 "total")))

  # Check validity of objective weights
   if (!inherits(obj.weights, c("numeric", "integer")) |
       length(obj.weights) != nobjs)
    stop("problem with objective weights")
   is.weights <- any(obj.weights != 1)

  # Intialize plot and its common variables
  windows(width=8, height=(nobjs + 1) * 2)
  op <- par(mfrow=c(nobjs + 1, 1), oma=c(3, 2, 2, 2), mar=c(1, 6, 0, 2))
  tcl <- 0.50 / (6 * par("csi"))
  pal <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")

  # Plot labels
  labs <- NULL
  labs[1] <- paste("Mean estimation error,",
                   "from the application of Kriging", sep="\n")
  labs[2] <- paste("Root-mean-square error,",
                   "difference between estimated",
                   "and measured values", sep="\n")
  labs[3] <- "Mean measurement error"
# labs[4] <- paste("Total standard deviaiton,",
#                  "variability of measurement"
#                  "over time at removed sites", sep="\n")
  labs[4] <- "Solution to objective function\nof optimization problem"

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
               format(elapsed.time), "hours\n")
  cat(txt)

  # Report number of iterations needed to find solution
  count <- 0L
  for (i in (niters - 1):1) {
    if (!identical(obj.values[i, ], obj.values[niters, ]))
      break
    count <- count + 1L
  }
  niters.solution <- niters - count
  txt <- paste("\nNumber of iterations needed to find solution:",
               niters.solution, "\n\n")
  cat(txt)

  # Return optimized site indexes to remove
  invisible(list(rm.idxs=rm.idxs, rm.obs=rm.obs, obj.values=obj.values,
                 obj.weights=obj.weights, elapsed.time=elapsed.time,
                 niters.solution=niters.solution, ans=ans))
}
