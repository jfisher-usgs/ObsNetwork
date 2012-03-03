RunGA <- function(obs, network, grd, nsites, vg.model, formula, nmax=Inf,
                  niters=200, pop.size=200, obj.weights=c(1, 1, 1, 1)) {

  # Additional functions (subroutines)

  # Calculate objective functions
  CalcObj <- function(idxs) {
    newdata <- rbind(grd.pts, obs[idxs, "var2"])

    # Perform kriging
    kr <- krige(formula=formula, locations=obs[-idxs, ], newdata=newdata,
                model=vg.model, nmax=nmax, debug.level=0)

    pred <- kr[(ngrd.pts + 1):length(kr), ]$var1.pred
    se <- sqrt(abs(kr[1:ngrd.pts, ]$var1.var))

    obj.1 <- mean(se)
    obj.2 <- sqrt(sum((pred - obs$var1[idxs])^2) / nsites)
    obj.3 <- mean(obs$sd[idxs])
    obj.4 <- mean(obs$acy[-idxs])

    obj.1 <- obj.1 * obj.weights[1]
    obj.2 <- obj.2 * obj.weights[2]
    obj.3 <- obj.3 * obj.weights[3]
    obj.4 <- obj.4 * obj.weights[4]

    c(obj.1, obj.2, obj.3, obj.4)
  }

  # Evaluate objective function in GA
  EvalFun <- function(string) {
    idxs <- round(string, digits=0)
    if (length(unique(idxs)) < length(idxs))
      return(1e15)
    objs <- CalcObj(idxs)
    sum(objs, na.rm=TRUE)
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
    obj.values[obj$iter, ] <<- c(objs, sum(objs, na.rm=TRUE))
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

  if (missing(network)) {
    nsites.in.network <- length(obs)
  } else {
    is.net <- obs$net == network
    nsites.in.network <- sum(is.net)
    if (nsites.in.network == 0)
      stop("network not in observation table")
    obs <- rbind(obs[is.net, ], obs[!is.net, ])
  }

  # Convert grid to data frame
  grd.pts <- as(grd, "SpatialPointsDataFrame")
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
   is.weights <- any(obj.weights != 1)

  # Set plot attributes
  windows(width=8, height=(nobjs + 1) * 2)
  op <- par(mfrow=c(nobjs + 1, 1), oma=c(3, 2, 2, 2), mar=c(1, 6, 0, 2))
  tcl <- 0.50 / (6 * par("csi"))
  pal <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
  labs <- NULL
  labs[1] <- paste("Mean prediction error,",
                   "from the application of Kriging", sep="\n")
  labs[2] <- paste("Root-mean-square error, diff.",
                   "between predicted and measured",
                   "values at removed sites", sep="\n")
  labs[3] <- paste("Mean standard deviaiton,",
                   "variability of measurement",
                   "over time at removed sites", sep="\n")
  labs[4] <- "Mean measurement error"
  labs[5] <- "Solution to objective function"

  # Run GA
  elapsed.time <- system.time({
    rbga.ans <- rbga(stringMin=rep(1, nsites),
                     stringMax=rep(nsites.in.network, nsites),
                     popSize=pop.size, iters=niters, verbose=TRUE,
                     monitorFunc=MonitorFun, evalFunc=EvalFun)
  })
  summary.rbga(rbga.ans, echo=TRUE)
  rm.idxs <- GetIdxsForBestSolution(rbga.ans)
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

  # Perform final kriging
  kr <- krige(formula=formula, locations=obs[-rm.idxs, ], newdata=grd,
              model=vg.model, nmax=nmax, debug.level=0)
  kr$var1.se <- sqrt(kr$var1.var)

  # Return optimized site indexes to remove
  invisible(list(rm.obs=rm.obs, obj.values=obj.values,
                 obj.weights=obj.weights, elapsed.time=elapsed.time,
                 niters.solution=niters.solution, rbga.ans=rbga.ans, kr=kr))
}
