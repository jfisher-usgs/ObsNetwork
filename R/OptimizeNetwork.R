OptimizeNetwork <- function(pts, grd, ply, network.nm, nsites, model, formula,
                            nmax=Inf, xlim=bbox(grd)[1, ], ylim=bbox(grd)[2, ],
                            grd.fact=1, obj.weights=c(1, 1, 1, 1),
                            penalty.constant=1E6, maxabort=10, popSize=50,
                            pcrossover=0.8, pmutation=0.1,
                            elitism=base::max(1, round(popSize * 0.05)),
                            maxiter=100, run=maxiter, suggestions=NULL, ...) {

  # Additional functions (subroutines)

  # Mutate
  Mutate <- function (object, parent, ...) {
    parent <- as.vector(object@population[parent, ])
    all.idxs <- 1:nsites.in.network
    idxs <- DecodeChromosome(parent)
    i <- sample(1:nsites, size=1)
    idxs[i] <- sample(all.idxs[!all.idxs %in% idxs], size=1)
    mutate <- EncodeChromosome(idxs)
    return(mutate)
  }

  # Crossover
  Crossover <- function (object, parents, ...) {
    fitness <- object@fitness[parents]
    encoded.parents <- object@population[parents, , drop=FALSE]
    decoded.parents <- t(apply(encoded.parents, 1, DecodeChromosome))
    n <- ncol(decoded.parents)
    decoded.children <- matrix(NA, nrow=2, ncol=n)
    fitness.children <- rep(NA, 2)
    i <- 1L
    while (i <= maxabort) {
      crossover.point <- sample(0:n, size=1)
      if (crossover.point == 0) {
          decoded.children[1:2, ] <- decoded.parents[2:1, ]
          fitness.children[1:2] <- fitness[2:1]
      } else if (crossover.point == n) {
          decoded.children <- decoded.parents
          fitness.children <- fitness
      } else {
          decoded.children[1, ] <- c(decoded.parents[1, 1:crossover.point],
                                     decoded.parents[2, (crossover.point + 1):n])
          decoded.children[2, ] <- c(decoded.parents[2, 1:crossover.point],
                                     decoded.parents[1, (crossover.point + 1):n])
          fitness.children <- NA
      }
      is.unique <- !any(apply(decoded.children, 1,
                              function(j) any(duplicated(j))))
      if (is.unique)
        break
      else
        i <- i + 1L
    }
    children <- t(apply(decoded.children, 1, function(i) EncodeChromosome(i)))
    list(children=children, fitness=fitness.children)
  }

  # Calculate objective functions
  CalcObjs <- function(idxs) {

    # Remove selected sites
    locations <- pts[-idxs, ]

    # Perform point kriging to predict values at removed site locations
    kr.pts <- krige(formula=formula, locations=locations, newdata=pts[idxs, ],
                    model=model, nmax=nmax, debug.level=0)
    kr.pts.pred <- kr.pts$var1.pred

    # Perform point kriging to predict standard errors in modified grid
    kr.grd <- krige(formula=formula, locations=locations, newdata=grd.mod,
                    model=model, nmax=nmax, debug.level=0)
    kr.grd.se <- sqrt(kr.grd$var1.var)

    # Solve individual objective functions
    objs <- NULL
    objs[1] <- mean(kr.grd.se, na.rm=TRUE)
    objs[2] <- sqrt(sum((kr.pts.pred - pts$var1[idxs])^2) / nsites)
    objs[3] <- mean(pts$var1.sd[idxs])
    objs[4] <- mean(pts$var1.acy[-idxs])
    objs * obj.weights
  }

  # Calculate fitness
  CalcFitness <- function(string) {
    idxs <- DecodeChromosome(string)
    npenalties <- sum(duplicated(idxs))
    if (npenalties > 0) {
      ncalls.penalty.iter <<- ncalls.penalty.iter + 1L
      return(-penalty.constant * npenalties)
    }
    objs <- CalcObjs(idxs)
    r <- vapply(1:4, function(i) c(min(obj.space[i, 1], objs[i], na.rm=TRUE),
                                   max(obj.space[i, 2], objs[i], na.rm=TRUE)),
                                   rep(0, 2))
    obj.space[, ] <<- t(r)
    fitness.score <- -sum(objs, na.rm=TRUE)
    fitness.score
  }

  # Monitor progress at end of each GA iteration
  MonitorGA <- function(obj) {
    string <- obj@population[which(obj@fitness == max(obj@fitness))[1L], ]
    idxs <- DecodeChromosome(string)
    objectives <- CalcObjs(idxs)
    obj.values[obj@iter, ] <<- c(objectives, sum(objectives, na.rm=TRUE))
    ncalls.penalty[obj@iter] <<- ncalls.penalty.iter
    ncalls.penalty.iter <<- 0L
    diff.time[obj@iter] <<- difftime(Sys.time(), start.time, units="hours")
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
      mtext(paste(format(y[n]), "    "), side=3, line=-2, adj=1, cex=0.75)
      if (i < m & is.weighted[i]) {
        txt <- paste("Weighted by", format(obj.weights[i]))
        mtext(txt, side=4, line=0.5, cex=0.75, col="dark gray")
      }
    }
  }

  # Find number of k-combinations
  FindNumCombinations <- function(n, k) {
    nc <- suppressWarnings(factorial(n) / (factorial(k) * factorial(n - k)))
    if (is.na(nc))
      return(Inf)
    nc
  }

  # Build integer chromosomes from random sample of sites
  GetIntChromosomes <- function(m, n, k) {
    t(vapply(1:m, function(i) sort(sample(1:n, k, replace=FALSE)), rep(0, k)))
  }

  # Encode chromosome
  EncodeChromosome <- function(int.chr) {
    bin.chr <- NULL
    for (i in int.chr) {
      gry <- GA::binary2gray(GA::decimal2binary(i, length.bin.string))
      bin.chr <- c(bin.chr, gry)
    }
    bin.chr
  }

  # Decode chromosome
  DecodeChromosome <- function(string) {
    sapply(seq(1, nsites * length.bin.string, by=length.bin.string),
           function(i) {
             gry <- string[i:(i + length.bin.string - 1L)]
             GA::binary2decimal(GA::gray2binary(gry))
           })
  }


  # Main program

  # Save call with specified arguments
  call <- match.call()

  # Check for required variables in spatial points data frame
  required.vars <- c("site.no", "var1", "var1.acy", "var1.sd")
  if (!all(required.vars %in% names(pts)))
    stop("missing required variable(s) in spatial points data frame")

  # Check validity of objective weights
  if (!inherits(obj.weights, c("numeric", "integer")))
    stop("problem with objective weights")
  is.weighted <- obj.weights != 1

  # Transform points and polygon projection and datum
  crs <- CRS(proj4string(grd))
  pts <- spTransform(pts, crs)
  if (!missing(ply))
    ply <- spTransform(ply, crs)

  # Save original vector of site numbers
  orig.site.no <- pts$site.no

  # Identify sites in observation network(s)
  if ("network.nm" %in% names(pts) & !missing(network.nm)) {
    is.net <- rep(FALSE, length(pts))
    for (i in seq(along=network.nm)) {
      chk <- sapply(strsplit(pts$network.nm, ","),
                    function (j) network.nm[i] %in% gsub("^\\s+|\\s+$", "", j))
      is.net <- is.net | chk
    }
  } else {
    is.net <- rep(TRUE, length(pts))
  }
  pts <- pts[is.net, ]
  nsites.in.network <- length(pts)
  if (nsites.in.network == 0)
    stop("no sites in selected observation network")

  # Crop grid to axis limits
  x <- coordinates(grd)[, 1]
  y <- coordinates(grd)[, 2]
  is.in.lim <- x >= xlim[1] & x <= xlim[2] & y >= ylim[1] & y <= ylim[2]
  grd <- grd[is.in.lim, ]

  # Crop grid to polygon
  if (!missing(ply))
    grd[[1]] <- grd[[1]] * over(as(grd, "SpatialPoints"),
                                as(ply, "SpatialPolygons"))

  # Reduce grid resolution
  # TODO(jfisher): prevent raster() from removing all but first field
  if (grd.fact > 1)
    grd.mod <- as(aggregate(raster(grd), fact=grd.fact, fun=mean, expand=TRUE,
                            na.rm=TRUE), "SpatialGridDataFrame")
  else
    grd.mod <- grd
  coordnames(grd.mod) <- c("x", "y")

  # Initialize number of calls to penalty function
  ncalls.penalty <- NULL
  ncalls.penalty.iter <- 0L

  # Initialize matrix of objective values
  nobjs <- length(obj.weights)
  obj.names <- c(paste("objective", 1:nobjs, sep="-"), "fitness")
  obj.values <- matrix(NA, nrow=maxiter, ncol=nobjs + 1L,
                       dimnames=list(1:maxiter, obj.names))

  # Set plot attributes
  dev.new(width=8, height=(nobjs + 1) * 2)
  op <- par(mfrow=c(nobjs + 1, 1), oma=c(3, 2, 2, 2), mar=c(1, 4, 0, 2))
  tcl <- 0.50 / (6 * par("csi"))
  pal <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
  labs <- NULL
  labs[1] <- "Mean standard error"
  labs[2] <- "Root-mean-square error"
  labs[3] <- "Mean standard deviaiton"
  labs[4] <- "Mean measurement error"
  labs[5] <- "Fitness score"

  # Initialize extent of solution space
  obj.space <- matrix(NA, nrow=nobjs, ncol=2, dimnames=list(obj.names[1:nobjs],
                                                            c("Min", "Max")))

  # Determine maximum length of binary string
  length.bin.string <- length(GA::decimal2binary(nsites.in.network))

  # Initialize population
  if (is.null(suggestions)) {
    int.pop <- GetIntChromosomes(popSize, nsites.in.network, nsites)

    # Remove duplicates
    ncombinations <- FindNumCombinations(nsites.in.network, nsites)
    if (popSize < ncombinations) {
      dups <- which(duplicated(t(apply(int.pop, 1, sort))))
      ndups <- length(dups)
      iter <- 1L
      while (ndups > 0 & iter < 1000L) {
        int.pop[dups, ] <- GetIntChromosomes(ndups, nsites.in.network, nsites)
        dups <- which(duplicated(t(apply(int.pop, 1, sort))))
        ndups <- length(dups)
        iter <- iter + 1L
      }
    }

    # Convert integer chromosomes to binary chromosomes with Gray encoding
    suggestions <- t(apply(int.pop, 1, function(i) EncodeChromosome(i)))
  }

  # Save system time
  start.time <- Sys.time()
  diff.time <- NULL

  # Run GA
  proc.time <- system.time({
    ga.ans <- GA::ga(type="binary", fitness=CalcFitness,
                     nBits=length.bin.string * nsites,
                     mutation=Mutate, crossover=Crossover,
                     popSize=popSize, pcrossover=pcrossover,
                     pmutation=pmutation, elitism=elitism, monitor=MonitorGA,
                     maxiter=maxiter, run=run, suggestions=suggestions, ...)
  })

  # Decode GA solution
  nsolutions <- nrow(ga.ans@solution)
  if (nsolutions > 1) {
    txt <- paste("The GA found", nsolutions, "solutions with identical",
                 "fitness scores.")
    warning(txt)
  }
  ga.decoded.solution <- DecodeChromosome(ga.ans@solution[1, ])
  rm.idxs <- sort(ga.decoded.solution)

  # Identify removed sites
  pts.rm <- pts[rm.idxs, ]
  is.rm <- orig.site.no %in% pts.rm$site.no

  # Reset graphics parameters
  par(op)

  # Block kriging of reduced network at original grid resolution
  kr <- krige(formula=formula, locations=pts[-rm.idxs, ], newdata=grd,
              model=model, debug.level=0, block=grd@grid@cellsize)
  kr$var1.se <- sqrt(kr$var1.var) # standard error

  # Block kriging of original network at original grid resolution
  kr0 <- krige(formula=formula, locations=pts, newdata=grd, model=model,
               debug.level=0, block=grd@grid@cellsize)
  kr$var1.diff <- kr0$var1.pred - kr$var1.pred

  # Root-mean-square deviation
  var1.diff <- as.numeric(na.omit(kr$var1.diff))
  rmsd <- sqrt(sum(var1.diff^2) / length(var1.diff))

  # Percent local error
  max.error <- var1.diff[which.max(abs(var1.diff))]
  relief <- diff(range(kr0$var1.pred, na.rm=TRUE))
  local.error <- (max.error / relief) * 100

  # Report elapsed time for running optimization
  cat("\nElapsed time:", format(as.numeric(proc.time["elapsed"]) / 3600),
      "hours\n")

  # Report the number of completed iterations
  obj.values <- obj.values[!is.na(obj.values[, "fitness"]), , drop=FALSE]
  niter <- nrow(obj.values)
  cat("\nNumber of completed iterations:", niter, "\n")

  # Report the number of times the final solution was repeated
  nrep.ans <- 0L
  if (niter > 1L) {
    for (i in (niter - 1L):1L) {
      if (!identical(obj.values[i, ], obj.values[niter, ]))
        break
      nrep.ans <-  nrep.ans + 1L
    }
  }
  cat("\nNumber of iterations the best fitness value was repeated:", nrep.ans,
      "\n")

  # Report the number of calls to the penalty function
  ppenalty <- sum(ncalls.penalty) / (popSize * niter) * 100
  cat("\nPercent of chromosomes that invoke the penalty function:",
      format(ppenalty), "\n")

  # Report best fitness score
  fitness <- obj.values[niter, "fitness"]
  cat("\nBest fitness value:", format(fitness), "\n\n")

  # Determine range of objectives in solution space
  col.names <- colnames(obj.space)
  obj.space <- cbind(obj.space, obj.space[, 2] - obj.space[, 1])
  colnames(obj.space) <- c(col.names, "Range")

  # Return GA solution
  invisible(list(call=call, pts.rm=pts.rm, is.net=is.net, is.rm=is.rm,
                 obj.values=obj.values, niter=niter, nrep.ans=nrep.ans,
                 proc.time=proc.time, ncalls.penalty=ncalls.penalty,
                 kr=kr, rmsd=rmsd, local.error=local.error,
                 obj.space=obj.space, ga.ans=ga.ans, start.time=start.time,
                 diff.time=diff.time))
}
