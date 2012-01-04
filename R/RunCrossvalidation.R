RunCrossvalidation <- function(obs, v.fit) {

  # Initialize correlation matrix
  mat <- matrix(NA, nrow=nrow(obs), ncol=3,
                dimnames=list(1:nrow(obs), c("obs", "est", "err")))

  # Calculate observation and error at each site by using all field
  # observations apart from the one under investigation. Repeat for
  # each observation.
  for (i in 1:nrow(obs)) {
    est.pnt <- krige(as.formula("observation~1"), obs[-i, ], obs[i, ],
                     model=v.fit, debug.level=0)
    mat[i, 1:2] <- c(obs[i, ]$observation, est.pnt$var1.pred)
  }
  mat[, "err"] <- mat[, "obs"] - mat[, "est"]

  # Plot measured versus estimated
  x <- mat[, "obs"]
  y <- mat[, "est"]
  lim <- range(pretty(c(x, y)))
  x11()
  tcl <- 0.50 / (6 * par("csi"))
  plot(x, y, xlim=lim, ylim=lim, xaxs="i", yaxs="i", asp=1, tcl=tcl,
       xlab="Measured value", ylab="Estimated value")
  lines(x=lim, y=lim, col="blue")

  # Plot estimated versus estimation error
  x <- mat[, "est"]
  y <- mat[, "err"]
  xlim <- range(pretty(x))
  ylim <- range(pretty(y))
  x11()
  plot(x, y, xlim=xlim, ylim=ylim, xaxs="i", yaxs="i", tcl=tcl,
       xlab="Estimated value", ylab="Estimated error")
  ave.err <- mean(y)
  lines(x=xlim, y=rep(ave.err, 2), col="red")
  txt <- paste("Mean estimated error", format(ave.err), sep=" = ")
  mtext(txt, side=3, line=0, adj=1, cex=0.75)

  invisible()
}
