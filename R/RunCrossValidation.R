RunCrossValidation <- function(formula, pts, model, nfold=nrow(pts), ..., 
                               projargs=proj4string(pts)) {

  # Transform projection and datum
  crs <- CRS(projargs)
  pts <- spTransform(pts, crs)
  
  # Create object of class gstat
  obj <- gstat::gstat(formula=formula, data=pts, model=model)
  
  # Cross validation
  cv <- gstat::gstat.cv(obj, verbose=FALSE, ...)
  proj4string(cv) <- pts@proj4string

  # Mean error
  me <- mean(cv$residual)
  # Mean square prediction error
  mspe <- mean(cv$residual^2)
  # Mean square normalized error
  msne <- mean(cv$zscore^2)
  # Correlation observed and predicted
  cor.op <- cor(cv$observed, cv$observed - cv$residual)
  # Correlation predicted and residual
  cor.pr <- cor(cv$observed - cv$residual, cv$residual)

  # Plot observed versus predicted
  x <- cv$observed
  y <- cv$var1.pred
  lim  <- range(pretty(extendrange(c(x, y))))
  xlim <- range(pretty(extendrange(x)))
  ylim <- range(pretty(extendrange(y)))
  x11()
  tcl <- 0.50 / (6 * par("csi"))
  plot(x, y, xaxs="i", yaxs="i", xlim=xlim, ylim=ylim, asp=1, tcl=tcl,
       xlab="Observed value", ylab="Predicted value")
  lines(x=lim, y=lim, col="blue")
  res <- lm(y ~ x)
  abline(res)

  # Plot predicted versus residual
  x <- cv$var1.pred
  y <- cv$residual
  xlim <- range(pretty(extendrange(x)))
  ylim <- range(pretty(extendrange(y)))
  x11()
  plot(x, y, xaxs="i", yaxs="i", xlim=xlim, ylim=ylim, tcl=tcl,
       xlab="Predicted value", ylab="Residual")
  lines(x=xlim, y=rep(me, 2), col="red")
  txt <- paste("Mean error", format(me), sep=" = ")
  mtext(txt, side=3, line=0, adj=1, cex=0.75)

  list(cv=cv, me=me, mspe=mspe, msne=msne, cor.op=cor.op, cor.pr=cor.pr)
}
