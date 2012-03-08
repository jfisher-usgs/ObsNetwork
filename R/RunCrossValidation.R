RunCrossValidation <- function(formula, obs, vg.model, nmax=Inf,
                               nfold=nrow(obs)) {

  # Cross validation
  cv <- gstat::krige.cv(formula, obs, model=vg.model, nmax=nmax, nfold=nfold)
  proj4string(cv) <- obs@proj4string

  # Mean error, ideally 0
  me <- mean(cv$residual)
  # MSPE, ideally small
  mspe <- mean(cv$residual^2)
  # Mean square normalized error, ideally close to 1
  msne <- mean(cv$zscore^2)
  # Correlation observed and predicted, ideally 1
  cor.obs.pred <- cor(cv$observed, cv$observed - cv$residual)
  # Correlation predicted and residual, ideally 0
  cor.pred.res <- cor(cv$observed - cv$residual, cv$residual)

  # Plot observed versus predicted
  x <- cv$observed
  y <- cv$var1.pred
  lim <- range(pretty(extendrange(c(x, y))))
  xlim <- range(pretty(extendrange(x)))
  ylim <- range(pretty(extendrange(y)))
  x11()
  tcl <- 0.50 / (6 * par("csi"))
  plot(x, y, xaxs="i", yaxs="i", xlim=xlim, ylim=ylim, asp=1, tcl=tcl,
       xlab="Observed value", ylab="Predicted value")
  lines(x=lim, y=lim, col="blue")

  # Plot predicted versus residual
  x <- cv$var1.pred
  y <- cv$residual
  xlim <- range(pretty(extendrange(x)))
  ylim <- range(pretty(extendrange(y)))
  x11()
  plot(x, y, xaxs="i", yaxs="i", xlim=xlim, ylim=ylim, tcl=tcl,
       xlab="Predicted value", ylab="Residual")
  ave.err <- mean(y)
  lines(x=xlim, y=rep(ave.err, 2), col="red")
  txt <- paste("Mean residual", format(ave.err), sep=" = ")
  mtext(txt, side=3, line=0, adj=1, cex=0.75)

  list(cv=cv, me=me, mspe=mspe, msne=msne, cor.obs.pred=cor.obs.pred,
       cor.pred.res=cor.pred.res)
}
