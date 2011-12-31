PlotKriging <- function(obs, v.fit, grd, rm.idxs, at.pred, at.se,
                        debug.level=0) {

  # Are site indexes being removed?
  is.rm.idxs <- !missing(rm.idxs) &&
                inherits(rm.idxs, c("numeric", "integer"))

  # Define point symbols
  if (is.rm.idxs) {
    pts.o <- list("sp.points", obs[-rm.idxs, ], pch=21, cex=0.5,
                  col="black", fill="white")
    pts.x <- list("sp.points", obs[rm.idxs, ], pch=4, cex=0.75,
                  col="black")
    sp.layout <- list(pts.o, pts.x)
  } else {
    pts.o <- list("sp.points", obs, pch=21, cex=0.5, col="black", fill="white")
    sp.layout <- list(pts.o)
  }

  # Run geostatistical prediction using ordinary kriging
  if (is.rm.idxs)
    obs <- obs[-rm.idxs, ]
  obj <- gstat(id="observation", formula=observation~1, data=obs, model=v.fit)
  pred.grd <- predict(obj, newdata=grd, debug.level=debug.level)

  # Axis limits
  bbox.grd <- bbox(pred.grd)
  xlim <- range(pretty(bbox.grd[1,], n=7))
  ylim <- range(pretty(bbox.grd[2,], n=7))

  # Calculate standardard error from variance
  pred.grd$observation.se <- sqrt(abs(pred.grd$observation.var))

  # Set main plot titles
  main1 <- "Prediction"
  main2 <- "Standard error"
  if (is.rm.idxs) {
    n <- length(rm.idxs)
    main1 <- paste("Prediction with", n, "sites removed")
    main2 <- paste("Standard error with", n, "sites removed")
  }

  # Set generic plot arguments
  colorkey <- list(width=1, space="right", labels=list(rot=90))
  scales <- list(draw=TRUE, y=list(rot=90))

  # Color palettes and breakpoints
  if (missing(at.pred))
    at.pred <- pretty(pred.grd$observation.pred, n=12)
  if (missing(at.se))
    at.se <- pretty(pred.grd$observation.se, n=12)

  cols1 <- rev(heat_hcl(length(at.pred) + 1,
                        h=c(130, 246), c=c(66, 0), l=c(60, 86),
                        power=c(0.0, 1.2)))
  cols2 <- rev(heat_hcl(length(at.se) + 1,
                        h=c(0, 90), c=c(80, 30), l=c(30, 90),
                        power=c(0.2, 2.0)))

  # Draw plots
  plot1 <- spplot(pred.grd, zcol="observation.pred",
                  scales=scales, xlim=xlim, ylim=ylim,
                  col.regions=cols1, at=at.pred, main=main1,
                  colorkey=colorkey, sp.layout=sp.layout,
                  contour=TRUE, labels=FALSE,
                  pretty=TRUE, col="gray")
  plot2 <- spplot(pred.grd, zcol="observation.se",
                  scales=scales, xlim=xlim, ylim=ylim,
                  col.regions=cols2, at=at.se, main=main2,
                  colorkey=colorkey, sp.layout=sp.layout,
                  contour=TRUE, labels=FALSE,
                  pretty=TRUE, col="gray")
  x11()
  print(plot1)
  x11()
  print(plot2)
}
