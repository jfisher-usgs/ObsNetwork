PlotKriging <- function(obs, v.fit, grd, rm.idxs, at.pred, at.se,
                        debug.level=0) {

  # Should site indexes be removed?
  is.rm.idxs <- !missing(rm.idxs) &&
                inherits(rm.idxs, c("numeric", "integer"))

  # Define point symbols
  if (is.rm.idxs) {
    pts.o <- list("sp.points", obs[-rm.idxs, ], pch=21, cex=0.5,
                  col="black", fill="white")
    pts.x <- list("sp.points", obs[rm.idxs, ], pch=4, cex=0.5,
                  col="black", lwd=2)
    sp.layout <- list(pts.o, pts.x)
  } else {
    pts.o <- list("sp.points", obs, pch=21, cex=0.5, col="black", fill="white")
    sp.layout <- list(pts.o)
  }

  # Delete observations associated with removed site indexes
  if (is.rm.idxs)
    obs <- obs[-rm.idxs, ]

  # Run geostatistical prediction using ordinary kriging
  obs.krig <- krige(as.formula("observation~1"), obs, grd, model=v.fit,
                    debug.level=debug.level)
  obs.krig[["observation.pred"]] <- obs.krig[["var1.pred"]]

  # Calculate standardard error from variance error
  obs.krig[["observation.se"]] <- sqrt(obs.krig[["var1.var"]])

  # Determine axis limits
  bbox.grd <- bbox(obs.krig)
  xlim <- range(pretty(bbox.grd[1,], n=7))
  ylim <- range(pretty(bbox.grd[2,], n=7))

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
  scales <- list(draw=TRUE, y=list(rot=90, tck=-1), x=list(tck=-1))

  # Set axis breakpoints
  if (missing(at.pred))
    at.pred <- pretty(obs.krig$observation.pred, n=12)
  if (missing(at.se))
    at.se <- pretty(obs.krig$observation.se, n=12)

  # Set color palettes
  cols1 <- rev(heat_hcl(length(at.pred) + 1,
                        h=c(234, 66), c=c(79, 25), l=c(56, 85),
                        power=c(0.2, 1.1)))
  cols2 <- rev(heat_hcl(length(at.se) + 1,
                        h=c(0, 90), c=c(80, 30), l=c(30, 90),
                        power=c(0.2, 2.0)))

  # Add spatial scale legend
  lng <- xlim[1] + diff(xlim) * 0.02
  lat <- ylim[1] + diff(ylim) * 0.02
  dx.1 <- 1
  dm.1 <- spDistsN1(cbind(dx.1, lat), c(0, lat), longlat=TRUE)
  xseq <- pretty(bbox.grd[1,])[1:2]
  dm <- spDistsN1(cbind(xseq[2], lat), c(xseq[1], lat), longlat=TRUE)
  dm.2 <- pretty(c(0, dm), n=1)[2]
  dx.2 <- dm.2 / dm.1
  leg.scale <- list("SpatialPolygonsRescale", layout.scale.bar(),
                    offset=c(lng, lat), scale=dx.2,
                    fill=c("transparent", "black"))
  scale.txt1 <- list("sp.text", loc=c(lng, lat + dx.2 * 0.1),
                     txt="0", cex=0.75)
  scale.txt2 <- list("sp.text", loc=c(lng + dx.2, lat + dx.2 * 0.1),
                     txt=paste(dm.2, "km"), cex=0.75)
  n <- length(sp.layout)
  sp.layout[[n + 1]] <- leg.scale
  sp.layout[[n + 2]] <- scale.txt1
  sp.layout[[n + 3]] <- scale.txt2

  # Add north arrow legend
  lng <- xlim[2] - diff(xlim) * 0.05
  leg.arrow <- list("SpatialPolygonsRescale", layout.north.arrow(),
                    offset = c(lng, lat), scale=diff(ylim) * 0.1)
  sp.layout[[n + 4]] <- leg.arrow

  # Draw plots
  plot1 <- spplot(obs.krig, zcol="observation.pred",
                  scales=scales, xlim=xlim, ylim=ylim,
                  col.regions=cols1, at=at.pred, main=main1,
                  colorkey=colorkey, sp.layout=sp.layout,
                  contour=TRUE, labels=FALSE,
                  pretty=TRUE, col="gray")
  x11()
  print(plot1)
  plot2 <- spplot(obs.krig, zcol="observation.se",
                  scales=scales, xlim=xlim, ylim=ylim,
                  col.regions=cols2, at=at.se, main=main2,
                  colorkey=colorkey, sp.layout=sp.layout,
                  contour=TRUE, labels=FALSE,
                  pretty=TRUE, col="gray")
  x11()
  print(plot2)
}
