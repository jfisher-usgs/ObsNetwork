PlotKriging <- function(obs, v.fit, grd, rm.idxs, at.pred, at.se,
                        debug.level=0) {

  # Are site indexes being removed?
  is.rm.idxs <- !missing(rm.idxs) &&
                inherits(rm.idxs, c("numeric", "integer"))

  # Define point symbols
  if (is.rm.idxs) {
    pts.o <- list("sp.points", obs[-rm.idxs, ], pch=21, cex=0.5,
                  col="black", fill="white")
    pts.x <- list("sp.points", obs[rm.idxs, ], pch=4, cex=0.75, col="black")
    sp.layout <- list(pts.o, pts.x)
  } else {
    pts.o <- list("sp.points", obs, pch=21, cex=0.5, col="black", fill="white")
    sp.layout <- list(pts.o)
  }

  # Run geostatistical prediction using ordinary kriging
  if (is.rm.idxs)
    obs <- obs[-rm.idxs, ]

  obs.krig <- krige(observation~1, obs, grd, model=v.fit)
  obs.krig[["observation.pred"]] <- obs.krig[["var1.pred"]]

  # Calculate standardard error from variance
  obs.krig[["observation.se"]] <- sqrt(obs.krig[["var1.var"]])

  # Axis limits
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
  scales <- list(draw=TRUE, y=list(rot=90))

  # Color palettes and breakpoints
  if (missing(at.pred))
    at.pred <- pretty(obs.krig$observation.pred, n=12)
  if (missing(at.se))
    at.se <- pretty(obs.krig$observation.se, n=12)

  cols1 <- rev(heat_hcl(length(at.pred) + 1,
                        h=c(234, 66), c=c(79, 25), l=c(56, 85),
                        power=c(0.2, 1.1)))
  cols2 <- rev(heat_hcl(length(at.se) + 1,
                        h=c(0, 90), c=c(80, 30), l=c(30, 90),
                        power=c(0.2, 2.0)))


  # Add legend scale
  lng <- xlim[1] + diff(xlim) * 0.02
  lat <- ylim[1] + diff(ylim) * 0.02

  xseq <- pretty(bbox.grd[1,], n=5)
  dm <- spDistsN1(cbind(xseq[2], lat), c(xseq[1], lat), longlat=TRUE)
  dm.mod <- pretty(c(0, dm), n=1)[2]
  dx <- dm.mod / dm * diff(xseq[1:2])
  n <- length(sp.layout)

  leg.scale <- list("SpatialPolygonsRescale", layout.scale.bar(),
                    offset=c(lng, lat), scale=dx,
                    fill=c("transparent", "black"))
  sp.layout[[n + 1]] <- leg.scale

  scale.txt1 <- list("sp.text", loc=c(lng, lat + dx * 0.1), txt="0", cex=0.75)
  sp.layout[[n + 2]] <- scale.txt1

  scale.txt2 <- list("sp.text", loc=c(lng + dx, lat + 0.1 * dx),
                     txt=paste(dm.mod, "km"), cex=0.75)
  sp.layout[[n + 3]] <- scale.txt2

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
