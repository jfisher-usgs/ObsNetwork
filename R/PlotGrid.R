PlotGrid <- function(grd, zcol, pts, ply, rm.idxs, xlim, ylim, at,
                     pal=terrain.colors, contour=TRUE, label.pts=FALSE, main="",
                     gr.type="windows", gr.file=NULL) {

  # Initialize layout
  lo <- list()

  # Define points
  if (!missing(pts)) {
    if (missing(rm.idxs)) {
      lo[[length(lo) + 1L]] <- list("sp.points", pts, pch=21, cex=0.5,
                                    col="black", fill="white")
    } else {
      lo[[length(lo) + 1L]] <- list("sp.points", pts[-rm.idxs, ], pch=21,
                                    cex=0.5, col="black", fill="white")
      lo[[length(lo) + 1L]] <- list("sp.points", pts[rm.idxs, ], pch=4,
                                    cex=0.5, col="black", lwd=2)
    }
  }

  # Point labels
  labs <- NULL
  if (is.logical(label.pts) && label.pts) {
    labs <- as.character(1:nrow(pts))
  } else if (is.character(label.pts) && label.pts %in% names(pts)) {
    labs <- as.character(pts[[label.pts]])
  }
  if (!is.null(labs)) {
    xy <- coordinates(pts)
    n <- length(lo)
    for (i in 1:nrow(xy)) {
      lo[[n + i]] <- list("sp.text", loc=xy[i, ], txt=labs[i], cex=0.5)
    }
  }

  # Define polygon
  if (!missing(ply)) {
    lo[[length(lo) + 1L]] <- list("sp.polygons", ply, col="black", first=FALSE)
  }

  # Determine axis limits
  bbox.grd <- bbox(grd)
  if (missing(xlim))
    xlim <- range(pretty(extendrange(bbox.grd[1,]), n=7))
  if (missing(ylim))
    ylim <- range(pretty(extendrange(bbox.grd[2,]), n=7))

  # Calculate aspect ratio, used by default in spplot
  asp <- mapasp(grd, xlim=xlim, ylim=ylim)

  # Set width and height of graphics device
  dev.width <- 7
  dev.height <- dev.width * asp

  # Set generic plot arguments
  colorkey <- list(width=1, space="right", labels=list(rot=-90))
  scales <- list(draw=TRUE, y=list(rot=90, tck=-1), x=list(tck=-1))

  # Set axis breakpoints
  if (missing(at))
    at <- pretty(grd[[zcol]], n=20)

  # Set color palettes
  n <- length(at) + 1L
  cols <- pal(n)

  # Add spatial scale legend
  lng <- xlim[1] + diff(xlim) * 0.02
  lat <- ylim[2] - diff(ylim) * 0.06
  dx.1 <- 1
  dm.1 <- spDistsN1(cbind(dx.1, lat), c(0, lat), longlat=TRUE)
  xseq <- pretty(xlim)[1:2]
  dm <- spDistsN1(cbind(xseq[2], lat), c(xseq[1], lat), longlat=TRUE)
  dm.2 <- pretty(c(0, dm), n=1)[2]
  dx.2 <- dm.2 / dm.1
  leg.scale <- list("SpatialPolygonsRescale", layout.scale.bar(),
                    offset=c(lng, lat), scale=dx.2,
                    fill=c("white", "black"))
  scale.txt1 <- list("sp.text", loc=c(lng, lat - dx.2 * 0.05),
                     txt="0", cex=0.75)
  scale.txt2 <- list("sp.text", loc=c(lng + dx.2, lat - dx.2 * 0.05),
                     txt=paste(dm.2, "km"), cex=0.75)
  lo[[length(lo) + 1L]] <- leg.scale
  lo[[length(lo) + 1L]] <- scale.txt1
  lo[[length(lo) + 1L]] <- scale.txt2

  # Open graphics device
  OpenGraphicsDevice(gr.file, type=gr.type, w=dev.width, h=dev.height)

  # Draw plots
  p <- spplot(grd, zcol=zcol, outer=FALSE, aspect=asp,
              scales=scales, xlim=xlim, ylim=ylim,
              col.regions=cols, at=at, main=main,
              colorkey=colorkey, sp.layout=lo,
              contour=contour, labels=FALSE,
              pretty=TRUE, col="gray")
  print(p)

  if (gr.type != "windows")
    dev.off()
}
