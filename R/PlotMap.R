PlotMap <- function(obj, zcol, pts, ply, rm.idxs, xlim, ylim, at, pal=1L,
                    contour=TRUE, main="", gr.type="windows", gr.file=NULL) {

  # Define point symbols
  if (missing(pts)) {
    sp.layout <- list()
  } else {
    if (missing(rm.idxs)) {
      pts.o <- list("sp.points", pts, pch=21, cex=0.5, col="black", fill="white")
      sp.layout <- list(pts.o)
    } else {
      pts.o <- list("sp.points", pts[-rm.idxs, ], pch=21, cex=0.5,
                    col="black", fill="white")
      pts.x <- list("sp.points", pts[rm.idxs, ], pch=4, cex=0.5,
                    col="black", lwd=2)
      sp.layout <- list(pts.o, pts.x)
    }
  }

  # Define polygon
  if (!missing(ply)) {
    n <- length(sp.layout)
    sp.layout[[n + 1]] <- list("sp.polygons", ply, col="black", first=FALSE)
  }

  # Determine axis limits
  bbox.obj <- bbox(obj)
  if (missing(xlim))
    xlim <- range(pretty(extendrange(bbox.obj[1,]), n=7))
  if (missing(ylim))
    ylim <- range(pretty(extendrange(bbox.obj[2,]), n=7))

  # Calculate aspect ratio, used by default in spplot
  asp <- mapasp(obj, xlim=xlim, ylim=ylim)

  # Set width and height of graphics device
  dev.width <- 7
  dev.height <- dev.width * asp

  # Set generic plot arguments
  colorkey <- list(width=1, space="right", labels=list(rot=90))
  scales <- list(draw=TRUE, y=list(rot=90, tck=-1), x=list(tck=-1))

  # Set axis breakpoints
  if (missing(at))
    at <- pretty(obj[[zcol]], n=20)

  # Set color palettes
  if (pal == 1L) {
    cols <- rev(diverge_hcl(length(at) + 1, h=c(260, 0), c=100,
                            l=c(50, 90), power=1.0))
  } else if (pal == 2L) {
    cols <- rev(heat_hcl(length(at) + 1,
                         h=c(234, 66), c=c(79, 25), l=c(56, 85),
                         power=c(0.2, 1.1)))
  } else if (pal == 3L) {
    cols <- rev(heat_hcl(length(at) + 1,
                         h=c(0, 90), c=c(80, 30), l=c(30, 90),
                         power=c(0.2, 2.0)))
  }

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
  n <- length(sp.layout)
  sp.layout[[n + 1]] <- leg.scale
  sp.layout[[n + 2]] <- scale.txt1
  sp.layout[[n + 3]] <- scale.txt2

  # Open graphics device
  if (gr.type == "windows") {
    x11(width=dev.width, height=dev.height)
  } else {
    if (is.null(gr.file))
      stop()
    if (gr.type == "postscript") {
      postscript(file=gr.file, width=dev.width, height=dev.height, pointsize=12,
                 horizontal=FALSE, paper="special")
    } else if (gr.type == "png") {
      png(filename=gr.file, width=dev.width, height=dev.height, units="in",
          pointsize=12, res=300)
    }
  }

  # Draw plots
  print(spplot(obj, zcol=zcol, aspect=asp,
               scales=scales, xlim=xlim, ylim=ylim,
               col.regions=cols, at=at, main=main,
               colorkey=colorkey, sp.layout=sp.layout,
               contour=contour, labels=FALSE,
               pretty=TRUE, col="gray"))

  if (gr.type != "windows")
    dev.off()
}
