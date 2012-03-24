PlotRaster <- function(grd, zcol, pts, ply, rm.idxs, xlim, ylim, at,
                       pal=heat.colors, contour=FALSE, label.pts=FALSE,
                       main="", gr.type="windows", gr.file=NULL,
                       width=7, height=NA, lo=list(),
                       add.llgridlines=FALSE, crop.grid=FALSE) {

  # Transform points and polygon projection and datum
  crs <- CRS(proj4string(grd))
  if (!missing(pts))
    pts <- spTransform(pts, crs)
  if (!missing(ply))
    ply <- spTransform(ply, crs)
  for (i in seq(along=lo)) {
    if (is.list(lo[[i]]) && length(lo[[i]]) > 1) {
      obj <- lo[[i]][[2]]
      ans <- try(proj4string(obj), silent=TRUE)
      if (!inherits(ans, "try-error"))
        lo[[i]][[2]] <- spTransform(obj, crs)
    }
  }

  # Exclude raster data outside of polygon
  if (crop.grid && !missing(ply))
    grd[[zcol]]  <- grd[[zcol]] * overlay(grd, ply)

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

  # Determine axis limits
  bbox.grd <- bbox(grd)
  if (missing(xlim))
    xlim <- range(pretty(extendrange(bbox.grd[1,]), n=7))
  if (missing(ylim))
    ylim <- range(pretty(extendrange(bbox.grd[2,]), n=7))

  # Reduce points to axis limits
  if (!missing(pts)) {
    coords <- as.data.frame(coordinates(pts))
    is.in.bbox <- coords$x >= xlim[1] & coords$x <= xlim[2] &
                  coords$y >= ylim[1] & coords$y <= ylim[2]
    pts <- pts[is.in.bbox, ]
  }

  # Define polygon
  if (!missing(ply))
    lo[[length(lo) + 1L]] <- list("sp.polygons", ply, col="black", first=FALSE)

  # Calculate aspect ratio, used by default in spplot
  asp <- mapasp(grd, xlim=xlim, ylim=ylim)

  # Set height of graphics device
  if (is.na(height))
    height <- 7
  if (inherits(asp, "numeric"))
    height <- width * asp

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
  if (!is.projected(grd)) {
    x <- xlim[1] + diff(xlim) * 0.02
    y <- ylim[2] - diff(ylim) * 0.06
    dx.1 <- 1
    dm.1 <- spDistsN1(cbind(dx.1, y), c(0, y), longlat=TRUE)
    xseq <- pretty(xlim)[1:2]
    dm <- spDistsN1(cbind(xseq[2], y), c(xseq[1], y), longlat=TRUE)
    dm.2 <- pretty(c(0, dm), n=1)[2]
    dx.2 <- dm.2 / dm.1
    leg.scale <- list("SpatialPolygonsRescale", layout.scale.bar(),
                      offset=c(x, y), scale=dx.2, fill=c("white", "black"))
    txt1 <- list("sp.text", loc=c(x, y - dx.2 * 0.05), txt="0", cex=0.75)
    txt2 <- list("sp.text", loc=c(x + dx.2, y - dx.2 * 0.05),
                 txt=paste(dm.2, "km"), cex=0.75)
    lo[[length(lo) + 1L]] <- leg.scale
    lo[[length(lo) + 1L]] <- txt1
    lo[[length(lo) + 1L]] <- txt2
  }

  # Add long-alt grid over projected data
  if (add.llgridlines && is.projected(grd)) {
    obj <- as(grd, "SpatialPointsDataFrame")
    obj.ll <- spTransform(obj, CRS("+proj=longlat +datum=WGS84"))
    easts <- pretty(bbox(obj.ll)[1, ])
    norths <- pretty(bbox(obj.ll)[2, ])
    grd.ll <- gridlines(obj.ll, easts=easts, norths=norths, ndiscr=50)
    grd.xy <- spTransform(grd.ll, CRS(proj4string(obj)))
    lo[[length(lo) + 1L]] <- list("sp.lines", grd.xy, lty=3, first=FALSE)
  }

  # Open graphics device
  OpenGraphicsDevice(gr.file, type=gr.type, w=width, h=height)

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
