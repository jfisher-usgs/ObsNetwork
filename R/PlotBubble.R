PlotBubble <- function(pts, zcol, ply, xlim=bbox(ply)[1, ], ylim=bbox(ply)[2, ],
                       main="", gr.type="windows", gr.file=NULL,
                       projargs=proj4string(pts)) {

  # Transform projection and datum
  crs <- CRS(projargs)
  pts <- spTransform(pts, crs)
  if (!missing(ply))
    ply <- spTransform(ply, crs)

  coords <- as.data.frame(coordinates(pts))
  is.in.bbox <- coords[, 1] >= xlim[1] & coords[, 1] <= xlim[2] &
                coords[, 2] >= ylim[1] & coords[, 2] <= ylim[2]
  pts <- pts[is.in.bbox, ]

  sp.layout <- list()
  sp.layout[[1]] <- list("sp.polygons", ply, col="black", first=FALSE)
  scales <- list(draw=TRUE, y=list(rot=90, tck=-1), x=list(tck=-1))

  OpenGraphicsDevice(gr.file, type=gr.type, w=7, h=7)

  tcl <- 0.50 / (6 * par("csi"))

  p <- sp::bubble(pts, zcol, main=main, tcl=tcl, xlim=xlim, ylim=ylim,
                  scales=scales, sp.layout=sp.layout, key.space="bottom")
  print(p)

  if (gr.type != "windows")
    dev.off()
}
