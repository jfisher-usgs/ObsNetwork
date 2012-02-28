PlotBubble <- function(pts, zcol, ply, xlim, ylim, main="",
                       gr.type="windows", gr.file=NULL) {

  sp.layout <- list()
  sp.layout[[1]] <- list("sp.polygons", ply, col="black", first=FALSE)
  scales <- list(draw=TRUE, y=list(rot=90, tck=-1), x=list(tck=-1))

  OpenGraphicsDevice(gr.file, type=gr.type, w=7, h=7)

  tcl <- 0.50 / (6 * par("csi"))

  p <- bubble(pts, zcol, main=main, tcl=tcl, xlim=xlim, ylim=ylim,
              scales=scales, sp.layout=sp.layout, key.space="bottom")
  print(p)

  if (gr.type != "windows")
    dev.off()
}
