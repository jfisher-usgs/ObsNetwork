OpenGraphicsDevice <- function(file, type="windows", w=7, h=7, p=12,
                               res=300, win.title="Save As") {
# This function opens a graphics device of type:
#   "windows", "pdf", "png", or "postscript"

  if (type == "windows") {
    if (.Platform$OS.type == "windows")
      windows(width=w, height=h, pointsize=p, family="sans")
    else
      x11(width=w, height=h, pointsize=p)
  } else {
    if (missing(file) || is.null(file)) {
      if (type == "postscript")
        ext <- ".eps"
      else
        ext <- paste(".", type, sep="")
      file <- as.character(tcl("tk_getSaveFile", title=win.title,
                               defaultextension=ext,
                               initialfile=paste("*", ext, sep=""),
                               initialdir=file.path(getwd(), "..")))
    }
    if (type == "pdf") {
      pdf(file=file, width=w, height=h, pointsize=p, version="1.6",
          colormodel="cmyk")
    } else if (type == "png") {
      png(filename=file, width=w, height=h, units="in", pointsize=p, res=res)
    } else if (type == "postscript") {
      postscript(file=file, width=w, height=h, pointsize=p)
    } else {
      stop(paste("graphics type not recognized"))
    }
  }
}
