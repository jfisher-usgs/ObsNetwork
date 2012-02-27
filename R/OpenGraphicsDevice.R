OpenGraphicsDevice <- function(file, type="windows", w=8.5, h=11, p=12,
                               res=300) {
# This function opens a graphics device of type:
#   "windows", "pdf", "png", or "postscript"

  if (type == "windows") {
    x11(width=w, height=h, pointsize=p)
  } else {
    if (missing(file)) {
      f <- tempfile(pattern="R", tmpdir=getwd(),
                    fileext=paste(".", type, sep=""))
    }
    if (type == "pdf") {
      pdf(file=f, width=w, height=h, pointsize=p, version="1.6",
          colormodel="cmyk")
    } else if (type == "png") {
      png(filename=f, width=w, height=h, units="in", pointsize=p, res=res)
    } else if (type == "postscript") {
      postscript(file=f, width=w, height=h, pointsize=p)
    } else {
      stop(paste("graphics type not recognized"))
    }
  }
}
