\name{OpenGraphicsDevice}

\alias{OpenGraphicsDevice}

\title{Open Graphics Device}

\description{
This function starts the device driver for producing graphics.
}

\usage{
OpenGraphicsDevice(file, type = "windows", w = 7, h = 7, p = 12,
                   res = 300, gui.title = "Save As")
}

\arguments{
\item{file}{character; the name of a file for writing.}
\item{type}{character; the name of graphics driver, either
  \code{"pdf"}, \code{"postscript"}, \code{"png"}, or \code{"windows"}.}
\item{w, h}{numeric; the (nominal) width and height of the canvas of the
  plotting window in inches. Default is \code{7} and \code{7}, respectively.}
\item{p}{numeric; the default point size of plotted text, its default is
  \code{12}.}
\item{res}{numeric; the nominal resolution in points per inch (ppi)
  which will be recorded in the PNG file, its default is \code{300} ppi
  to set the size of text and line widths.}
\item{win.title}{character; a string to display as the title of the dialog box.}
}

\details{
A \code{"postscript"} graphics device produces an EPS file and
is recommended for producing figures of publication quality.
}

\value{
A plot device is opened.
}

\author{J.C. Fisher}

\seealso{
\code{\link{windows}}, \code{\link{pdf}}, \code{\link{png}},
\code{\link{postscript}}
}

\examples{OpenGraphicsDevice(type = "windows")
dev.off()
}

\keyword{misc}
