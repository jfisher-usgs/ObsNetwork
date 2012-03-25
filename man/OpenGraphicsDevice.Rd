\name{OpenGraphicsDevice}

\alias{OpenGraphicsDevice}

\title{Open Graphics Device}

\description{
This function starts the device driver for producing graphics.
}

\usage{
OpenGraphicsDevice(file, type = "windows", w = 7, h = 7, p = 12,
                   res = 300, win.title = "Save As")
}

\arguments{
\item{file}{character; the path of a file for writing (optional).}
\item{type}{character; the name of a graphics driver, either
  "postscript", "pdf", "png", or the default "windows".}
\item{w, h}{numeric; the (nominal) width and height of the canvas of the
  plotting window in inches. Default is \code{7} and \code{7}, respectively.}
\item{p}{numeric; the default point size of plotted text, its default is
  \code{12}.}
\item{res}{numeric; the nominal resolution in points per inch (ppi)
  which will be recorded in the Portable Network Graphics (PNG) file,
  its default is \code{300} ppi to set the size of text and line widths.}
\item{win.title}{character; a string to display as the title of the dialog box,
  only used if \code{file} is not specified.}
}

\details{
A \code{"postscript"} graphics device produces an Encapsulated PostScript
(EPS) file and is recommended for figures of publication quality.
}

\value{
A plot device is opened.
}

\author{J.C. Fisher}

\seealso{
\code{\link{postscript}}, \code{\link{pdf}}, \code{\link{png}},
\code{\link{windows}}
}

\examples{
OpenGraphicsDevice(type = "windows")
plot(1:3)
dev.off()
\donttest{
OpenGraphicsDevice(type = "pdf")
plot(1:3)
dev.off()}
}

\keyword{device}
