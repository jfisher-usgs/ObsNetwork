\name{PlotBubble}

\alias{PlotBubble}

\title{Create a Bubble Plot of Spatial Data}

\description{
Bubble plot for spatial data with attributes.
This is a wrapper around \code{\link[sp]{bubble}} in the \pkg{sp} package.
}

\usage{
PlotBubble(pts, zcol, ply, xlim = bbox(ply)[1, ], ylim = bbox(ply)[2, ],
           main = "", gr.type = "windows", gr.file = NULL,
           projargs = proj4string(pts))
}

\arguments{
\item{pts}{\code{SpatialPointsDataFrame}; the data at the observation sites.}
\item{zcol}{\code{character}; the z-variable column name in attribute table of \code{pts}.}
\item{ply}{\code{SpatialPolygonsDataFrame}; a polygon to include in layout.}
\item{xlim}{\code{numeric}; a vector of length 2 giving left and right limits for the x-axis.}
\item{ylim}{\code{numeric}; a vector of length 2 giving lower and upper limits for the y-axis.}
\item{main}{\code{character}; the main plot title to be placed on top.}
\item{gr.type}{\code{character}; the name of a graphics driver, either "postscript", "pdf", "png", or the default "windows".}
\item{gr.file}{\code{character}; the path of a file for writing the graphics, only used if \code{gr.type} is not "windows".}
\item{projargs}{\code{character}; the projection arguments; the arguments must be entered exactly as in the PROJ.4 documentation.}
}

\value{
Returns (or plots) the bubble plot.
Values plotted in the key include the five quantiles: minimum, 25th percentile, median, 75th percentile, and maximum.
}

\author{J.C. Fisher}

\seealso{
\code{\link[sp]{bubble}}, \code{\link{OpenGraphicsDevice}}
}

\examples{
data(ESRP_NWIS)
data(ESRP_Boundary)
data(ESRP_NED)

PlotBubble(ESRP_NWIS, "var1.acy", ESRP_Boundary,
           main = "Measurment error")
PlotBubble(ESRP_NWIS, "var1.sd", ESRP_Boundary,
           xlim = c(178000, 257500), ylim = c(202000, 272000),
           main = "Standard deviation", projargs = proj4string(ESRP_NED))
}

\keyword{hplot}
