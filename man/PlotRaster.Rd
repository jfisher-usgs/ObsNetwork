\name{PlotRaster}

\alias{PlotRaster}

\title{Create a Filled Contour Plot of Spatial Data}

\description{
Raster plot for spatial data with attributes.
}

\usage{
PlotRaster(grd, zcol, pts, ply, rm.idxs, xlim, ylim, at,
           pal = heat.colors, contour = FALSE, label.pts = FALSE,
           main = "", gr.type = "windows", gr.file = NULL)
}

\arguments{
\item{grd}{SpatialGridDataFrame; raster data.}
\item{zcol}{character; z-variable column name or number in attribute table
  of \code{grd}.}
\item{pts}{SpatialPointsDataFrame; point coordinates to include in layout.}
\item{ply}{SpatialPolygonsDataFrame; polygons to include in layout.}
\item{rm.idxs}{integer; vector of row indexes in \code{pts}, point symbols at
  these coordinates are drawn as crosses (\code{pch=4}) rather than the
  default, an open circle symbol (\code{pch=21}).}
\item{xlim}{numeric; vector of length 2 giving left and right limits for
  the x-axis.}
\item{ylim}{numeric; vector of length 2 giving lower and upper limits for
  the y-axis.}
\item{at}{numeric; vector giving breakpoints along the range of z.}
\item{pal}{function; a color palette to be used to assign colors in the plot.}
\item{contour}{logical; add contour lines to plot, default is \code{FALSE}.}
\item{label.pts}{character; column name in \code{pts}, specifies the labels to
  place at point coordinates (optional).}
\item{main}{character; main plot title to be placed on top.}
\item{gr.type}{character; the name of a graphics driver, either
  "postscript", "pdf", "png", or the default "windows".}
\item{gr.file}{character; the path of a file for writing the graphics,
               only used if \code{gr.type} is not "windows".}
}

\value{
Returns (or plots) the raster plot.
}

\author{J.C. Fisher}

\seealso{
\code{\link[sp]{spplot}}
}

\examples{
data(ESRP_NED500m)
data(ESRP_WaterLevels_2008)
data(ESRP_SpatialDomain)

PlotRaster(ESRP_NED500m, "var2", ESRP_WaterLevels_2008, ESRP_SpatialDomain)

pts <- ESRP_WaterLevels_2008[ESRP_WaterLevels_2008$net == "State", ]
xlim <- c(-115.25, -111.50)
ylim <- c(  42.25,   44.50)
PlotRaster(ESRP_NED500m, "var2", pts, ESRP_SpatialDomain, xlim = xlim,
           ylim = ylim, pal = terrain.colors, label.pts = "mapid")

# Plot hill shade from slope and aspect layers
slp <- terrain(raster(ESRP_NED500m), opt = "slope")
asp <- terrain(raster(ESRP_NED500m), opt = "aspect")
grd <- as(hillShade(slp, asp, 40, 270), "SpatialGridDataFrame")
zlim <- range(grd[[1]], na.rm = TRUE)
at <- seq(zlim[1], zlim[2], length.out = 100)
pal <- function(n) grey(0:100 / 100)[1:n]
PlotRaster(grd, 1, xlim = xlim, ylim = ylim, at = at, pal = pal)

# Kriging
newdata <- ESRP_NED500m
newdata$var2 <- newdata$var2 * sp::overlay(newdata, ESRP_SpatialDomain)
vg.model <- vgm(psill = 4500, model = "Sph", range = 100, nugget = 0)
kr <- krige(formula = var1 ~ var2, locations = ESRP_WaterLevels_2008,
            newdata = newdata, model = vg.model)
kr$var1.se <- sqrt(kr$var1.var) # standard error

# Plot prediction and standard error maps
PlotRaster(kr, "var1.pred", ESRP_WaterLevels_2008 , ESRP_SpatialDomain,
           pal = colorRampPalette(c("#F02311", "#F7FDFA", "#107FC9")),
           main = "Predictions", xlim = xlim, ylim = ylim)
PlotRaster(kr, "var1.se", ESRP_WaterLevels_2008 , ESRP_SpatialDomain,
           pal = terrain.colors, main = "Standard errors",
           xlim = xlim, ylim = ylim)

# Reduce axis limits and replot maps
xlim <- c(-113.30, -112.20)
ylim <- c(  43.30,   44.00)
x <- coordinates(kr)[, "x"]
y <- coordinates(kr)[, "y"]
is.in.lim <- x >= xlim[1] & x <= xlim[2] & y >= ylim[1] & y <= ylim[2]
PlotRaster(kr[is.in.lim, ], "var1.pred", ESRP_WaterLevels_2008 ,
           ESRP_SpatialDomain, xlim = xlim, ylim = ylim,
           pal = colorRampPalette(c("#F02311", "#F7FDFA", "#107FC9")),
           main = "Predictions")
PlotRaster(kr[is.in.lim, ], "var1.se", ESRP_WaterLevels_2008,
           ESRP_SpatialDomain, xlim = xlim, ylim = ylim,
           pal = terrain.colors, main = "Standard errors")
}

\keyword{hplot}
