
library(colorspace)
library(gstat)
library(genalg)
library(rgdal)
library(raster)
library(RSurvey)

setwd("K:/Software/ObsNetwork")
# setwd("D:/WORK/JFisher/Software/ObsNetwork")
RestoreSession(file.path(getwd(), "R"))

###



network <- "INL"
ply.dsn <- "INL_Polygon"
xlim <- c(-113.3, -112.2)
ylim <- c(43.3, 44.0)
grd.fact <- 1


network <- "State"
ply.dsn <- "ESRP_Polygon"
xlim <- c(-115.25, -111.5)
ylim <- c(42.25, 44.5)
grd.fact <- 2



krige.technique <- "OK"
vg.model <- vgm(model="Lin", nugget=0)
fit.vg <- TRUE



krige.technique <- "UK"
vg.model <- vgm(psill=4200, model="Sph", range=80, nugget=0)
fit.vg <- FALSE



###


grd.file <- "NED_500m.tif"
path <- file.path(getwd(), "inst", "extdata")
obs.file <- "ESRP_Observations.gz"
yr <- 2008
dt.lim <- c("2008-01-01 00:00", "2008-12-31 23:59")
nmax <- 50 # default is Inf


###


# Raster grid
f <- file.path(path, grd.file)
grd <- readGDAL(f, band=1)
names(grd) <- "alt"
grd <- as(crop(raster(grd), extent(c(xlim, ylim))), 'SpatialGridDataFrame')
if (grd.fact > 1) {
  grd <- as(aggregate(raster(grd), fact=grd.fact, fun=mean, expand=TRUE,
                      na.rm=TRUE), 'SpatialGridDataFrame')
}
grd.crs <- grd@proj4string


# Observations
f <- file.path(path, obs.file)
obs <- ReadObservations(f, x.var="dec_long_va", y.var="dec_lat_va",
                        site.var="site_no", net.var="network", alt.var="alt_va",
                        hole.var="hole_depth_va", lev.var="lev_va",
                        acy.var="lev_acy", dt.var="lev_dt", dt.lim=dt.lim)
proj4string(obs) <- grd.crs


# Polygon
ply <- readOGR(dsn=file.path(path, ply.dsn), layer=ply.dsn)
ply <- rgdal::spTransform(ply, grd.crs)


PlotMap(grd, "alt", obs[obs$net == network, ], ply,
        xlim=xlim, ylim=ylim, pal=1L, contour=FALSE)


# Identify drift
lm.drift <- lm(alt.lev ~ x + y, data=obs)
summary(lm.drift)
## rgl::plot3d(x=cbind(coordinates(obs), drift(coordinates(obs))),
##             col="red", xlab="x", ylab="y", zlab="z")
## rgl::plot3d(x=cbind(coordinates(obs), obs$alt.lev), col="blue", add=TRUE)


# Variogram model (Ordinary-kriging and Regression-kriging)
if (krige.technique == "OK") {
  fo <- alt.lev~1
} else {
  fo <- alt.lev~alt
}
vg <- variogram(fo, obs)
if (fit.vg)
  vg.model <- fit.variogram(vg, vg.model)
## plot(vg, vg.model)


# Reduce size
obs.in.ply <- overlay(obs, ply)
if (sum(obs.in.ply, na.rm=TRUE) < nrow(obs))
  warning("")
obs <- obs[!is.na(obs.in.ply), ]

grd.in.ply <- overlay(grd, ply)
grd$alt <- grd$alt * grd.in.ply


# Kriging interpolation
elapsed.time <- system.time({
  kr <- krige(formula=fo, locations=obs, newdata=grd, model=vg.model, nmax=nmax)
})
elapsed.time <- as.numeric(elapsed.time['elapsed'])

kr$var1.se <- sqrt(kr$var1.var)

PlotMap(kr, "var1.pred", obs, ply, xlim=xlim, ylim=ylim, pal=2L)
PlotMap(kr, "var1.se",   obs, ply, xlim=xlim, ylim=ylim, pal=3L, contour=FALSE)

# Cross-validation
cv <- RunCrossValidation(fo, obs, grd, vg.model, nmax, ply)

