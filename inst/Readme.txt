
library(colorspace)
library(gstat)
library(genalg)
library(rgdal)
library(raster)
library(RSurvey)

# setwd("C:/Users/jfisher/Documents/ObsNetwork")
# setwd("D:/Software/ObsNetwork")
setwd("D:/WORK/JFisher/Software/ObsNetwork")
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
obs.file <- "ESRP_Observations.csv.gz"
yr <- 2008
dt.lim <- c("2008-01-01 00:00", "2008-12-31 23:59")
nmax <- 50 # default is Inf


###


# Raster grid
f <- file.path(path, grd.file)
grd <- readGDAL(f, band=1)
names(grd) <- "var2"
grd <- as(crop(raster(grd), extent(c(xlim, ylim))), 'SpatialGridDataFrame')
if (grd.fact > 1) {
  grd <- as(aggregate(raster(grd), fact=grd.fact, fun=mean, expand=TRUE,
                      na.rm=TRUE), 'SpatialGridDataFrame')
}
grd.crs <- grd@proj4string


# Observations
f <- file.path(path, obs.file)
obs <- ReadObservations(f, x.var="dec_long_va", y.var="dec_lat_va",
                        site.var="site_no", net.var="network",
                        var1.var="alt_lev_va", var2.var="alt_va",
                        acy.var="lev_acy", dt.var="lev_dt", dt.lim=dt.lim)
proj4string(obs) <- grd.crs


# Polygon
ply <- readOGR(dsn=file.path(path, ply.dsn), layer=ply.dsn)
ply <- rgdal::spTransform(ply, grd.crs)

PlotGrid(grd, "var2", obs[obs$net == network, ], ply,
         xlim=xlim, ylim=ylim, pal=1L, contour=FALSE, label.pts="mapid")


# Identify drift
lm.drift <- lm(var1 ~ x + y, data=obs)
summary(lm.drift)

# Variogram model (Ordinary-kriging and Regression-kriging)
if (krige.technique == "OK") {
  fo <- var1~1
} else {
  fo <- var1~var2
}
vg <- variogram(fo, obs)
if (fit.vg)
  vg.model <- fit.variogram(vg, vg.model)
## print(plot(vg, vg.model))


# Reduce data
obs.in.ply <- overlay(obs, ply)
if (sum(obs.in.ply, na.rm=TRUE) < nrow(obs))
  warning("")
obs <- obs[!is.na(obs.in.ply), ]

grd$var2 <- grd$var2 * overlay(grd, ply)


# Kriging interpolation
elapsed.time <- system.time({
  kr <- krige(formula=fo, locations=obs, newdata=grd, model=vg.model, nmax=nmax)
})
elapsed.time <- as.numeric(elapsed.time['elapsed'])

kr$var1.se <- sqrt(kr$var1.var)

PlotGrid(kr, "var1.pred", obs, ply, xlim=xlim, ylim=ylim, pal=2L)
PlotGrid(kr, "var1.se",   obs, ply, xlim=xlim, ylim=ylim, pal=3L)


# Cross-validation
cross.validation <- RunCrossValidation(fo, obs, grd, vg.model, nmax, ply)


# Bubble plots
PlotBubble(cross.validation$cv, "residual", main="Residuals",
           ply=ply, xlim=xlim, ylim=ylim)
PlotBubble(obs, "acy", main="Accuracy",
           ply=ply, xlim=xlim, ylim=ylim)
PlotBubble(obs, "sd", main="Standard deviation",
           ply=ply, xlim=xlim, ylim=ylim)


# Optimization

graphics.off()

new.grd <- as(aggregate(raster(grd), fact=10, fun=mean, expand=TRUE,
                        na.rm=TRUE), 'SpatialGridDataFrame')
new.grd$var2 <- new.grd$var2 * overlay(new.grd, ply)
PlotGrid(new.grd, "var2", ply=ply, xlim=xlim, ylim=ylim, pal=2L, contour=FALSE)



ga <- RunGA(obs[obs$net == network, ], grd=new.grd, nsites=5,
            vg.model=vg.model, formula=fo, nmax=nmax, niters=20, pop.size=200)

