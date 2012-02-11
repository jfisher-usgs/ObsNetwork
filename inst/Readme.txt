
library(colorspace)
library(gstat)
library(genalg)
library(rgdal)
library(RSurvey)

setwd("K:/Software/ObsNetwork")
RestoreSession(file.path(getwd(), "R"))

###

map.id <- "INL"; dx <- 0.003
# map.id <- "ESRP"; dx <- 0.01

sites.id <- 2008
yrs <- 2008

path <- file.path(getwd(), "inst", "extdata")

f <- file.path(path, paste("Map", map.id, "_SpatialDomain.gz", sep=""))
grd <- BuildGrid(file=f, x.var="Longitude", y.var="Latitude", dx=dx)

for (yr in yrs) {
  dt.lim <- paste(yr, c("-01-01 00:00", "-12-31 23:59"), sep="")
  f <- file.path(path, paste("Map", map.id, "_Sites", sites.id, "_Data.gz", sep=""))
  obs <- ReadObservations(file=f, x.var="dec_long_va", y.var="dec_lat_va",
                          site.var="site_no", obs.var="alt_lev_va",
                          acy.var="lev_acy", dt.var="lev_dt", dt.lim=dt.lim)

  v.fit <- FitVariogram(obs, model=vgm(model="Lin", nugget=0))
# RunCrossvalidation(obs, v.fit)

  PlotKriging(obs, v.fit, grd)
}






########################### OLD README ###########################

file.obs <- file.path(dir.path, "inst", "extdata", "ObservationData.txt")
obs <- ReadObservations(file=file.obs, x.var="Longitude", y.var="Latitude",
                        site.var="Site_ID", obs.var="WL_elev",
                        acc.var="Accuracy")

###

file.ply <- file.path(dir.path, "inst", "extdata", "SpatialDomain.txt")
grd.gr <- BuildGrid(file=file.ply, x.var="Longitude", y.var="Latitude", dx=0.01)
grd.ga <- BuildGrid(file=file.ply, x.var="Longitude", y.var="Latitude", dx=0.03)

###

#for (i in c(0, 45, 90, 135)) {
#  model <- vgm(psill=190000, model="Gau", range=112, nugget=0, anis=c(i, 0.5))
#  FitVariogram(obs, model)
#}

v.fit <- FitVariogram(obs, model=vgm(model="Lin", nugget=0))

#v.fit <- FitVariogram(obs, model=vgm(psill=190000, model="Gau",
#                                     range=112, nugget=0))

###

RunCrossvalidation(obs, v.fit)

###

#PlotKriging(obs, v.fit, grd.gr, at.pred=seq(2600, 6000, by=200),
#            at.se=seq(30, 70, by=2))

PlotKriging(obs, v.fit, grd.gr)

graphics.off()

#PlotKriging(obs, v.fit, grd.gr, rm.idxs=c(1, 20))

#PlotKriging(obs, v.fit, grd.gr, rm.idxs=50:100)

###

#ga <- RunGA(obs, v.fit, grd.ga, nsites=10, niters=10)

ga <- RunGA(obs, v.fit, grd.ga, nsites=10)

summary.rbga(ga$ans, echo=TRUE)
PlotKriging(obs, v.fit, grd.gr, rm.idxs=ga$rm.idxs)

