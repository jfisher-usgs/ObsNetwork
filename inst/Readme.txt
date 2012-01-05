library(sp)
library(lattice)
library(colorspace)
library(rgdal)
library(gstat)
library(genalg)

library(RSurvey)

RestoreSession("D:/WORK/JFisher/Software/ObsNetwork")
dir.path <- "D:/WORK/JFisher/Software/ObsNetwork"

#RestoreSession("K:/Software/ObsNetwork")
#dir.path <- "K:/Software/ObsNetwork"




###

file.obs <- file.path(dir.path, "inst", "extdata", "ObservationData.txt")
obs <- ReadObservations(file=file.obs, x.var="Longitude", y.var="Latitude",
                        site.var="Site_ID", obs.var="WL_elev",
                        acc.var="Accuracy", dt.var="Date_of_La")

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

#ga <- RunGA(obs, v.fit, grd.ga, nsites=3, niters=3, pop.size=200)

ga <- RunGA(obs, v.fit, grd.ga, nsites=10, niters=200, pop.size=200)


summary.rbga(ga$ans, echo=TRUE)
PlotKriging(obs, v.fit, grd.gr, rm.idxs=ga$rm.idxs)

