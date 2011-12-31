
library(RSurvey)

library(lattice)
library(sp)
library(rgdal)
library(gstat)
library(colorspace)



RestoreSession("D:/WORK/JFisher/Software/ObsNetwork")


dir.path <- "D:/WORK/JFisher/Software/ObsNetwork/inst/extdata"



###

file.obs <- file.path(dir.path, "ObservationData.txt")

obs <- ReadObservations(file=file.obs, site.var="Site_ID", obs.var="WL_elev",
                        acc.var="Accuracy", lng.var="Longitude",
                        lat.var="Latitude", dt.var="Date_of_La")

###

file.ply <- file.path(dir.path, "SpatialDomain.txt")

grd <- BuildGrid(file=file.ply, lng.var="Longitude", lat.var="Latitude",
                 dx=0.03)


###

#for (i in c(0, 45, 90, 135)) {
#  model <- vgm(psill=190000, model="Gau", range=112, nugget=0, anis=c(i, 0.5))
#  FitVariogram(obs, model)
#}


model <- vgm(psill=190000, model="Gau", range=112, nugget=0)
v.fit <- FitVariogram(obs, model)


###


at.pred <- seq(2600, 6000, by=200)
at.se <- seq(30, 70, by=2)

PlotKriging(obs, v.fit, grd, at.pred=at.pred, at.se=at.se)

#PlotKriging(obs, v.fit, grd, at.pred=at.pred, at.se=at.se, rm.idxs=c(1, 20))

#PlotKriging(obs, v.fit, grd, rm.idxs=50:100)






graphics.off()



