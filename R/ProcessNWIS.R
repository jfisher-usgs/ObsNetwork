ProcessNWIS <- function(map.id, sites.id=2008) {
  # ProcessNWIS(map.id="ESRP")
  # ProcessNWIS(map.id="INL")

# path <- "D:/WORK/JFisher/Projects/Observation Network/Data"
  path <- "K:/Projects/Observation Network/Data"

  f <- file.path(path, paste("Map", map.id, "_Sites", sites.id, "_RNWIS.txt",
                             sep=""))
  d <- read.table(f, header=TRUE, sep="\t", stringsAsFactors=FALSE)

  d$lev_dt <- as.POSIXct(as.character(d$lev_dt), format="%Y-%m-%d %H:%M")

  d$alt_lev_va <- d$alt_va - d$lev_va
  d$alt_hole_depth_va <- d$alt_va - d$hole_depth_va

  d$lev_acy <- rep(NA, nrow(d))
  d$lev_acy[d$lev_acy_cd == 0] <- 1
  d$lev_acy[d$lev_acy_cd == 1] <- 0.1
  d$lev_acy[d$lev_acy_cd == 2] <- 0.01

  d$lev_acy <- d$alt_acy_va + d$lev_acy

  d$station_nm <- as.character(sapply(d$station_nm,
                                      function(i) {
                                        x <- unlist(strsplit(i, " "))
                                        paste(x[x != ""], collapse=" ")
                                      }))

  d.out <- d[, c("dec_long_va", "dec_lat_va", "site_no", "alt_va", "lev_va",
                 "alt_lev_va", "lev_acy", "lev_dt", "station_nm")]

  f <- file.path(path, paste("Map", map.id, "_Sites", sites.id, "_Data.gz",
                             sep=""))
  con <- bzfile(f, open="", compression=9)
  write.table(d.out, file=con, quote=FALSE, sep="\t", row.names=FALSE)
}
