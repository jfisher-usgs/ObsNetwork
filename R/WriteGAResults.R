WriteGAResults <- function(ga, file) {

  if (missing(file))
    file <- as.character(tcl("tk_getSaveFile", title="Save GA Results As",
                             defaultextension="txt", initialfile="*.txt",
                             initialdir=file.path(getwd(), "..")))

  # Removed observations
  x <- ga$rm.obs
  write.table(x, file=file, append=FALSE, quote=FALSE, sep="\t",
              row.names=FALSE)
  cat("\n", file=file, append=TRUE)

  # Objective values
  x <- ga$obj.values[nrow(ga$obj.values), ]
  write.table(x, file=file, append=TRUE, col.names=FALSE, row.names=TRUE,
              quote=FALSE, sep="\t")

  # Answer repeated
  x <- ga$ans.rep
  cat(x, file=file, append=TRUE)

  # Elapsed time
  x <- ga$elapsed.time
  cat(x, file=file, append=TRUE)
}
