WriteGAResults <- function(ga, file) {

  if (missing(file))
    file <- as.character(tcltk::tcl("tk_getSaveFile",
                                    title="Save GA Results As",
                                    defaultextension="txt",
                                    initialfile="*.txt",
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
  cat("\nNumber of times final solution was repeated:", x, "\n", file=file,
      append=TRUE)

  # Elapsed time
  x <- ga$elapsed.time
  cat("\nElapsed time:", format(x), "hours\n", file=file, append=TRUE)

  invisible(NULL)
}
