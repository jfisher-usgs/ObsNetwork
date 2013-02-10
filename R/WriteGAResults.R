WriteGAResults <- function(x, file) {
  
  if (missing(x))
    stop("object is missing")
  
  if (missing(file)) {
    file <- as.character(tcltk::tcl("tk_getSaveFile", 
                                    title="Save GA Results As",
                                    defaultextension="txt",
                                    initialfile="*.txt",
                                    initialdir=file.path(getwd(), "..")))
    if (length(file) == 0 | !nzchar(file[1]))
      return(NULL)
    file <- paste(file, collapse=" ")
  }
  
  # Call
  cat(paste(format(x$call), collapse="\n"), "\n\n", file=file, append=FALSE)
  
  # Objectives
  tbl <- x$obj.values[nrow(x$obj.values), ]
  write.table(tbl, file=file, col.names=FALSE, row.names=TRUE, quote=FALSE, 
              sep="\t", append=TRUE)
  
  # Root-mean-square-deviation
  cat("\nRoot-mean-square-deviation:\t", format(x$rmsd), "\n", 
      file=file, append=TRUE)
  
  # Iterations
  cat("\nNumber of completed iterations:\t", x$niter, "\n", 
      file=file, append=TRUE)
  
  # Repeats
  cat("\nNumber of times final solution was repeated:\t", x$nrep.ans, "\n", 
      file=file, append=TRUE)

  # Elapsed time
  cat("\nElapsed time, in hours:\t", 
      format(as.numeric(x$proc.time["elapsed"]) / 3600), "\n", 
      file=file, append=TRUE)
  
  # Solution-space
  cat("\nRange of objective values in solution space:\n", 
      file=file, append=TRUE) 
  cat("\t", paste(colnames(x$obj.space), collapse="\t"), "\n", 
      file=file, append=TRUE) 
  write.table(x$obj.space, file=file, col.names=FALSE, row.names=TRUE, 
              quote=FALSE, sep="\t", append=TRUE)
  
  # Penalty
  npenalty <- sum(x$ncalls.penalty)
  ppenalty <- npenalty / (x$ga.ans@popSize * x$niter) * 100
  cat("\nNumber of calls to penalty function:\t", npenalty, "\t", 
      ppenalty, "%\n", file=file, append=TRUE)
  cat(paste(deparse(as.numeric(x$ncalls.penalty)), collapse="\n"), "\n",
      file=file, append=TRUE)
  
  # Remove
  cat("\nRemoved sites:\n", file=file, append=TRUE) 
  cat(paste(names(x$pts.rm), collapse="\t"), "\n", file=file, append=TRUE)
  write.table(x$pts.rm, file=file, append=TRUE, quote=FALSE, sep="\t", 
              col.names=FALSE, row.names=FALSE)
  
  # Suggestion
  cat("\nSuggestion for initial population:\n", file=file, append=TRUE)
  cat(paste(deparse(x$ga.ans@population), collapse="\n"), "\n",
      file=file, append=TRUE)
  
  # System time
  cat("\nSystem date-time at start of GA run:\t", 
      format(x$start.time, "%Y-%m-%d %H:%M:%S"), "\n", file=file, append=TRUE)
  
  cat("\nTime differences at the end of each GA iteration, in hours:\n", 
      file=file, append=TRUE)
  cat(paste(deparse(x$diff.time), collapse="\n"), "\n", file=file, append=TRUE)
  
  invisible(NULL)
}
