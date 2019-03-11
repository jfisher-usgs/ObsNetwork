# ObsNetwork

[![Travis-CI Build Status](https://travis-ci.org/jfisher-usgs/ObsNetwork.svg?branch=master)](https://travis-ci.org/jfisher-usgs/ObsNetwork)

## Overview

The [R](https://www.r-project.org/) package **ObsNetwork** evaluates and optimizes long-term monitoring networks using a kriging-based genetic algorithm method.

## Install

You can install the stable version of **ObsNetwork** from [GitHub](https://jfisher-usgs.github.io/R/),
and its dependencies from [CRAN](https://cran.r-project.org/), using the following commands:

```r
repos <- c("https://jfisher-usgs.github.io/R", "https://cloud.r-project.org/")
install.packages("ObsNetwork", repos = repos, dependencies = TRUE)
```

Or use **remotes** to install the development version.

```r
remotes::install_github("jfisher-usgs/ObsNetwork", dependencies = TRUE)
```

## Run

Load **ObsNetwork** in the current R session

```r
library(ObsNetwork)
```

Access package documentation

```r
help(package = "ObsNetwork")
```

## Bugs

Please consider reporting bugs and asking questions on the
[Issues page](https://github.com/jfisher-usgs/ObsNetwork/issues).

## Disclaimer

This software has been approved for release by the U.S. Geological Survey
(USGS). Although the software has been subjected to rigorous review, the USGS
reserves the right to update the software as needed pursuant to further analysis
and review. No warranty, expressed or implied, is made by the USGS or the U.S.
Government as to the functionality of the software and related material nor
shall the fact of release constitute any such warranty. Furthermore, the
software is released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.
