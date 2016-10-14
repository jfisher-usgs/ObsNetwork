# ObsNetwork

This [R](http://www.r-project.org/ "R") package evaluates and optimizes long-term monitoring networks using a kriging-based genetic algorithm method.

## Install

If R is not already installed on your computer, download and install the latest binary distribution from the
Comprehensive R Archive Network ([CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network")).
Next, open an R session and install user-contributed R packages using the following commands:

```r
repos <- c("https://jfisher-usgs.github.io/R", getOption("repos"))
update.packages(ask = FALSE, repos = repos)
install.packages("ObsNetwork", repos = repos)
```

## Run

Load **ObsNetwork** in the current R session:

```r
library(ObsNetwork)
```

Access package documentation:

```r
help(package = "ObsNetwork")
```

## Bugs

Please consider reporting bugs and asking questions on the Issues page:

[https://github.com/jfisher-usgs/ObsNetwork/issues](https://github.com/jfisher-usgs/ObsNetwork/issues)

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey,
an agency of the United States Department of Interior.
For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy").

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied,
is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)
