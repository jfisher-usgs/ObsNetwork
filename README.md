ObsNetwork
==========

This [R](http://www.r-project.org/ "R") package evaluates and optimizes long-term monitoring networks using a kriging-based genetic algorithm method.

The set of standards used for coding **ObsNetwork** is documented in [Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml "Google's R Style Guide").

Install
-------

If R is not already installed on your computer, download and install the latest binary distribution from [CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").

Open an R session and install the required packages using the following command:

    install.packages("ObsNetwork",
    repos = c("http://jfisher-usgs.github.com/R", "http://cran.us.r-project.org"),
    dependencies = TRUE, type = "both")

Run
---

Load **ObsNetwork** in the current R session:

    library(ObsNetwork)

See help documentation:

    help(package = "ObsNetwork")
