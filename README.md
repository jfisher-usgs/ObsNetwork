ObsNetwork
==========

This [R](http://www.r-project.org/ "R") package
evaluates and optimizes long-term monitoring networks using a kriging-based
genetic algorithm method.

The set of standards used for coding **ObsNetwork** is documented in
[Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html "Google's R Style Guide").

Install
-------

If R is not already installed on your computer, download and install the latest
binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the startup options and specifying the SDI interface
(not the default).

Open an R session and install the required packages from CRAN:

    > install.packages(c('sp', 'rgdal', 'gstat', 'raster', 'GA', 'devtools'))

Download and compile the **ObsNetwork** package (this may take a few minutes):

    > devtools::install_github('ObsNetwork', username='jfisher-usgs', ref='v0.1.7')

Run
---

Load **ObsNetwork** in the current R session:

    > library(ObsNetwork)

See examples in the help documentation:

    > help(package='ObsNetwork')
