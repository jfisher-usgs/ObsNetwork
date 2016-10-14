# ObsNetwork 1.0.0.9000

- ...

# ObsNetwork 1.0.0

- Change repository for package installation to https://jfisher-usgs.github.io/R

- Minor changes for compiling with R version 3.3.0.

- Change format for package version numbering from #.#-# to #.#.#

# ObsNetwork 0.2-1

- Uncompressed "ESRP_NWIS.txt.gz" file and saved to "ESRP_NWIS.tsv" text file.

# ObsNetwork 0.2-0

- Tidy help documentation and import dependent packages in namespace.

# ObsNetwork 0.1-9

- Graphics are now opened in a platform-independent way using the `dev.new` function.

- Fixed broken URL in citation.

- Package namespaces for **tcltk** and **GA** are imported from and no longer need to be attached.

# ObsNetwork 0.1-7

- Replaced the deprecated overlay function with over.

- Added citation for U.S. Geological Survey report.

# ObsNetwork 0.1-6

- LICENSE file edited to clearly state that this software is in the public domain.

- Revised calculation of the number of iterations the best fitness value was repeated; was reporting n + 1 for n repeats.

# ObsNetwork 0.1-5

- Added percent local error to output.

- Added `maxabort` argument in `OptimizeNetwork` function.

# ObsNetwork 0.1-4

- Implemented crossover and mutation functions that are problem specific.

- Improved construction of initial default population.

- Implemented variable penalty function.

- Changed chromosome from binary to integer type.

- Changed genetic algorithm (GA) R library from **genalg** to **GA**.

- Software license changed from "Unlimited" to "GPL (>= 2)".
