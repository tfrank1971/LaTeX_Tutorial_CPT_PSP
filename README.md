# LaTeX_Tutorial_CPT_PSP
LaTeX tutorial for standardization and automation of population analysis reports. To cite this work please read and cite the manuscript:
## textblocks directory
Contains LaTeX source files that hold the actual content of the report, e.g., summary.tex, introduction.tex, etc.
## data directory
Contains NONMEM input data set `data_set_504.dat`
## logo directory
Contains image file `logo.png`
## nonmem directory
Contains 3 tar.gz files containing NONMEM control streams and results for base model (No. 1117171), full covariate model (No. 1117172) and final model (No. 1117173)
## Rfunctions directory
Contains 24 R functions needed to post-process NONMEM archives
## Rresults directory
Includes subdirectories containing tables and plots belonging to the NONMEM analysis (output created by the 3 R scripts `standard.plots.R`, `exploratory.R` and `postprocessing.R`)
## Rscripts directory
Contains 3 R scripts `standard.plots.R` (functions to create goodness-of-fit plots and summary tables for NONMEM runs), `exploratory.R` (descriptive statistics of PK samples and subjects, concentration versus time plots of raw data), and `postprocessing.R` (post-processing steps for NONMEM runs)
## file `.latexmkrc`
(Hidden) configuration file for automated report compilation with latexmk
