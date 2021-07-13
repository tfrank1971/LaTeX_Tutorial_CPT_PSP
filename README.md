# LaTeX_Tutorial_CPT_PSP
LaTeX tutorial for standardization and automation of population analysis reports. To cite this work please read and cite the manuscript:
## textblocks directory
Contains LaTeX source files that hold the actual content of the report, e.g., summary.tex, introduction.tex, etc.
## data directory
Contains NONMEM input data set `data_set_504.dat`
## license directory
Contains a text file holding GNU AFFERO GENERAL PUBLIC LICENSE
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
(Hidden) configuration file for automated report compilation with **latexmk**
## file `extractrep.pl`
Perl script completing the action of some of the R functions under `Rfunctions`
## file `makereprt.pl`
Perl script that retrieves the template files from a directory and renames them according to user input. Adapt the variable $LATEXTMPLDIR as explained in the paper.
## file `#report#.bib`
Literature database file from which the reference list will be created by BibLaTeX and biber
## file `#report#.tex`
The main source file (document environment) in which the titlepage, approval page, textblocks, behind-of-text displays, tables, and appendix get included
## file `#report#-appendix.tex`
Source file of the appendix
## file `#report#-approval.tex`
Source file of the approval page
## file `#report#-behindoftextdisplays.tex`
Source file to include additional figures and tables behind the core sections of the report
## file `#report#-preamble.tex`
Preamble, e.g., to define common macros to plot figures and tables, to define layout, fonts, page header and footer
## file `#report#-properties.tex`
Defines the meta information, i.e., title, subtitle, author name, reviewer name, and other variables used by the other template files
## file `#report#-titlepage.tex`
Source file of the titlepage
## file `myacronyms.tex`
Definition of terms and symbols; used to create the list of acronyms and definition of terms
## file `sample.pdf`
Example of the compiled report template
