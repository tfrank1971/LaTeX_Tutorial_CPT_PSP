##******************************************************************
## Study R-script name : postprocessing.R
## Description : post-processing of NONMEM runs
## Platform : openSUSE Linux, version 15.1
## Original programmer : TF
## Current programmer / Modified by : SG
## Compound number :  
## Study code : 
## Date completed : Apr 2021
## Outputs created : figures, tables
## R version : R version 3.6.1
## NONMEM version : 7.4.1
##**************************************************************

rm(list=ls())

# additional packages ----------------------------------------

library(plyr)
library(dplyr)
library(papeR)
library(reshape)
library(ggplot2)

# set working directory----------------------------------------------------
wd <- "/workfra/content/POHXXXX/analysis/"
setwd(wd)

#additional functions---------------------------------

source("standard.plots.R") 
source(".R/unziparch.R")
source(".R/ziparch.R")
source(".R/getnmr7.R")
source(".R/createXposeClassesSanofi.R")
source(".R/getnmr7.esti.R")
source(".R/printnmr7.R")
source(".R/npdeplot.R")
source(".R/contcorr.R")
source(".R/datalist.R")
source(".R/dataoverview.R")
source(".R/paramsum.R")
source(".R/gofplots.R")
source(".R/ctpop.R")
source(".R/getN.R")
source(".R/ckeckdat.R")
source(".R/carryfwd.R")
source(".R/plotlist.R")
source(".R/printSessionInfo.R")
source(".R/extractdata.R")
source(".R/makeFootnote.R")
source(".R/compnmr7.R")
source(".R/unpaste.R")
source(".R/check.factor.R")
source(".R/appendList.R")

##create symbolic links in a separate folder only for model comparison
basemodel<-"run504_no_cov.1117171.tar.gz"
fullcovmod<-"run504_full_cov.1117172.tar.gz"
finalmodel<-"run504_with_cov_fix.1117173.tar.gz"

system(paste("ln -s ",wd,"nmnqs/",basemodel," ",wd,"comparison/nmnqs/",basemodel, sep=""))
system(paste("ln -s ",wd,"nmnqs/",fullcovmod," ",wd,"comparison/nmnqs/",fullcovmod, sep=""))
system(paste("ln -s ",wd,"nmnqs/",finalmodel," ",wd,"comparison/nmnqs/",finalmodel, sep=""))

## base model
standard.plots(
  path=wd,
  num=1117171,
  subdir="comparison",
  base_model = T
  
)
## full covariates
standard.plots(
  path=wd,
  num=1117172,
  subdir="comparison",
  base_model = F
  
)
## fixing some covariates
standard.plots(
  path=wd,
  num=1117173,
  subdir="comparison",
  base_model = F
 
)
##extract files from archive for inclusion in the report
system(paste("tar -xvzf ",wd,"nmnqs/",basemodel," tables/paramsum1117171.tex -C tables", sep="")) # parameter summary
system(paste("tar -xvzf ",wd,"nmnqs/",basemodel," plots -C plots", sep="")) # GOF plots
system(paste("tar -xvzf ",wd,"nmnqs/",basemodel," -C tables/ run504_no_cov1117171.rep", sep="")) # NONMEM control stream and report file

system(paste("tar -xvzf ",wd,"nmnqs/",fullcovmod," tables/paramsum1117172.tex -C tables", sep=""))
system(paste("tar -xvzf ",wd,"nmnqs/",fullcovmod," plots -C plots", sep=""))
system(paste("tar -xvzf ",wd,"nmnqs/",fullcovmod," -C tables/ run504_full_cov1117172.rep", sep="")) # NONMEM control stream and report file
system(paste("tar -xvzf ",wd,"nmnqs/",finalmodel," tables/paramsum1117173.tex -C tables", sep=""))
system(paste("tar -xvzf ",wd,"nmnqs/",finalmodel," plots -C plots", sep=""))
system(paste("tar -xvzf ",wd,"nmnqs/",finalmodel," -C tables/ run504_with_cov_fix1117173.rep", sep="")) # NONMEM control stream and report file


# ------------------------------------------
# R version and packages
#--------------------------------------------

printSessionInfo(subdir="tables")
    
# save R workspace------------------------------------------------------------
 
#save.image("postprocessing.RData")
