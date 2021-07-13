##******************************************************************
## Study R-script name : exploratory.R
## Description : data overview of NONMEM input dataset
## Platform : openSUSE Linux, version 15.1
## Original programmer : TF
## Current programmer / Modified by :TF 
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
wd <-  "/workfra/content/POHXXXX/analysis/"
setwd(wd)

# additional functions ----------------------------------------
source(".R/datalist.R")
source(".R/dataoverview.R")
source(".R/getN.R")
source(".R/ckeckdat.R")
source(".R/carryfwd.R")
source(".R/plotlist.R")
source(".R/appendList.R")
source(".R/makeFootnote.R")

PKdat <- getN("data_set_504.dat") # import NONMEM analysis dataset 
PKdat<-PKdat%>%
  dplyr::mutate(DOSE=case_when(AMT==1000 ~ 1, AMT==2000 ~ 2, AMT==4000 ~ 4), DOSE=carryfwd(DOSE), SEX= recode_factor(SEX, `0` = "female", `1` = "male"))

class_sum1<- PKdat%>%
  dplyr::select(ID, DV, EVID, MDV)

 # dplyr::summarise(n = n()) %>%
 # dplyr::mutate(rel.freq = round(100 * n/sum(n), 1))

dataoverview(class_sum1, filename = "OverviewPKSamples", variables = 'DV',group="MDV", caption= "Overview of PK samples (statistics in \\textmu g/mL)", label="tab:SumPKsamples")


dataoverview(PKdat[!duplicated(PKdat$ID),], filename = "SummaryContCovariates", variables = c('AGE','WT'), variable.labels = c('Age (years)','Weight (kg)'),caption= "Summary statistics of continuous covariates", label="tab:SumContCov") 

PKdat2<-PKdat%>%
  dplyr::select(ID, SEX,WT, AGE)%>%
  dplyr::filter(!duplicated(ID))

df<-melt(PKdat2, id.vars=c("ID","SEX"), measure.vars=c("WT", "AGE"),  variable_name = "covariate")

class_sum3<-df%>%
  dplyr::group_by( covariate, SEX) %>%
  dplyr::mutate(covariate= recode_factor(covariate, `AGE` = "Age(y)", `WT` = "Weight (kg)"))%>%
  dplyr::summarise(n=n(), mean = mean(value), sd=sd(value), min=min(value), q1=quantile(value, probs=0.25), median=median(value), q3=quantile(value, probs=0.75), max=max(value))

datalist(class_sum3,filename="SummaryContCovariatesSEX", digits=1,  variablelabels = c("Covariate", "Sex","N","Mean",'SD', "Min","Q1", 'Median',"Q3", "Max"), caption="Summary statistics of continuous covariates by sex", label="tab:SumContCovSEX")

dataoverview(PKdat[!duplicated(PKdat$ID),], filename = "SummaryCatCovariates", variables = 'SEX',type = "factor",caption= "Number and percentage of subjects by sex", label="tab:SumCatCov")

class_sum4<-PKdat%>% 
 dplyr::filter(EVID==0)%>%
  dplyr::group_by( DOSE, TIME) %>%
  dplyr::summarise(n = n(), q25 = quantile(DV, probs=0.025, na.rm=T), median = median(DV, na.rm=T), q975 = quantile(DV, probs=0.975, na.rm=T)) %>%
  dplyr::rename(DV=median)%>%
  dplyr::mutate(ID=1, WT=1)

p1 <- ggplot(data = PKdat[PKdat$EVID==0,], aes(TIME, DV, group = ID, colour=WT)) + geom_line()+geom_line(data=class_sum4, colour="black", size=1)+scale_x_continuous(name="Time (hours)")+scale_y_continuous(name="Concentration (µg/mL)")  +facet_wrap(~case_when(DOSE==1~"1 mg", DOSE==2~"2  mg",DOSE==4~"4 mg")) +  scale_colour_gradient(low = "salmon", high = "#56B1F7", na.value = NA, name="Weight (kg)") + theme_bw(12);p1
plotlist(list(p1), file="ctplotallone.lin.DOSE", width=8.5, height=5., subdir="plots")

p2 <- ggplot(data = PKdat[PKdat$EVID==0,], aes(TIME, DV, group = ID, colour=WT)) + geom_line()+geom_line(data=class_sum4, colour="black", size=1)+scale_x_continuous(name="Time (hours)")+scale_y_log10(name="Concentration (µg/mL)") + annotation_logticks(sides = "l") +facet_wrap(~case_when(DOSE==1~"1 mg", DOSE==2~"2  mg",DOSE==4~"4 mg")) +  scale_colour_gradient(low = "salmon", high = "#56B1F7", na.value = NA, name="Weight (kg)") + theme_bw(12);p2
plotlist(list(p2), file="ctplotallone.log.DOSE", width=8.5, height=5., subdir="plots")

# save R workspace------------------------------------------------------------
#save.image("exploratory.RData")
