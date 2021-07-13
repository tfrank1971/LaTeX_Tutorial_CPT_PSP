##******************************************************************
## Study R-script name : standard.plots.R
## Description : Creates GoF plots and summary tables for NONMEM runs
##  the tar.gz file containing NONMEM results is extracted in a 
##  temporary directory, plots and tables are created and added
##  to the tar.gz file
## Platform : openSUSE Linux, version 15.1
## Original programmer : AS
## Current programmer / Modified by : TF,SG 
## Date completed : Apr 2021
## R version : R version 3.6.1
## NONMEM version : 7.4.1
##***********************************************************

standard.plots <- function(num, path, del=T, subdir, base_model=F
                           #, digits=2, ym=0.45, xm=1.8, 
                           #nofactor = c("ID", "DV", "TIME", "TVCL", "TVV", "CL", "V"), cor.lowETA=F, cor.lowCOV=T, 
                           #thetaVariableNames=c("TVCL (mL/h)", "TVV (L)", "CLBWT", "VBWT")
                           ) {


wd <- path
  setwd(wd) ## set working directory, nmanalysis folder where NM runs were performed from
num=num
#unzip file
unziparch(num) #unzip archive into a tmp folder, create subfolders, save all plots in tmp folder


getnmr7(num,  xposeread=T, data=F, RM=T, check=T, shrinkage = T) ## input run number from archive (Xpose step)
extractdata(num)

## use temporary data object
mydat = paste("dat",num,sep='')
cmd = mydat
tmp<-eval(parse(text=cmd))

# create table with model outputs
printnmr7(eval(parse(text=paste('fit',num,sep=''))) , digits=2, formatted=F, path=paste(wd,"nmntmp", num,"/tables",sep = "")) 

# parameter summary table
paramsum(eval(parse(text=paste('fit',num,sep=''))), path=paste(wd,"nmntmp", num,"/tables",sep = ""), table.placement = "!htpb", label = paste("tab:paramsum",num, sep=""))#, thetaVariableNames=thetaVariableNames)

# gofplots A (DV vs IPRED and PRED) and B (CWRES)
gofplots(tmp,num=num,conc="DV",time="TIME", timelabel="time (hours)", logdata=F, cmt=1, label = "concentration (µg/mL)", 
         lloq=0, show.dv = 2, smooth=T, addname =NULL,subdir= paste("nmntmp", num,"/plots",sep = ""), theme_set = theme_bw()  + 
           theme(legend.position = "bottom", legend.box = "horizontal", strip.background = element_rect(fill = "#e6ffe6", linetype = "dashed"), strip.text = element_text(size = 12), text = element_text(size = 12), plot.caption = element_text(size = 10, colour = "darkgrey")), alpha.bw = 0.5)

# individual plots of observations, together with individual, and population predictions
ctpop(tmp,num=num, conc="DV", idv="TIME", xlabel="Time (hours)", ylabel="Concentration (µg/mL)", profile=c('ID'), logdata=F,  addname=NULL, theme_set = theme_bw(10),subdir= paste("nmntmp", num,"/plots",sep = ""))


# npde plots
npdeplot(tmp, num=num,addname =NULL, time="TIME", timelabel = "time (hours)", predlabel="population predicted concentration (µg/mL)",log=F , alpha.bw=0.5, npde=5,ym = 0.45, theme_set = theme_bw(12), subdir=paste("nmntmp", num,"/plots",sep = ""))


# plot correlation on eta vs covariates and eta vs eta

contcorr(tmp, num=num, cov = c("ETA1","ETA2", "WT","SEX","AGE"), covlabels = c("ETA CL", "ETA V","WT","SEX","AGE"), addname = 'ETA', alpha.bw = 0.5,   theme_set = theme_bw(10),subdir = paste("nmntmp", num,"/plots",sep = ""))


ziparch(num, del=T) #zip tmp folder

## for base model we want a comparison with other relevant models from an extra comparison folder
if(base_model ==T ) {
wd_comp<-paste(".",subdir,sep="/")
setwd(wd_comp)
comparison<<-compnmr7(merging=T, OBJref=num) # run comparison

baseModelsubset<-subset(comparison, select=c("ARCHIVE","PROBLEM","nPSE","OBJ", "OBJdiff", "ETCL:ETCL", "ETV:ETV"))%>%
  dplyr::rename(ETCL="ETCL:ETCL", ETV = "ETV:ETV")%>%
  dplyr::mutate(ETCL=sqrt(ETCL), ETV=sqrt(ETV))
write.csv(baseModelsubset, file="modelcomp.csv")
datalist(baseModelsubset, filename="modelcomp", subdir=subdir, path=path, caption="Comparison of models", label="tab:comparison",  digits=c(0,0,0,1,1,1, 3, 3))
setwd(wd)}
}
