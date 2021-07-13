#' @title Creates plots of observed, individual predicted and population predicted concentrations versus time.
#' @description The standard plots of observed, individual predicted and population predicted concentrations versus time are overlaid in one plot. Instead of concentrations any other dependent value can be displayed on the y axis.
#' @details A NONMEM dataset is always filtered for EVID=0, MDV=0 and CMT number.
#'
#' @param x a data.frame to be used.
#' @param num archive number.
#' @param conc character string identifying the dependent variable in the data.frame ("DV").
#' @param idv character string identifying the independent variable in the dataset to be plotted ("TAD").
#' @param cmt compartment number ("2").
#' @param lloq numerical value of the LLOQ of the bioanalytical method used ("-1000").
#' @param sim simulations used ("FALSE").
#' @param labl character string identifying the additional data records in the data.frame ("adl").
#' @param profile character string identifying the variable by which PK profiles should be grouped ("ID").
#' @param ylabel character string identifying label of the y axis ("concentration (ng/mL)").
#' @param xlabel character string identifying label of the x axis ("time after dose (h)").
#' @param xfactor factor defining scale limits (0.05 = 5\% of min and max values in the data)
#' @param logdata usage of log transformed data ("TRUE").
#' @param subdir writes output in the given subdirectory (optional).
#' @param addname additional file name specifier.
#' @param theme_set defaults to standard ggplot theme (grey background, white grid lines).
#' @param mycols character string identifying colors: blue,  black,  red, grey,green, violet. 'hash' replaces # in hex color code only to create the help file. It is automatically replaced with # within the function.
#' @param pagelayout character string identifying the page layout ("portrait"). Known values are "portrait" and "landscape".
#' @param zoomfactor factor for page width and height when chosing landscape layout ("0.8").
#' @param legend legend is off ("FALSE") or on ("TRUE").
#' @param size footnote size ("0.4").
#' @param ... Additional arguments. (Currently ignored.)
#'
#' @return Returns a pdf file.
#' @export
#'
#' @examples num=46892
#' ctpop(eval(parse(text=cmd)),num=num, conc="LCONC",profile=c("ID","VST"), sim=T, labl='ADDrecord',logdata=T, lloq=log(0.2), subdir= paste("nmntmp", num,"/plots",sep = ""))
ctpop <- function
(x,
num,
conc="DV",
idv="TAD",
cmt=2,
lloq=-1000,
sim=F,
labl='adl',
profile=c("ID"),
ylabel='concentration (ng/mL)',
xlabel='time after dose (h)',
xfactor = 0.05,
logdata=T ,
subdir="plots",
addname =NULL,
theme_set=theme_grey(12),
mycols=c(paste('hash','2591D9', sep=""), paste('hash','000000',sep=""), paste('hash','FF0000',sep=""),paste('hash','8E8D8D',sep=""), paste('hash','7CAE00',sep=""), paste('hash','C77CFF',sep="")),
pagelayout="portrait",
zoomfactor=0.8,
legend=F,
size=0.4,
...
){


 # x<- dat46505
dat<-x
mycols <- gsub("hash","#",mycols)

z=""
for(i in 1:length(profile)){
  y = dat[,profile[i]]
  z = paste(z,", ", profile[i],"=" ,y, sep="")
}
dat$PROFILE <- substring(as.character(z),2)

dat$conc <- dat[,conc]
dat$TAD <- dat[, idv]

if(logdata==T) {
  #if(conc!="CONC") {dat$conc[dat$conc>lloq] <- exp(dat$conc[dat$conc>lloq])}
  if(conc!="CONC") {dat$conc <- exp(dat$conc)}
  dat$PRED <- exp(dat$PRED )
  dat$IPRED <- exp(dat$IPRED )
}

#browser()
if(!("EVID" %in% colnames(dat)))
  dat$EVID <- rep(0, nrow(dat))
if(!("MDV" %in% colnames(dat)))
  dat$MDV <- rep(0, nrow(dat))
if(!("CMT" %in% colnames(dat)))
  dat$CMT <- rep(cmt, nrow(dat))

subdat <- dat[dat$EVID==0 & dat$MDV==0 & dat$CMT==cmt & dat$conc>lloq & !is.na(dat$conc),]
if(sim==T)  {subdat2 <- dat[(dat$EVID==0 & dat$MDV==0 & dat$CMT==cmt & dat$conc>lloq & !is.na(dat$conc)) | (dat$LABL==labl),] }

default_theme<-theme_set(theme_set)
pp1<-list()
pp2<-list()

  xmin <- floor(min(subdat$TAD[subdat[,conc]>lloq])- xfactor*min(subdat$TAD[subdat[,conc]>lloq]))
  xmax <- ceiling(max(subdat$TAD[subdat[,conc]>lloq])+ xfactor*max(subdat$TAD[subdat[,conc]>lloq]))
  if(sim==T) {
    xmin <- floor(min(subdat2$TAD)- xfactor*min(subdat2$TAD))
    xmax <- ceiling(max(subdat2$TAD)+ xfactor*max(subdat2$TAD))}

subdatmdv <- dat[dat$EVID==0 & dat$MDV==1 & dat$conc>lloq & !is.na(dat$conc)& dat$TAD>=xmin,]
#if(sim==T)  {subdatmdv <- dat[dat$EVID==0 & dat$MDV==1 & dat$conc>lloq & !is.na(dat$conc)& dat$TAD>=xmin & dat$LABL!=labl,]}

  ymin <- floor(min(c(subdat[,conc][subdat[,conc]>lloq],subdat$IPRED[subdat[,conc]>lloq],subdat$PRED[subdat[,conc]>lloq])))
  ymax <- ceiling(max(c(subdat[,conc][subdat[,conc]>lloq],subdat$IPRED[subdat[,conc]>lloq],subdat$PRED[subdat[,conc]>lloq])))

npages <- ceiling(length(unique(subdat$PROFILE[subdat$EVID==0]))/36)
for(j in 1:npages){
  idtmp <- unique(subdat$PROFILE[subdat$EVID==0])[(((j-1)*36)+1):((j-1)*36 + 36)]
  idtmp <- idtmp[!is.na(idtmp)]
  subtmp <- subdat[subdat$PROFILE %in%idtmp,]
  if(sim==T)  {subtmp3 <- subdat2[subdat2$PROFILE %in%idtmp,]} else {subtmp3 <- subtmp}
  subtmp2 <- subdatmdv[subdatmdv$PROFILE %in%idtmp,]

 ## linear scale
  p1<-ggplot(subtmp3)+
      geom_point(data=subtmp, shape=1,aes(TAD, conc, colour="observation"), size=1.3)+
      geom_line(data=subtmp3, aes(TAD, PRED, colour="population prediction"))+
      geom_line(data=subtmp3, aes(TAD, IPRED, colour="individual prediction")) +
      guides(color=guide_legend(override.aes = list(shape=c(NA, 1, NA), linetype=c(1, 0, 1))))+
      xlab(xlabel) +
      ylab(ylabel) +
      xlim(xmin,xmax) +
      #ylim(ymin,ymax) +
      facet_wrap(~PROFILE, ncol=6)+
      scale_colour_manual(name = "", values = mycols)
  if(legend==F){
      p1 <- p1 + theme(legend.position="none")
  } else{
    p1 <- p1 + theme(legend.position="top")
  }

 # if(nrow(subtmp2)!=0){  p1 <- p1 + geom_point(data=(subtmp2), aes(TAD, conc, colour="z_mdv"),shape=4)}
  if(j!=npages) pp1[[j]]<-p1

  ## log scale

  p2<-ggplot(subtmp3)+
      geom_point(data=subtmp, aes(TAD, conc, colour="observation"), shape=1, size=1.3)+
      geom_line(data=subtmp3, aes(TAD, PRED, colour="population prediction"))+
      geom_line(data=subtmp3, aes(TAD, IPRED, colour="individual prediction")) +
      guides(color=guide_legend(override.aes = list(shape=c(NA, 1, NA), linetype=c(1, 0, 1))))+
      xlab(xlabel) +
      ylab(ylabel) +
      xlim(xmin,xmax) +
      facet_wrap(~PROFILE,  ncol=6, drop=F)+
      scale_colour_manual(name = "", values = mycols) +
      scale_y_log10() +
      annotation_logticks(sides = "l")
  if(legend==F){
    p2 <- p2 + theme(legend.position="none")
  } else{
    p2 <- p2 + theme(legend.position="top")
  }

 # if(nrow(subtmp2)!=0){  p2 <- p2 + geom_point(data=subtmp2, aes(TAD, conc, colour="z_mdv"),shape=4)}
  if(j!=npages) pp2[[j]]<-p2
  # print(j)
}

if (substring(subdir,1,6) =="nmntmp"){
  WD <- getwd()
  apath = paste(WD, "/nmnqs", sep = "")
  cmd = paste("cd ", apath, "; basename *.", num, ".tar.gz", sep = "")
  tarname = system(cmd, intern = TRUE)
  finalwd = paste("nmnqs/",tarname,"; ", sep="")}else {finalwd=paste("/",subdir,"/",sep="")}


filein1=paste("ctpop.linear1.",profile,".",num, addname, sep = "")
filein2=paste("ctpop.linear2.",profile,".",num, addname, sep = "")
filein=paste("ctpop.linear.",profile,".",num, addname, sep = "")
footnoteText = paste(Sys.getenv("USER"), "; ", getwd(), "/", finalwd, filein, ".pdf; ", format(Sys.time(), "%d %b %Y %T"), sep="")

if(pagelayout=="portrait"){
if(npages>1){
  #create pdf files
  plotlist(pp1 , file=filein1,width = 8, height = 11, subdir=subdir,footnoteText =footnoteText, size=size)
  rows=ceiling(length(idtmp[!is.na(idtmp)])/6)
  plotlist(list(p1), file=filein2,width = 8, height = (11*rows/6), subdir=subdir,footnoteText =footnoteText,   size=size)

  #combine pdf files
  cmd = paste("pdftk", paste(subdir, "/", filein1, ".pdf",sep = ""), paste(subdir, "/", filein2, ".pdf",sep = ""), "cat output", paste(subdir, "/", filein, ".pdf",sep = ""))
  system(cmd,intern=T)
  #remove files
  cmd2 = paste("rm", paste(subdir, "/", filein1, ".pdf",sep = ""))
  system(cmd2, intern=T)
  cmd3 = paste("rm", paste(subdir, "/", filein2, ".pdf",sep = ""))
  system(cmd3, intern=T)}else{

    rows=ceiling(length(idtmp[!is.na(idtmp)])/6)
    plotlist(list(p1), file=filein ,width = 8, height = (11*rows/6), subdir=subdir,footnoteText =footnoteText, size=size)}


filein1=paste("ctpop.log1.",profile,".",num, addname, sep = "")
filein2=paste("ctpop.log2.",profile,".",num, addname, sep = "")
filein=paste("ctpop.log.",profile,".",num, addname, sep = "")
footnoteText = paste(Sys.getenv("USER"), "; ", getwd(), "/", finalwd, filein, ".pdf; ", format(Sys.time(), "%d %b %Y %T"), sep="")

if(npages>1){
  #create pdf files
  plotlist(pp2, file=filein1,width = 8, height = 11, subdir=subdir,footnoteText =footnoteText, size=size)
  rows=ceiling(length(idtmp[!is.na(idtmp)])/6)
  plotlist(list(p2), file=filein2,width = 8, height = (11*rows/6), subdir=subdir,footnoteText =footnoteText, size=size)

  #combine pdf files
  cmd = paste("pdftk", paste(subdir, "/", filein1, ".pdf",sep = ""), paste(subdir, "/", filein2, ".pdf",sep = ""), "cat output", paste(subdir, "/", filein, ".pdf",sep = ""))
  system(cmd,intern=T)
  #remove pdf files
  cmd2 = paste("rm", paste(subdir, "/", filein1, ".pdf",sep = ""))
  system(cmd2, intern=T)
  cmd3 = paste("rm", paste(subdir, "/", filein2, ".pdf",sep = ""))
  system(cmd3, intern=T)}else{

    plotlist(list(p2), file=filein,width = 8, height = (11*rows/6), subdir=subdir, footnoteText =footnoteText, size=size)}
}

if(pagelayout=="landscape"){
if(npages>1){
  #create pdf files
  plotlist(pp1 , file=filein1, width = 11*zoomfactor, height = 8*zoomfactor, subdir=subdir,footnoteText =footnoteText, size=size)
  rows=ceiling(length(idtmp[!is.na(idtmp)])/6)
  plotlist(list(p1), file=filein2,width = 11, height = (8*rows/6), subdir=subdir,footnoteText =footnoteText,   size=size)

  #combine pdf files
  cmd = paste("pdftk", paste(subdir, "/", filein1, ".pdf",sep = ""), paste(subdir, "/", filein2, ".pdf",sep = ""), "cat output", paste(subdir, "/", filein, ".pdf",sep = ""))
  system(cmd,intern=T)
  #remove files
  cmd2 = paste("rm", paste(subdir, "/", filein1, ".pdf",sep = ""))
  system(cmd2, intern=T)
  cmd3 = paste("rm", paste(subdir, "/", filein2, ".pdf",sep = ""))
  system(cmd3, intern=T)}else{

    rows=ceiling(length(idtmp[!is.na(idtmp)])/6)
    plotlist(list(p1), file=filein ,width = 11, height = (8*rows/6), subdir=subdir,footnoteText =footnoteText, size=size)}


filein1=paste("ctpop.log1.",profile,num, addname, sep = "")
filein2=paste("ctpop.log2.",profile,num, addname, sep = "")
filein=paste("ctpop.log.",profile,num, addname, sep = "")
footnoteText = paste(Sys.getenv("USER"), "; ", getwd(), "/", finalwd, filein, ".pdf; ", format(Sys.time(), "%d %b %Y %T"), sep="")

if(npages>1){
  #create pdf files
  plotlist(pp2, file=filein1,width = 11*zoomfactor, height = 8*zoomfactor, subdir=subdir,footnoteText =footnoteText, size=size)
  rows=ceiling(length(idtmp[!is.na(idtmp)])/6)
  plotlist(list(p2), file=filein2,width = 11, height = (8*rows/6), subdir=subdir,footnoteText =footnoteText, size=size)

  #combine pdf files
  cmd = paste("pdftk", paste(subdir, "/", filein1, ".pdf",sep = ""), paste(subdir, "/", filein2, ".pdf",sep = ""), "cat output", paste(subdir, "/", filein, ".pdf",sep = ""))
  system(cmd,intern=T)
  #remove pdf files
  cmd2 = paste("rm", paste(subdir, "/", filein1, ".pdf",sep = ""))
  system(cmd2, intern=T)
  cmd3 = paste("rm", paste(subdir, "/", filein2, ".pdf",sep = ""))
  system(cmd3, intern=T)}else{

    plotlist(list(p2), file=filein,width = 11, height = (8*rows/6), subdir=subdir, footnoteText =footnoteText, size=size)}
}

}

# end ctpop()

