#' @title Creates a series of goodness-of-fit plots
#' @description The goodness-of fit plots comprise PREDV/IPRED vs. DV, PREDV vs. CWRES, CWRES vs. time after dose, Q-Q plots of CWRES, and IWRES vs. IPRED. Printed as panels on one single page.
#' @details A NONMEM dataset is always filtered for EVID=0, MDV=0 and CMT number.
#'
#' @param x a data.frame to be used.
#' @param num archive number.
#' @param conc character string identifying the dependent variable in the data.frame ("DV").
#' @param time character string identifying the independent variable in the dataset to be plotted ("TAD").
#' @param cmt compartment number ("2").
#' @param lloq numerical value of the LLOQ of the bioanalytical method used ("-1000").
#' @param label character string identifying the dependent variable label ("concentration").
#' @param timelabel character string identifying label of the x axis, time scale ("time after dose (h)").
#' @param logdata usage of log transformed data ("TRUE").
#' @param addname additional file name specifier.
#' @param smooth add smoothing lines to the plots? ("FALSE").
#' @param method smoothing method (function) to use, eg, lm, glm, gam, loess, rlm. Default is "auto" (automatic selection of smoothing method), i.e., for datasets with n < 1000 default is loess. For datasets with 1000 or more observations defaults to gam, see gam for more details.
#' @param span Controls the amount of smoothing for the loess smoother. Smaller numbers produce wigglier lines, larger numbers produce smoother lines (ggplot default=2/3).
#' @param se.set  display confidence interval around smooth? ("TRUE").
#' @param show.wres numerical value for the CWRES treshold to identify datapoints by ID ("5").
#' @param show.dv numerical value for the treshold to identify datapoints by ID ("5").
#' @param subdir writes output in the given subdirectory (optional).
#' @param theme_set defaults to standard ggplot theme (grey background, white grid lines)
#' @param alpha.bw transparency value for all points (range between 0 (totally transparent) and 1 (totally intransparent), "0.2").
#' @param size footnote size ("0.4").
#' @param point.size size of plotted points ("2").
#' @param offsetlog adjustment of x/y axis labels on log scale ("1").
#' @param offsetlin adjustment of x/y axis labels on linear scale ("1").
#' @param ... Additional arguments. (Currently ignored.)
#'
#' @return Returns a pdf file.
#' @export
#'
#' @examples num=46892
#' @examples cmd=paste("dat", num, sep = "")
#' @examples gofplots(eval(parse(text=cmd)),num=num,conc="CONC",logdata=T,logplot=T, show.dv = 100,show.wres=5, subdir= paste("nmntmp", num,"/plots",sep = "") )
gofplots <- function
(x,
num,
conc="DV",
time="TAD",
cmt=2,
lloq=-1000,
label='concentration (ng/L)',
timelabel='time after dose (h)',
logdata=T,
addname =NULL,
smooth=F,
method="auto",
span=2/3,
se.set=T,
show.wres=5,
show.dv=5,
subdir="plots",
theme_set=theme_grey(12),
alpha.bw=0.5,
size=0.4,
point.size=2,
offsetlog=1.02,
offsetlin=1.0,
...
){

#formatting parameters
point.shape=1
binwidth=0.5
line.size=0.4
#point.size=1
linetype="twodash"
line.col="darkgrey"
text.col="black"
text.size=3

#subset data
dat <- x

if(!("EVID" %in% colnames(dat)))
  dat$EVID <- rep(0, nrow(dat))
if(!("MDV" %in% colnames(dat)))
  dat$MDV <- rep(0, nrow(dat))
if(!("CMT" %in% colnames(dat)))
  dat$CMT <- rep(cmt, nrow(dat))

if(!("PREDV" %in% names(dat))){
  names(dat)[names(dat)=="PRED"] <- "PREDV"
}

subdat <- dat[dat$EVID==0 & dat$MDV==0 & dat$CMT==cmt & dat[,conc]>lloq,]
subdat$conc <- subdat[,conc]
subdat$TAD <- subdat[, time]

#back log transform data
if(logdata==T){
  if(conc!="CONC") {
    subdat$conc <- exp(subdat$conc)
    subdat[,conc] <- exp(subdat[,conc])
  }
  subdat$PREDV <- exp(subdat$PREDV )
  subdat$IPRED<- exp(subdat$IPRED )
}

#define axis
YMAX<-max(c(subdat$conc,subdat$PREDV,subdat$IPRED))
YMIN<-min(c(subdat$conc,subdat$PREDV,subdat$IPRED))

if("CWRES" %in% colnames(subdat)){YMAXC<-max(abs(subdat$CWRES))}
if("IWRES" %in% colnames(subdat)){YMAXI<-max(abs(subdat$IWRES))}


## plots
default_theme<-theme_set(theme_set)
pp<-list()

subdat$conc <- subdat[!is.na(subdat[,conc]),conc]

p1<-ggplot(subdat, aes(PREDV, conc)) +
  geom_point(shape=point.shape,size=point.size,alpha=alpha.bw) +
  scale_x_continuous(limits=c(YMIN,YMAX*offsetlin)) +
  scale_y_continuous(limits=c(YMIN,YMAX*offsetlin)) +
  xlab(paste('population predicted ', label, sep='')) +
  ylab(paste('observed ', label,  sep=''))+
  geom_abline(size = line.size, linetype=linetype, colour=line.col)
  #geom_text(aes(label=ID),data=subset(subdat, abs(conc/PREDV)>show.dv | abs(conc/PREDV)<1/show.dv ),hjust=-0.5,vjust=0.5,angle=0,color=text.col,size=text.size)
if(smooth == TRUE){
    p1 <- p1 + stat_smooth(se=se.set, method=method, span=span)
}
pp[[1]]<-p1

p2<-ggplot(subdat, aes(IPRED, conc)) +
  geom_point(shape=point.shape,size=point.size,alpha=alpha.bw) +
  scale_x_continuous(limits=c(YMIN,YMAX*offsetlin)) +
  scale_y_continuous(limits=c(YMIN,YMAX*offsetlin)) +
  xlab(paste('individual predicted ', label,  sep='')) +
  ylab(paste('observed ', label,  sep='')) +
  geom_abline(size = line.size, linetype=linetype, colour=line.col)
  #geom_text(aes(label=ID),data=subset(subdat, abs(conc/IPRED)>show.dv | abs(conc/IPRED)<1/show.dv),hjust=-0.5,vjust=0.5,angle=0,color=text.col,size=text.size)
if(smooth == TRUE){
  p2 <- p2 + stat_smooth(se=se.set, method=method, span=span)
}
pp[[2]]<-p2

p1a<-ggplot(subdat, aes(PREDV, conc)) +
  geom_point(shape=point.shape,size=point.size,alpha=alpha.bw) +
  scale_x_log10(limits=c(YMIN,YMAX*offsetlog)) +
  scale_y_log10(limits=c(YMIN,YMAX*offsetlog)) +
  annotation_logticks(scaled = TRUE, sides ="bl") +
  xlab(paste('population predicted ', label, sep='')) +
  ylab(paste('observed ', label, sep=''))+
  geom_abline(size = line.size, linetype=linetype, colour=line.col)
  # geom_text(aes(label=ID),data=subset(subdat, abs(conc/PREDV)>show.dv | abs(conc/PREDV)<1/show.dv),hjust=-0.5,vjust=0.5,angle=0,color=text.col,size=text.size)
if(smooth == TRUE){
  p1a <- p1a + stat_smooth(se=se.set, method=method, span=span)
}

pp[[3]]<-p1a

p2a<-ggplot(subdat, aes(IPRED, conc)) +
  geom_point(shape=point.shape,size=point.size,alpha=alpha.bw) +
  scale_x_log10(limits=c(YMIN,YMAX*offsetlog)) +
  scale_y_log10(limits=c(YMIN,YMAX*offsetlog)) +
  annotation_logticks(scaled = TRUE, sides = "bl") +
  xlab(paste('individual predicted ', label, sep='')) +
  ylab(paste('observed ', label, sep='')) +
  geom_abline(size = line.size, linetype=linetype, colour=line.col)
  #geom_text(aes(label=ID),data=subset(subdat, abs(conc/IPRED)>show.dv | abs(conc/IPRED)<1/show.dv ),hjust=-0.5,vjust=0.5,angle=0,color=text.col,size=text.size)
if(smooth == TRUE){
  p2a <- p2a + stat_smooth(se=se.set, method=method, span=span)
}
pp[[4]]<-p2a


if("IWRES" %in% colnames(subdat) & "CWRES" %in% colnames(subdat)){


  p3<-ggplot(subdat, aes(PREDV, CWRES,label=ID)) +
    geom_point(shape=point.shape,size=point.size,alpha=alpha.bw)+
    scale_y_continuous(limits=c(-YMAXC,YMAXC)) +
    scale_x_continuous(limits=c(YMIN,YMAX*offsetlin))+
    geom_hline(yintercept=0, size = line.size,linetype=linetype,colour=line.col) +
    geom_text(data=subset(subdat, abs(CWRES)>show.wres),hjust=-0.5,vjust=0.5,angle=0,color=text.col,size=text.size)+
    labs(x=paste('population predicted ', label, sep=''), y="conditional weighted residuals")
  if(smooth == TRUE){
    p3 <- p3 + stat_smooth(se=se.set, method=method, span=span)
  }
  pp[[5]]<-p3

  p4<-ggplot(subdat, aes(TAD, CWRES,label=ID))+
    geom_point(shape=point.shape,size=point.size,alpha=alpha.bw)+
    scale_y_continuous(limits=c(-YMAXC,YMAXC)) +
    #scale_x_continuous(limits=c(YMIN,YMAX*offsetlin))+
    geom_hline(yintercept=0,size = line.size,linetype=linetype,colour=line.col)+
    geom_text(aes(label=ID), subset(subdat, abs(CWRES)>show.wres),hjust=-0.5,vjust=0.5,angle=0,color=text.col,size=text.size)+
    labs(x=timelabel, y="conditional weighted residuals")
  if(smooth == TRUE){
    p4 <- p4 + stat_smooth(se=se.set, method=method, span=span)
  }
  pp[[6]]<-p4

  p5<-ggplot(subdat, aes(sample = CWRES)) +
    stat_qq(shape=point.shape,size=point.size,alpha=alpha.bw)+
    geom_abline(size=line.size,linetype=linetype,colour=line.col) +
    labs(x="theoretical quantiles",y="conditional weighted residuals quantiles")
  pp[[7]]<-p5

  p6<-ggplot(subdat, aes(IPRED, IWRES)) +
    geom_point(shape=point.shape,size=point.size,alpha=alpha.bw) +
    scale_y_continuous(limits=c(-YMAXI,YMAXI)) +
  scale_x_continuous(limits=c(YMIN,YMAX*offsetlin))+
    geom_hline(yintercept=0,size = line.size,linetype=linetype,colour=line.col) +
    geom_text(aes(label=ID), subset(subdat, abs(IWRES)>show.wres),hjust=-0.5,vjust=0.5,angle=0,color=text.col,size=text.size)+
    labs(x=paste('individual predicted ', label, sep=''), y="individual weighted residuals")
  if(smooth == TRUE){
    p6 <- p6 + stat_smooth(se=se.set, method=method, span=span)
  }
  pp[[8]]<-p6
}

if("CWRES" %in% colnames(subdat) & !("IWRES" %in% colnames(subdat))){

  p3<-ggplot(subdat, aes(PREDV, CWRES,label=ID)) +
    geom_point(shape=point.shape,size=point.size,alpha=alpha.bw)+
    scale_y_continuous(limits=c(-YMAXC,YMAXC)) +
scale_x_continuous(limits=c(YMIN,YMAX*offsetlin))+
    geom_hline(yintercept=0, size = line.size,linetype=linetype,colour=line.col) +
    geom_text(data=subset(subdat, abs(CWRES)>show.wres),hjust=-0.5,vjust=0.5,angle=0,color=text.col,size=text.size)+
    labs(x=paste('population predicted ', label, sep=''), y="conditional weighted residuals")
  if(smooth == TRUE){
    p3 <- p3 + stat_smooth(se=se.set, method=method, span=span)
  }
  pp[[5]]<-p3

  p4<-ggplot(subdat, aes(TAD, CWRES,label=ID))+
    geom_point(shape=point.shape,size=point.size,alpha=alpha.bw)+
    scale_y_continuous(limits=c(-YMAXC,YMAXC)) +
#scale_x_continuous(limits=c(YMIN,YMAX*offsetlin))+
    geom_hline(yintercept=0,size = line.size,linetype=linetype,colour=line.col)+
    geom_text(aes(label=ID), subset(subdat, abs(CWRES)>show.wres),hjust=-0.5,vjust=0.5,angle=0,color=text.col,size=text.size)+
    labs(x=timelabel, y="conditional weighted residuals")
  if(smooth == TRUE){
    p4 <- p4 + stat_smooth(se=se.set, method=method, span=span)
  }
  pp[[6]]<-p4

  p5<-ggplot(subdat, aes(sample = CWRES)) +
    stat_qq(shape=point.shape,size=point.size,alpha=alpha.bw)+
    geom_abline(size=line.size,linetype=linetype,colour=line.col) +
    labs(x="theoretical quantiles",y="conditional weighted residuals quantiles")
  pp[[7]]<-p5
}


if(!("CWRES" %in% colnames(subdat)) & "IWRES" %in% colnames(subdat)){
  p6<-ggplot(subdat, aes(IPRED, IWRES)) +
    geom_point(shape=point.shape,size=point.size,alpha=alpha.bw) +
    scale_y_continuous(limits=c(-YMAXI,YMAXI)) +
scale_x_continuous(limits=c(YMIN,YMAX*offsetlin))+
    geom_hline(yintercept=0,size = line.size,linetype=linetype,colour=line.col) +
    geom_text(aes(label=ID), subset(subdat, abs(IWRES)>show.wres),hjust=-0.5,vjust=0.5,angle=0,color=text.col,size=text.size)+
    labs(x=paste('individual predicted ', label, sep=''), y="individual weighted residuals")
  if(smooth == TRUE){
    p6 <- p6 + stat_smooth(se=se.set, method=method, span=span)
  }
  pp[[8]]<-p6
}

ppp <- pp[1:4]

pppp <-pp[5:8]


filename = paste(subdir, "/", "gofA",addname,".",num, ".pdf", sep = "")
pdf(file=filename, width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
layout = matrix(c(1:4),nrow=2,byrow=T)
for (i in 1:length(ppp)) {
  # Get the i,j matrix positions of the regions that contain this subplot
  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

  print(ppp[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                  layout.pos.col = matchidx$col))}
       filefin = paste(num, ".1",".pdf", sep = "")

if (substring(subdir,1,6) =="nmntmp"){
  WD <- getwd()
  apath = paste(WD, "/nmnqs", sep = "")
  cmd = paste("cd ", apath, "; basename *.", num, ".tar.gz", sep = "")
  tarname = system(cmd, intern = TRUE)
  sub = substring(subdir,regexpr("/",subdir,perl=T)[1]+1, nchar(subdir))
  finalwd = paste(WD, tarname, sub ,sep="/")
  filename = paste("gof",addname,".",num, ".pdf", sep = "")
  makeFootnote(footnoteText = paste(Sys.getenv("USER"),"; ", finalwd, "/",filename, ";", format(Sys.Date()), sep = ""), size=size) }else{
  makeFootnote(footnoteText = paste(Sys.getenv("USER"),"; ", getwd(), "/",filename, ";", format(Sys.Date()), sep = ""), size=size)
  }

dev.off()

  filename = paste(subdir, "/", "gofB",addname,".",num, ".pdf", sep = "")
pdf(file=filename, width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
layout = matrix(c(1:4),nrow=2,byrow=T)

for (i in 1:length(pppp)) {
  # Get the i,j matrix positions of the regions that contain this subplot
  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

  print(pppp[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                  layout.pos.col = matchidx$col))}
       filefin = paste(num, ".1",".pdf", sep = "")

if (substring(subdir,1,6) =="nmntmp"){
  WD <- getwd()
  apath = paste(WD, "/nmnqs", sep = "")
  cmd = paste("cd ", apath, "; basename *.", num, ".tar.gz", sep = "")
  tarname = system(cmd, intern = TRUE)
  sub = substring(subdir,regexpr("/",subdir,perl=T)[1]+1, nchar(subdir))
  finalwd = paste(WD, tarname, sub ,sep="/")
  filename = paste("gof",addname,".",num, ".pdf", sep = "")
  makeFootnote(footnoteText = paste(Sys.getenv("USER"),"; ", finalwd, "/",filename, ";", format(Sys.Date()), sep = ""), size=size) }else{
  makeFootnote(footnoteText = paste(Sys.getenv("USER"),"; ", getwd(), "/",filename, ";", format(Sys.Date()), sep = ""), size=size)
  }

dev.off()

}

# end gofplots()
