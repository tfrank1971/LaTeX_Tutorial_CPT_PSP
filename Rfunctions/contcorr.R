#' @title Creates a scatterplot matrix.
#' @description The scatterplot matrix shows correlations among continuous covariates or random effects (ETAs).
#' @details A NONMEM dataset is always filtered for EVID=0 and MDV=0.
#'
#' @param x a data.frame to be used.
#' @param num archive number.
#' @param cov character string identifying the ETA and the continuous covariates ('ETACL','AGE', 'WT', 'HT', 'BMI').
#' @param covlabels character string to the labels of the continuous covariates ("NULL").
#' @param subset character string indentifying the splitting variable of the dataset ("ID").
#' @param addname additional file name specifier.
#' @param pagelayout character string identifying the page layout ("portrait"). Known values are "portrait" and "landscape".
#' @param method smoothing method (function) to use, eg, lm, glm, gam, loess, rlm. Default is "auto" (automatic selection of smoothing method), i.e., for datasets with n < 1000 default is loess. For datasets with 1000 or more observations defaults to gam, see gam for more details.
#' @param span Controls the amount of smoothing for the loess smoother. Smaller numbers produce wigglier lines, larger numbers produce smoother lines (ggplot default=2/3).
#' @param se.set display confidence interval around smooth? (TRUE by default).
#' @param point.shape shape of the point (for more details see point shapes of ggplot, "1").
#' @param point.size diameter of the point ("2").
#' @param alpha.bw transparency value for all points (range between 0 (totally transparent) and 1 (totally intransparent), "0.2").
#' @param size.cor font size of the correlation coefficients ("5").
#' @param binwidth The width of the bins of the histogram instead of the correlation coefficients, if you have a combination of continuous and discrete variables ("1").
#' @param cor.low correlation coefficients are plotted in the lower part of the scatterplot matrix ("TRUE") or in the upper part ("FALSE").
#' @param cardinality_threshold maximum number of levels allowed in a character / factor column. Set this value to NULL to not check factor columns. Defaults to 15
#' @param theme_set defaults to standard ggplot theme (grey background, white grid lines)
#' @param subdir writes output in the given subdirectory (optional).
#' @param size footnote size ("0.4").
#' @param ... Additional arguments. (Currently ignored.)
#'
#' @return Returns a pdf file.
#' @export
#'
contcorr <- function
(x,
num,
cov =c('ETACL','AGE', 'WT', 'HT', 'BMI'),
covlabels=NULL,
subset=c("ID"),
addname =NULL,
pagelayout="portrait",
method="auto",
span=2/3,
se.set=T,
point.shape=1,
point.size=2,
alpha.bw=0.2,
size.cor=5,
binwidth=1,
cor.low=T,
cardinality_threshold=15,
theme_set=theme_grey(12),
subdir="plots",
size=0.4,
...
){

dat <- x

if(!("EVID" %in% colnames(dat)))
  dat$EVID <- rep(0, nrow(dat))
if(!("MDV" %in% colnames(dat)))
  dat$MDV <- rep(0, nrow(dat))


dat <-  dat[dat$EVID==0 & dat$MDV==0,]

cmd=""
for(i in 1:length(subset)){
 if(i==1){cmd =paste("paste(dat[,'",subset[i],"']",sep="") } else {
   if (i!=1 & i!=length(subset)) {cmd =paste(cmd,",dat[,'",subset[i],"']",sep="")} else {
     cmd =paste(cmd,",dat[,'",subset[i],"'])",sep="")}}
}
if (length(subset)==1) {cmd=paste(cmd,")",sep='')}

dat <- dat[!duplicated(eval(parse(text=cmd))),]


my_fn <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(shape=point.shape,size=point.size,alpha=alpha.bw) +
    geom_smooth(method=method, ...)
  p
}

if(is.null(covlabels)){covlabels <- cov}else{covlabels <- covlabels}

default_theme<-theme_set(theme_set)

if(cor.low==T){
  p1 <-GGally::ggpairs(dat,
        columns= cov,
        columnLabels =covlabels,
        axisLabels="show",
        cardinality_threshold=cardinality_threshold,
        upper = list(continuous = GGally::wrap(my_fn, se=se.set, method=method,span=span)),
        lower=list(continuous = GGally::wrap("cor",size=size.cor, col="gray50"), combo = GGally::wrap("facethist", binwidth = binwidth)))
}else{
  p1 <-GGally::ggpairs(dat,
               columns= cov,
               columnLabels =covlabels,
               axisLabels="show",
               cardinality_threshold=cardinality_threshold,
               upper = list(continuous = GGally::wrap("cor",size=size.cor, col="gray50"), combo = GGally::wrap("facethist", binwidth = binwidth)),
               lower= list(continuous = GGally::wrap(my_fn, se=se.set, method=method,span=span), combo = GGally::wrap("box")))
}


if (substring(subdir,1,6) =="nmntmp"){
  WD <- getwd()
  apath = paste(WD, "/nmnqs", sep = "")
  cmd = paste("cd ", apath, "; basename *.", num, ".tar.gz", sep = "")
  tarname = system(cmd, intern = TRUE)
  finalwd = paste("nmnqs/",tarname,"; ", sep="")}else {finalwd=paste("/",subdir,"/",sep="")}


filein=paste("contcorr",addname,".",num, sep = "")
footnoteText = paste(Sys.getenv("USER"), "; ", getwd(), "/", finalwd, filein, ".pdf; ", format(Sys.time(), "%d %b %Y %T"), sep="")

cols=length(cov)/9

if(pagelayout=="portrait"){

plotlist(list(p1), file=filein ,width = 1.8*8*cols, height = (1.8*11*cols),subdir=subdir,footnoteText =footnoteText, size=size)

}

if(pagelayout=="landscape"){

plotlist(list(p1), file=filein ,width = 1.8*11*cols, height = (1.8*8*cols),subdir=subdir,footnoteText =footnoteText, size=size)

}

}
# end contcorr()
