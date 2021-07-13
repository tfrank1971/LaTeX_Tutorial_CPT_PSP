library(npde)


#' @title Provides diagnostic plots for NPDE (normalised prediction distribution error)
#' @description The function creates the following graphs: (i) a Q-Q-plot of the distribution of the NPDE versus theoretical \eqn{N}(0,1) distribution, (ii) a histogram of the NPDE, (iii) NPDE versus population predicted concentrations, and (iv) NPDE versus time after dose. Statistical tests are provided to evaluate significant differences from the theoretical \eqn{N}(0,1) distribution. \strong{Needs variables PRED, NPDE and TAD (or another time variable) in the dataset in order to work correctly!}
#' @details A NONMEM dataset is always filtered for EVID=0 and MDV=0.
#'
#' @param x a data.frame to be used.
#' @param num archive number.
#' @param log log transformation to be used ("FALSE")
#' @param lloq M3 method used in NONMEM to treat LLOQ ("FALSE")
#' @param addname additional file name specifier.
#' @param subdir writes plot (pdf file) in the given subdirectory (optional).
#' @param theme_set defaults to standard ggplot theme (grey background, white grid lines)
#' @param statistic annotate NDPE plot with statistical test results ("TRUE").
#' @param parametric whether parametric or non-parametric tests should be applied ("FALSE")
#' @param npde extrem value of NPDE to be identified in the plot.
#' @param xm x coordinate of the position of the statistical test annotation
#' @param ym y coordinate of the position of the statistical test annotation
#' @param predlabel character string identifying label of the x axis, PRED ("concentration (ng/mL)").
#' @param time character string identifying the independent variable in the dataset to be plotted ("TAD").
#' @param timelabel character string identifying label of the x axis, time scale ("time after dose (h)").
#' @param xlim limits x axis
#' @param ylim limits y axis
#' @param xlim2 limits x axis 2nd plot
#' @param xlim3 limits x axis 3rd plot
#' @param alpha.bw transparency value for all points (range between 0 (totally transparent) and 1 (totally intransparent), "0.2").
#' @param se.set display confidence interval around smooth? ("TRUE").
#' @param span Controls the amount of smoothing for the loess smoother. Smaller numbers produce wigglier lines, larger numbers produce smoother lines (ggplot default=2/3).
#' @param size footnote size ("0.4").
#' @param ... Additional arguments. (Currently ignored.)
#'
#' @return Returns a pdf file with 4 plots on a single A4 page.
#' @export
#'
#' @examples num=46892; npdeplot(eval(parse(text=cmd)), num=num, ym=0.45,log=T ,lloq=F, subdir=paste("nmntmp", num,"/plots",sep = ""))
npdeplot <- function
(x,
 num,
 log=F,
 lloq=F,
 addname =NULL,
 subdir="plots",
 theme_set=theme_grey(12),
 statistic=TRUE,
 parametric=FALSE,
 npde=3,
 xm=1.8,
 ym=0.5,
 predlabel='population predicted concentration (ng/L)',
 time="TAD",
 timelabel='time after dose (h)',
 xlim=c(0,0),
 ylim=c(0,0),
 xlim2=c(0,0),
 xlim3=c(0,0),
 alpha.bw=0.2 ,
 se.set=T,
 span=2/3,
 size=0.4,
 ...
 ){

dat <- x

if(is.null(dat$PRED)){stop("PRED is missing in your dataset!")}
if(is.null(dat$NPDE)){stop("NPDE is missing in your dataset!")}


if(!("EVID" %in% colnames(dat)))
  dat$EVID <- rep(0, nrow(dat))
if(!("MDV" %in% colnames(dat)))
  dat$MDV <- rep(0, nrow(dat))

dat <- dat[dat$EVID==0 & dat$MDV==0, ]

if(!("TAD" %in% colnames(dat)))
  message("TAD is missing in your dataset! Make sure to use another TIME variable instead.")
dat[["TAD"]] <- dat[, time]

if(statistic==T) {stat <- npde::gof.test(dat[,'NPDE'], parametric=parametric)
dat$meannpde <- mean(dat[,'NPDE'])} else {dat$meannpde <- mean(dat[,'NPDE'])}
if(lloq==T){dat$PRED <-dat$PREDV}
if(log==T){dat$PRED <- exp(dat$PRED)}

if (all(xlim==0)){xlim=c(min(dat$NPDE)+0.1*min(dat$NPDE), max(dat$NPDE)+0.1*max(dat$NPDE))} else {xlim=xlim}
if (all(ylim==0)){ylim=c(min(dat$NPDE)+0.1*min(dat$NPDE), max(dat$NPDE)+0.1*max(dat$NPDE))} else {ylim=ylim}
if (all(xlim2==0)){xlim2=c(min(dat$PRED)-0.1*min(dat$PRED), max(dat$PRED)+0.1*max(dat$PRED))} else {xlim2=xlim2}
if (all(xlim3==0)){xlim3=c(min(dat$TAD)-0.1*min(dat$TAD), max(dat$TAD)+0.1*max(dat$TAD))} else {xlim3=xlim3}


if(statistic==T){stattext=paste("N = ", stat$nobs,"\n",
               "mean = ",signif(stat$mean,3), " (p value = ",signif(stat$p.value[1],2),") ","\n",
               "var = ",signif(stat$var,3), " (p value = ",signif(stat$p.value[2],2),") ","\n",
               "kurtosis = ",signif(stat$kurtosis,2),"; skweness = ",signif(stat$skewness,2),"\n",
               "SW test of normality = ",signif(stat$p.value[3],2), "\n",
               "global p value = ",signif(stat$p.value[4],2), sep="")}else{stattext=''}


default_theme<-theme_set(theme_set)

pp6<-list()
p61<-ggplot(dat,aes(sample = NPDE)) +
  stat_qq(shape=1,size=2,alpha=alpha.bw)+
  geom_abline(size=0.4,linetype="twodash",colour="darkgrey") +
  labs(y="theoretical quantiles",x="sample quantiles (npde)")
pp6[[1]]<-p61

x = seq(-3,3,by=.1)
dens <- data.frame(x = x, y = dnorm(x))
mycols <- c("#3366FF","darkgrey")
p62<-ggplot(dat, aes(x=NPDE)) +
  geom_density(colour="darkgrey")+
  geom_histogram( binwidth=.2, alpha=.5, aes(y=..density..)) +
  geom_vline(aes(xintercept=meannpde),  colour="darkgrey")+
  geom_line(data=dens, aes(x, y, colour="#3366FF"))+
  geom_vline(aes(xintercept=0),  colour="#3366FF")+
  labs(x="npde")+
  xlim(xlim)+
  scale_colour_manual(values = mycols) +
  theme(legend.position="none")+
  annotate("text", x=xm, y=ym, label=stattext, size=2.2)
pp6[[2]]<-p62

p63<-ggplot(dat, aes(PRED, NPDE,label=ID)) +
  geom_point(shape=1,size=2,alpha=alpha.bw)+
  stat_smooth(method="loess", se=se.set, span = span)+
  geom_hline(yintercept=0, size = 0.4,linetype="twodash",colour="darkgrey") +
  geom_text(data=dat[abs(dat$NPDE)>npde,],hjust=-0.5,vjust=0.5,angle=0,color='black',size=3)+
  ylab('npde')+
  xlab(predlabel)+
  ylim(ylim)+
  xlim(xlim2)
pp6[[3]]<-p63

p64<-ggplot(dat, aes(TAD, NPDE,label=ID)) +
  geom_point(shape=1,size=2,alpha=alpha.bw)+
  stat_smooth(method="loess", se=se.set, span = span)+
  geom_hline(yintercept=0, size = 0.4,linetype="twodash",colour="darkgrey") +
  geom_text(data=dat[abs(dat$NPDE)>npde,],hjust=-0.5,vjust=0.5,angle=0,color='black',size=3)+
  xlab(timelabel)+
  ylab('npde')+
  ylim(ylim)+
  xlim(xlim3)

pp6[[4]]<-p64


filename = paste(subdir, "/", "npde",addname,".",num, ".pdf", sep = "")

pdf(file=filename, width = 8, height = 11*2/3)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
layout = matrix(c(1, 2, 3, 4), nrow = 2,byrow=T)
for (i in 1:4) {
  # Get the i,j matrix positions of the regions that contain this subplot
  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

  print(pp6[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                layout.pos.col = matchidx$col))}

if (substring(subdir,1,6) =="nmntmp"){
  WD <- getwd()
  apath = paste(WD, "/nmnqs", sep = "")
  cmd = paste("cd ", apath, "; basename *.", num, ".tar.gz", sep = "")
  tarname = system(cmd, intern = TRUE)
  sub = substring(subdir,regexpr("/",subdir,perl=T)[1]+1, nchar(subdir))
  finalwd = paste(WD, tarname, sub ,sep="/")
  filename = paste("npde",addname,".",num, ".pdf", sep = "")
  makeFootnote(footnoteText = paste(Sys.getenv("USER"),"; ", finalwd, "/",filename, ";", format(Sys.Date()), sep = ""), size=size) }else{
    makeFootnote(footnoteText = paste(Sys.getenv("USER"),"; ", getwd(), filename, ";", format(Sys.Date()), sep = ""), size=size)
  }

dev.off()

}

# end npdeplot()
