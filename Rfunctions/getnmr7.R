getnmr7 <- function#Creates an xpose object with special extensions as the slot @Esti
###Input is the run number from a archive (tar.gz). It creates a temporary subdirectory to expand the archive and reads the fit of an XPOSE object. The control file must be compatible to the xpose needs.
###FunctionGroup:
###PopPKCalculation
(num,
###The number of the archive.
path=".",
###Where to find the nmnqs subdir
data=FALSE,
###If True the data file is merged with the table files out of the run.
xposeread=TRUE,
###If True the standard XPOSE data read is active else only the object sanofi-xpose is created and the slot Esti is filled
RM=TRUE,
###Delete the created nmtmp<num> directory
hist=FALSE,
###Bool to create esti.hist usfull for SAEM to plot the burn in is used in getnmr7.esti
esti=TRUE,
###estimates are read and stored in the fit obj. With FALSE we have poor XPOSE behavior. No estimates are read.
thetastart=FALSE,
###Bool variable to read initial THETAS from the report file
check=TRUE,
###use check.factor to eval factor settings
shrinkage=F,
###to skip shrinkage extract needed for $MIX
estinum=NULL
###for default use of multiple esti
) {
  require(xpose4)
  require(Biobase)
  #require(popnew)
  require(plyr)
###########
###########

## memorising the working directory
  WD<-getwd()
  NUM<-sample(1:1000)[1]

##path detection
  if ( path=="." ) {
    apath=paste(WD,"/nmnqs",sep="")
    storepath=WD
  } else {
    ### check for realative path
    if (length(grep("^/",path)==1)==0) {
      apath=paste(WD,"/",path,"/nmnqs",sep="")
      storepath=paste(WD,"/",path,sep="")
    } else {
      apath=paste(path,"/nmnqs",sep="")
      storepath=path
    }
  }
  ## check remote operation and run if possible speed up because no network operation on tar
  cmd=paste("TT=`df -t nfs ",apath, "| grep :`; echo ${TT%%:*}",sep="")
  HOST=system(cmd,intern=T)
  TEMPDIR=paste(storepath, "/nmntmp",NUM,sep="")
  cmd2=NULL
  if ( nchar(HOST) > 0 ) {
    cmd=paste("ssh ",HOST, " 'rm -r ",TEMPDIR, " 2>/dev/null; mkdir ",TEMPDIR, "; cd ",TEMPDIR,"; tar -xzf ", apath, "/*.",num,".tar.gz 1>/dev/null'",sep="")
    system(cmd)
  } else {
    ## temp dir for tar operations
    #browser()
    cmd=paste("rm -r ",TEMPDIR, " 2>/dev/null; mkdir ",TEMPDIR, "; cd ",TEMPDIR,"; tar -xzf ", apath, "/*.",num,".tar.gz 1>/dev/null",sep="")
    #browser()
    system(cmd)
  }

  ##browser()


  ### name of the fit
  fit<-paste("fit",num,sep="")
  ##browser()
  if ( xposeread==F ) {
  #  cmd=paste(fit,"@Data<-NULL",sep="")
  #  eval(parse(text=cmd))
    createXposeClassesSanofi()
    cmd=paste(fit,"<-new('xpose.data',Runno = num, Doc = NULL, Data = NULL)",sep="")
    eval(parse(text=cmd))
    cmd2=paste("fit",num, " created", sep="")
  } else {

    ### creating a Xpose Data object
    #cmd=paste(fit,"<-xpose.data(",num," ,directory='",WD,"/",TEMPDIR,"/')",sep="")
    cmd=paste(fit,"<-try(xpose.data(",num,"))",sep="")
    #browser()
    setwd(TEMPDIR)
    eval(parse(text=cmd))
    setwd(WD)
    ### updating to the sanofi Xpose object
    createXposeClassesSanofi()
    cmd=paste(fit,"<-updateObject(",fit,")",sep="")
    ##browser()
    eval(parse(text=cmd))
    ##browser()
    ## egrep eliminate all lines beginning with space followed by ; or beginning with ;
    cmd=paste("cd ", TEMPDIR,"; egrep -iv '^[[:space:]]+;|^;' *.con | grep '$DA' | awk '{print $2}'",sep="")
    dataname<-system(cmd,intern=T)
    if ( data==T ) {
      ## to have the real datafile name in the $call of the attributes of the dataframe
      ##browser()
      cmd=paste("data<-getN(","'",dataname,"'",",path='",TEMPDIR,"')",sep="")
      ##browser()

      eval(parse(text=cmd))
      ## this is not so good, because dataname is no useful information in $call
      ##data<-getN(dataname)
      ## the merge command is constructed
      cmd=paste("try(",fit,"@Data<-mergeXpose(data,",fit,"))",sep="")
      #browser()
      eval(parse(text=cmd))
    }

  }
  #browser()
  if ( esti==T ) {
    ## adding results in @Esti
    xposeobj<-paste("fit",num,sep="")
    #browser()
    cmd=paste(fit,"@Esti<-getnmr7.esti(num=",num,",path=TEMPDIR,storepath=storepath,hist=hist,xposeobj=eval(parse(text=xposeobj)),thetastart=thetastart,shrinkage=shrinkage,estinum=estinum)",sep="")
    #browser()

    eval(parse(text=cmd))
    #browser()
   }
    cmd=paste(fit,"<<-",fit,sep="")
    ##browser()
    eval(parse(text=cmd))
   ### The fit<num> object must exists befor check.factor can run
   if (check==T&xposeread==T) {

      try(check.factor(num=num))
        ## putlabel if exists
      #browser()
      cmd<-paste("cd ",TEMPDIR,";  tt=`ls *",num,".lab 2>/dev/null`; echo $tt", sep="")
      #browser()
      tt<-system(cmd,intern=T)

      if ( nchar(tt)) {
	#browser()
	cmd<-paste("getLabels(obj=",fit,",path='",TEMPDIR,"')",sep="")
        #browser()
        try(eval(parse(text=cmd)))
        #browser()
      }


    }

  ## parse to the calling level
  #cmd=paste(fit,"<<-",fit,sep="")
  ##browser()
  #eval(parse(text=cmd))
  ##assign(fit, fit, envir=.GlobalEnv)
  ##setwd(WD)

  #browser()
  #cmd=paste("rm(fit",num,",envir=.GlobalEnv)",sep="")
  #eval(parse(text=cmd))
  if ( RM==T ) {
    cmd=paste("rm -r ",TEMPDIR, sep="")
    system(cmd)
  } else {
    #cmd=TEMPDIR
    return(basename(TEMPDIR))
  }
  if (!is.null(get0("cmd2"))) {
   return(cmd2)
  }
###Returns the temp directory full path. Useful with RM=F and individual manipulation of the created directory.
###In addition, an XPOSE object called fit<archive number> is created in the actual R environment.
}

