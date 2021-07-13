compnmr7<-function#Compares a serie of runs
###Reads the results of all runs in the subdir nmnqs and creates a data.frame of the results as return value.
###Detects the different compare modes like bootstrap,jacknife,profile,initial and compares the runs in an appropriate way.
###FunctionGroup:
### PopPKCalculation
(path=".",
###Path to find base directory (one directory above nmnqs). Full path is the best.
merging=T,
###Reads fits from the actual environment and loads the missing ones from path.
###Useful to add runs, which are later finished, whithout reading again the fits. (Saves a lot of time).
del=F,
###Removes all called fits (path) from the environment. Re-reading from the filesystem is needed in the case of comparing the runs again.
data = F,
###Parameter for getnmr7. Data read is not useful for comparing runs.
xposeread = F,
###Parameter for getnmr7. Table read is not useful for comparing runs.
OBJdiff=T,
###Parameter for OBJdiff to compare only successful runs.
OBJref=NULL,
###Parameter for getnmr7 chose not the last estimation.
estinum=NULL,
###Parameter for OBJdiff to an objective value of a reference run (Base Model)  to compare only successful runs. NULL the default makes the standard relative diff to the run in the line before.
ltheta = F
###Not used in the moment. For future handling of log fitted THETAS.
){
  require(plyr)

    ##path detection
  WD<-getwd()
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
## detect compare methode initial,profile,jacknife,bootstrap,etc
compmethod<-strsplit(storepath,"/")
compmethodbase<-compmethod[[1]][length(compmethod[[1]])]

compmethod<-strsplit(compmethodbase,".",fixed=T)
compmethod<-compmethod[[1]][1]
compmethodref=c("bootstrap","initial","jacknife","jitter","profile","robust")
### clean from old nmntmp dir
    cmd="rm -r nmntmp* 2>/dev/null"
    system(cmd)

### search for tar.gz and start getnmr7 to create the fits
    cmd=paste("cd ",apath," ;ls *.tar.gz | awk -F .tar '{print $1}' | awk -F . '{print $NF}'",sep="")
    vectfitpath<-system(cmd,intern=T)
    vectfittargz<-vectfitpath
    vectfits<-ls(envir=.GlobalEnv)
    vectfits<-vectfits[grep("^fit",vectfits)]
    if ( merging==T ) {
    ### Filter for what must be read from tar.gz
      vectfittargz<-vectfittargz[! (paste("fit",vectfittargz,sep="") %in% vectfits) ]
    }
    #browser()
    for ( i in vectfittargz ) {
      try(getnmr7( num=i, path=storepath,data=data, xposeread=xposeread, estinum=estinum))
      print(i)
    }
    vectfitsf<-ls(envir=.GlobalEnv)
    vectfitsf<-vectfitsf[grep("^fit",vectfitsf)]
    vectfitsf<-vectfitsf[vectfitsf %in% (paste("fit",vectfitpath,sep=""))]
    ##browser()
    j=0
    for ( i in vectfitsf ) {
      j=j+1
      if ( exists(i) ){
	## needed to get from the .GlobalEnv
	fit<-get(i,envir=.GlobalEnv)
        if ( j==1 ) {
	  comp<-fit@Esti$res.comp
	}
	if ( j>1 ) {
	 try(comp<-rbind.fill(comp,fit@Esti$res.comp))
	  ##browser()
	} #else {
	  ##browser()
	  #comp<-fit@Esti$res.comp
	#}
        #if ( j==201 ) {
	#  browser()
	#}
      }
    }

    if ( del==T ) {
	cmd<-paste("fit",vectfitpath,sep="")
	##browser()
	cmd<-paste("try(rm('",cmd,"',envir=.GlobalEnv))",sep="")
        ##browser()
        eval(parse(text=cmd))
    }
  ### sorting
  #browser()
  ### order based on compmethod
    comp$STATUSCOMP<-0
    comp$STATUSCOMP[!is.na(comp$EIGENRATIO)]<-1
    comp$STATUSCOMP[comp$STATUS1=="0MINIMIZATION TERMINATED"]<-0
    comp$STATUSCOMP[comp$STATUS1=="0MINIMIZATION SUCCESSFUL"]<-1
    lastpos<-length(dimnames(comp)[[2]])
    eigenvectorpos<-grep("^EIGENRATIO",dimnames(comp)[[2]])
    objvectorpos<-grep("^OBJ",dimnames(comp)[[2]])
    corrvectorpos<-grep("^CORRELATION",dimnames(comp)[[2]])
    statusvectorpos<-grep("^STATUS",dimnames(comp)[[2]])
    infovectorpos<-c(1:(statusvectorpos[1]-1),statusvectorpos,eigenvectorpos,corrvectorpos)
    thetavectorpos<-(1:lastpos)[c((-1)*(infovectorpos),(-1)*objvectorpos,(-1)*(grep(":|\\(",dimnames(comp)[[2]])))]
    thetacvvectorpos<- (1:dim(comp)[2])[dimnames(comp)[[2]] %in% paste("CV(",dimnames(comp)[[2]][thetavectorpos],")",sep="")]
    errvectorpos<-grep("^sigma²",dimnames(comp)[[2]])
    errcvvectorpos<-grep("^CVsigma²",dimnames(comp)[[2]])
    etavectorpos<-(1:lastpos)[c((-1)*(infovectorpos),(-1)*(eigenvectorpos+2),(-1)*thetavectorpos,(-1)*(grep("\\(",dimnames(comp)[[2]])))]
    etacvvectorpos<-grep("^CV\\(.+:",dimnames(comp)[[2]])
    etabarvectorpos<-grep("^ETABAR",dimnames(comp)[[2]])
    etapvalvectorpos<-grep("^ETAPVAL",dimnames(comp)[[2]])
    omegaSHRINKvectorpos<-grep("^omegaSHRINK",dimnames(comp)[[2]])
    sigmaSHRINKvectorpos<-grep("^sigmaSHRINK",dimnames(comp)[[2]])
    #browser()
    finalpos<-c(infovectorpos,objvectorpos,thetavectorpos,thetacvvectorpos,etavectorpos,etacvvectorpos,errvectorpos,errcvvectorpos,etabarvectorpos,etapvalvectorpos,omegaSHRINKvectorpos,sigmaSHRINKvectorpos)
    #summarycompdataframe<-summarycompdataframe
    #browser()
    ### musss geprüft werden
    #c("bootstrap","initial","jacknife","jitter","profile")
    if (compmethod %in% c("bootstrap","jacknife","robust")) {
      #browser()
      comp<-comp[comp$STATUSCOMP==1,]
      comp<-comp[,finalpos]
      rownames1<-paste("fit",comp[,1],sep="")
      #dimnames(comp)[[1]]<-rownames1
      ###summarycomp<-summary(comp[,(-1)*1:grep("^OBJ$",names(comp))])
      #browser()
      #dimnames(summarycomp)[[2]]<-dimnames(comp[,(-1)*c(infovectorpos,objvectorpos)])[[2]]
      ###rownamessummary<-c("Min.","2.5% Qu.","5% Qu.","1st Qu.","Median","3rd Qu.","95% Qu.","97.5% Qu.","Max.","NA's","Mean","Std","CV%")
      ###summarycomp<-sub("Min.    :|2.5% Qu :|5% Qu   :|1st Qu. :|Median  :|3rd Qu. :|95% Qu  :|97.5% Qu:|Max.    :|NA's    :|Mean    :|Std     :|CV%     :","",summarycomp)
      ###dimnames(summarycomp)[[1]]<-rownamessummary
      ###summarycompvector<-as.numeric(summarycomp)
      ###summarycompdataframe<-as.data.frame(matrix(summarycompvector,nrow=13))
      ###dimnames(summarycompdataframe)[[1]]<-dimnames(summarycomp)[[1]]
      ###dimnames(summarycompdataframe)[[2]]<-dimnames(comp)[[2]][(-1)*1:grep("^OBJ$",names(comp))]
      #browser()
      ###comp<-rbind.fill(comp,summarycompdataframe)
      #browser()
      ###rownames2<-dimnames(summarycompdataframe)[[1]]
      ###rownames<-c(rownames1,rownames2)
      #browser()
      ###dimnames(comp)[[1]]<-rownames
      ###cmd<-paste("attr(comp,'comp') <- '",compmethodbase,"'", sep="")
      #browser()
      ###eval(parse(text=cmd))
      ##new perhaps better 8.11.2012
      df.df<-comp[,(-1)*1:grep("^OBJ$",names(comp))]
      sumcomp<-summary.data.frame(df.df)
      sumcomp1<-as.matrix(gsub("*.*:","",sumcomp,perl=F))
      sumcomp1a<-apply(sumcomp1,2,function(x){as.numeric(x)})
      dimnames(sumcomp1a)[[2]]<-dimnames(comp[,(-1)*1:grep("^OBJ$",names(comp))])[[2]]
      sumcomp1a.df<-as.data.frame(sumcomp1a)
      rownamessum<-gsub(":.*",":",sumcomp[,1])
      #dimnames(sumcomp1a.df)[[1]]<-rownamessum

      #browser()
      testnames<-c(rownames1,rownamessum)
      compback<-rbind.fill(comp,sumcomp1a.df)
      #compback<-cbind(testnames,compback)
      try(dimnames(compback)[[1]]<-testnames)
      #dimnames(compback)[[1]]<-c(rownames1,rownamessum)
      cmd<-paste("attr(compback,'comp') <- '",compmethodbase,"'", sep="")
      #browser()
      eval(parse(text=cmd))
      return(compback)
    }
    if (compmethod=="initial") {
      #browser()
      rownames1<-paste("fit",comp[,1],sep="")
      dimnames(comp)[[1]]<-rownames1
      #browser()

      #if (!is.null(comp$EVALS)) {
	#comp<-comp[order(comp$STATUS1,comp$OBJ,comp$EVALS,decreasing=T),finalpos]
      #} else {
	try(comp<-comp[order(comp$STATUS1,comp$OBJ,decreasing=T),finalpos])
        if (!is.null(comp$OBJSAEM)){
	  try(comp<-comp[order(comp$STATUS1,comp$OBJSAEM,decreasing=T),finalpos])
      	}
      #}
      cmd<-paste("attr(comp,'comp') <- '",compmethodbase,"'", sep="")
      #browser()
      eval(parse(text=cmd))
      return(comp)
    }
    if (compmethod=="profile") {
      rownames1<-paste("fit",comp[,1],sep="")
      dimnames(comp)[[1]]<-rownames1
      comp<-comp[comp$STATUSCOMP==1,]
      ## sorting makes no sense, because it is sorted by default over the runnumber
      #POS<-as.numeric(gsub("^init.|\\.+|[a-Z]+[0-9]+|[a-Z]+|[0-9]+.con$","",comp$CONFILE))
      #comp$POS<-POS
      #browser()
      #comp<-comp[order(comp$POS,decreasing=F),finalpos]
      #cmd<-paste("attr(comp,'comp') <- ",compmethodbase, sep="")
      #eval(parse(text=cmd))
      cmd<-paste("attr(comp,'comp') <- '",compmethodbase,"'", sep="")
      #browser()
      eval(parse(text=cmd))
      return(comp)
    }

    rownames1<-paste("fit",comp[,1],sep="")
    dimnames(comp)[[1]]<-rownames1
    #browser()
    if (OBJdiff==T) {
      comp<-comp[comp$STATUSCOMP==1,]
      #comp<-comp[order(comp$STATUS1,comp$OBJ,comp$EVALS,decreasing=T),finalpos]
      comp<-comp[order(comp$STATUS1,comp$OBJ,decreasing=T),finalpos]


      if (is.null(OBJref)){
        REF<-comp$OBJ
        OBJdiff<-c(comp$OBJ,comp$OBJ[length(comp$OBJ)])
        OBJdiff<-OBJdiff[2:length(OBJdiff)]
        OBJdiff<-c(OBJdiff-REF)
        OBJdiff<-c(0,OBJdiff)
      } else {
        OBJdiff<-comp$OBJ
        REF<-comp$OBJ[comp$ARCHIVE==OBJref]
        OBJdiff<-c(OBJdiff-REF)
      }
      #browser()
      OBJdiff<-OBJdiff[1:length(comp$OBJ)]
      comp$OBJdiff<-OBJdiff

      objvectorpos<-grep("^OBJ$",dimnames(comp)[[2]])
      objdiffvectorpos<-grep("^OBJdiff$",dimnames(comp)[[2]])
      pos<-c(1:objvectorpos,objdiffvectorpos,(objvectorpos+1):(objdiffvectorpos-1))
      #browser()
      #comp<-comp[,c(1:objvectorpos)

      #browser()
      #finalpos<-c(infovectorpos,objvectorpos,objdiffvectorpos,thetavectorpos,thetacvvectorpos,etavectorpos,etacvvectorpos,errvectorpos,errcvvectorpos,etabarvectorpos,etapvalvectorpos,omegaSHRINKvectorpos,sigmaSHRINKvectorpos)
      comp<-comp[,pos]
      cmd<-paste("attr(comp,'comp') <- '",compmethodbase,"'", sep="")
      eval(parse(text=cmd))
      #browser()
      return(comp)
    } else {
      #comp<-comp[order(comp$STATUS1,comp$OBJ,comp$EVALS,decreasing=T),finalpos]
      comp<-comp[order(comp$STATUS1,comp$OBJ,decreasing=T),finalpos]

      cmd<-paste("attr(comp,'comp') <- '",compmethodbase,"'" ,sep="")
      eval(parse(text=cmd))

      return(comp)
    }
###Returns the data.frame of the compared runs.
}
