getnmr7.esti<-function#Reads the report results of a run from actual dir
###Only one run is allowed per directory. The run must be coded in the XPOSE4 style. This is an internal function used by other functions.
###FunctionGroup:
###PopPKCalculation
(num,
###Runnumber of the run
path=".",
###From which subdir the run is taken
storepath=".",
###Whitch path should be store in the variable PATH in $files
hist=F,
###Bool to create esti.hist usfull for new METHODS to plot the burn in
xposeobj,
###The generated xposeobj
thetastart=FALSE,
###Bool value to read Initial Theta from report file
estinum=NULL,
###For multiple esti, what to take for future use in the moment the last one is taken
shrinkage=T
###report shrinkage not for bayes or $MIX

) {
  #browser()
  ## adding results in @Esti
  ## storing names defined with the contol file

  cmd=paste("cd ",path,"; nm_get_names7.pl -f *.con",sep="")
  tmp<-as.list(system(cmd, intern=T))
  names(tmp)<-c("namesth","nameset","nameser","namescor","namescorer","namescoret")

  ## split the string into substring sep by BLANK
  tmp$namesth<-as.character(unpaste(tmp$namesth,sep=" "))
  ##data.frame for namesth (THETA)
  tmp[["namesth"]]<-as.data.frame(t(as.data.frame(tmp[["namesth"]],stringsAsFactors=FALSE)),stringsAsFactors=FALSE)
  names(tmp[["namesth"]])<-paste("THETA",1:length(tmp[["namesth"]]),sep="")
  dimnames(tmp[["namesth"]])[[1]]<-1

  ## split the string into substring sep by BLANK
  tmp$nameset<-as.character(unpaste(tmp$nameset,sep=" "))
  ##data.frame for nameset (ETA)
  tmp[["nameset"]]<- as.data.frame(t(as.data.frame(tmp[["nameset"]],stringsAsFactors=FALSE)),stringsAsFactors=FALSE)
  names(tmp[["nameset"]])<-paste("ETA",1:length(tmp[["nameset"]]),sep="")
  dimnames(tmp[["nameset"]])[[1]]<-1

  ## split the string into substring sep by BLANK
  tmp$nameser<-as.character(unpaste(tmp$nameser,sep=" "))
  ##data.frame for nameser (ERR)
  tmp[["nameser"]]<-as.data.frame(t(as.data.frame(tmp[["nameser"]],stringsAsFactors=FALSE)),stringsAsFactors=FALSE)
  names(tmp[["nameser"]])<-paste("ERR",1:length(tmp[["nameser"]]),sep="")
  dimnames(tmp[["nameser"]])[[1]]<-1

  # split the string into substring sep by BLANK
  #tmp$namescor<-as.character(unpaste(tmp$namescor,sep=" "))

  ## split the string into substring sep by BLANK
  tmp$namescorer<-as.character(unpaste(tmp$namescorer,sep=" "))
  ##data.frame for namescorer the correlations of (ERR)
  tmp[["namescorer"]]<- as.data.frame(t(as.data.frame(tmp[["namescorer"]],stringsAsFactors=FALSE)),stringsAsFactors=FALSE)
  tochange<-as.character(tmp[["namescorer"]])
  for(i in seq(along = tmp[["nameser"]])){
   #print(i)
   cmd<-paste("tochange<-gsub(tmp[['nameser']][[",i,"]],names(tmp[['nameser']][",i,"]), tochange)",sep="")
   #print(cmd)
   eval(parse(text=cmd))
  }
  tochange<-paste("SIGMA(",gsub("ERR","",tochange),")",sep="")
  tochange<-gsub(":",",",tochange)
  tochange<-gsub("Y","",tochange)
  names(tmp[["namescorer"]])<-tochange
  dimnames(tmp[["namescorer"]])[[1]]<-1


  ## split the string into substring sep by BLANK
  tmp$namescoret<-as.character(unpaste(tmp$namescoret,sep=" "))
  tmp[["namescoret"]]<- as.data.frame(t(as.data.frame(tmp[["namescoret"]],stringsAsFactors=FALSE)),stringsAsFactors=FALSE)
  tochange<-as.character(tmp[["namescoret"]])
  for(i in seq(along = tmp[["nameset"]])){
   #print(i)
   cmd<-paste("tochange<-gsub(tmp[['nameset']][[",i,"]],names(tmp[['nameset']][",i,"]), tochange)",sep="")
   eval(parse(text=cmd))
  }
  tochange<-paste("OMEGA(",gsub("ETA","",tochange),")",sep="")
  tochange<-gsub(":",",",tochange)
  names(tmp[["namescoret"]])<-tochange
  dimnames(tmp[["namescoret"]])[[1]]<-1

  ## create namescor from the calulated names data frames
  tmp$namescor<-cbind(tmp[["namesth"]],tmp[["namescorer"]],tmp[["namescoret"]])
  #browser()
  if ( thetastart ) {
    ## check first if initial THETAS are defined
    cmd=paste("cd '",path, "'; grep '0LENGTH OF THETA:' *.rep | awk '{print $4}'",sep="")
    THNUM<-as.numeric(system(cmd,intern=T))
    if ( THNUM>1 ) {
      ## creation of dataframe of starting THETAS
      ## check if THETA with constrains
      cmd=paste("cd '",path, "'; grep '0INITIAL ESTIMATE OF THETA:' *.rep -A ",length(tmp$namesth) + 1," >initialTheta.txt ",sep="")
      system(cmd)
      FILE=paste(path,"/initialTheta.txt",sep="")
      tmp$initialTHETA<-read.fwf(file=FILE,widths=c(13,15,15),header=FALSE,skip=2)
      dimnames(tmp$initialTHETA)[[1]]<-tmp$namesth
      dimnames(tmp$initialTHETA)[[2]]<-c("LOWER","START","UPPER")
      ##tmp$initialTHETA$V2=")"
      tmp$initialTHETA<-cbind(V1="(",tmp$initialTHETA,V2=")")
      tmp$initialTHETA<-format(tmp$initialTHETA, scientific = F)
    }
  }
  #browser()
  ## extracting last TABLE into temp file
  if ( is.null(estinum) ) {
    cmd=paste('cd ',path,'; TT=`egrep TABLE  *.ext | tail -n 1 | awk -F : \'{print $1}\'`; egrep "$TT"  *.ext -A 1000000 | egrep "ITERATION|-100000000">tmp.res',sep="")
    system(cmd)
    #browser()
    ## reading temp file for XPOSEobject@Esti
    cmd=paste(path,"/tmp.res",sep="")
    if ( is.null(count.fields(file=cmd))) {
      return(NULL)
    }
  } else {
    cmd=paste('cd ',path,'; egrep "ITERATION|-100000000" *.ext.',estinum,' >tmp.res',sep="")

    system(cmd)
    cmd=paste(path,"/tmp.res",sep="")
    if ( is.null(count.fields(file=cmd))) {
      return(NULL)
    }
  }
  #browser()
  tmp$esti<-getN(file="tmp.res",path=path)
  #browser()
  if (tmp$esti[2,1]!="-1000000001"|is.na(tmp$esti[2,1])) {
    tmp$esti<-tmp$esti[1,,drop=FALSE]
  }
  if (is.null(tmp$esti$OBJ)){
    hist=T
  }
  ## extracting history TABLE into temp file the last fit is taken
  if ( is.null(estinum) ) {
    cmd=paste('cd ',path,'; TT=`egrep TABLE  *.ext | tail -n 1 | awk -F : \'{print $1}\'`; egrep "$TT"  *.ext -A 1000000 | egrep -iv "$TT" | grep -iv 1000000 >tmp1.res',sep="")
    system(cmd)
    #browser()
    ## overwrite with saemext.pl is only active if SAEM is fitted
    cmd=paste('cd ',path,';saemext.pl -f *.ext', sep="")
    system(cmd)
    #browser()
    ## reading temp file for XPOSEobject@Esti
    cmd=paste(path,"/tmp1.res",sep="")
    if ( is.null(count.fields(file=cmd))) {
      return(NULL)
    }
    } else {
      cmd=paste('cd ',path,'; egrep "10000000" *.ext.',estinum,' >tmp1.res',sep="")
      system(cmd)
      #browser()
      ## overwrite with saemext.pl is only active if SAEM is fitted
      cmd=paste('cd ',path,';saemext.pl -f *.ext', sep="")
      system(cmd)
      #browser()
      ## reading temp file for XPOSEobject@Esti
      cmd=paste(path,"/tmp1.res",sep="")
      if ( is.null(count.fields(file=cmd))) {
        return(NULL)
      }
    }
  if ( hist ) {
   tmp$esti.hist<-getN(file="tmp1.res",path=path)
  }

  ## creating cor table
  cmd=paste('cd ',path,'; TT=`egrep TABLE  *.cor 2>/dev/null| tail -n 1 | awk -F : "{print $1}"`; egrep "$TT"  *.cor -A 1000000 2>/dev/null | grep -iv "TABLE" >cor.res',sep="")
  system(cmd)
  cmd=paste(path,"/cor.res",sep="")
  if ( !is.null(count.fields(file=cmd))) {
    tmp$cor<-getN(file="cor.res",path=path,row.names="NAME")
    ##cortotest<<-tmp$cor
    ##COR<<-tmp$cor
    ##namescor<<-tmp$namescor
    #browser()
    nametochange<-as.character(dimnames(tmp$cor)[[1]])
    searchvector<-tmp[["namescor"]]

    for(i in seq(along = as.character(searchvector))){
      #print(i)
      #PATTERN=
      cmd<-paste("nametochange<-gsub('",names(searchvector)[i], "','",searchvector[[i]],"',nametochange,fixed=T)",sep="")
      ##print(cmd)
      eval(parse(text=cmd))
     }
    ###
    try(dimnames(tmp$cor)[[1]]<-nametochange)
    try(dimnames(tmp$cor)[[2]]<-nametochange)
    ##Index vector of nonzero columns
    nonzero<-which(unlist(lapply(tmp$cor,function(col)return(!all(col==0)))))
    #browser()

    #BOOLH<- !tmp$cor[1,]==0
    #BOOLV<- !tmp$cor[,1]==0
    tmp$cor<-as.data.frame(tmp$cor[nonzero,nonzero])
    tmp$cortest<-as.matrix(tmp$cor)
    diag(tmp$cortest)<-1
    test<-stack(as.data.frame(tmp$cortest))
    #browser()
    if (!dim(tmp$cor)[1]==0) {
    VALLOW<-unique(as.character(test[test[,1]<(-0.96),]$ind))
    test[test[,1]==1,1]<-(-1)
    VALHIGH<-unique(as.character(test[test[,1]>(0.96),]$ind))
    VAL=c(VALLOW,VALHIGH)
    BOOLV<-dimnames(tmp$cor)[[1]] %in% VAL
    BOOLH<-dimnames(tmp$cor)[[2]] %in% VAL
    if ( dim(tmp$cor[BOOLV,BOOLH,drop=F])[1]!=0 ){
      tmp$cor96<-tmp$cor[BOOLV,BOOLH,drop=F]
    }
    }
  }
  ## creating list of names of the archiv
  #browser()
  tmp$files=list()
  ##cmd=paste('cd ',path,"; ls patab* | perl -ne '{s/patab//; chomp $_; print $_;}'",sep="")
  #browser()
  tmp$files[["ARCHIVE"]]<-num
  cmd=paste('cd "',path,'"; ls *.con', sep="")
  tmp$files[["CONFILE"]]<-system(cmd,intern=T)
  cmd=paste("cd ",path,"; egrep -iv '^[[:space:]]+;|^;' *.con | grep '$DA' | awk '{print $2}'",sep="")
  tmp$files$DATFILE<-system(cmd,intern=T)




  cmd=paste("cd ",path,"; egrep -iv '^[[:space:]]+;|^;' *.rep | grep ' TOT. NO. OF' | awk -F : '{print $2}'",sep="")
  #browser()
  listtmp<-as.list(as.numeric(system(cmd,intern=T)))
  names(listtmp)<-c("OBS","IDS")
  tmp$files<-c(tmp$files,listtmp)

##add computer name here
  #browser()
  cmd=paste("cd ",path,"; egrep -iv '^[[:space:]]+;|^;' *.rep | grep '^popkin[0-9][0-9][0-9]$'",sep="")
  tmp$files[["NODE"]]<-system(cmd,intern=T)
  if (length(tmp$files$NODE)==0) {
    #browser()
    tmp$files[["NODE"]]<-"unkown"
    #browser()
  } else {
    tmp$files[["NODE"]]<-tmp$files[["NODE"]][1]
  }

##add used cores here
  cmd=paste("cd ",path,"; egrep -iv '^[[:space:]]+;|^;' *.rep | grep 'PARA:'| awk -F 'NODES=' {'print $2'}",sep="")
  tmp$files[["CORES"]]<-as.numeric(system(cmd,intern=T))
#browser()
  if (length(tmp$files$CORES)==0) {
    #browser()
    tmp$files[["CORES"]]<-1
    #browser()
  }


#  tmp$files<-c(tmp$files,listtmp)
#browser()
  ### add runtime here

  cmd=paste("cd ",path,"; egrep -iv '^[[:space:]]+;|^;' *.rep | grep 'seconds' | awk -F : '{print $2}'",sep="")
  listtmp<-as.list(as.numeric(system(cmd,intern=T)))
  cmd=paste("cd ",path,"; egrep -iv '^[[:space:]]+;|^;' *.rep | grep 'seconds' | awk -F : '{print $1}' | awk '{print $2}' ",sep="")

  names(listtmp)<-system(cmd,intern=T)
  #browser()
  if ( !length(listtmp)==0 ) {
    tmp$files[["RUNTIME(h)"]]<-round(sum(as.numeric(listtmp))/3600,4)
    tmp$files[["TIME ESTI(h)"]]<-round(as.numeric(listtmp[[1]])/3600,4)

  }


  cmd=paste("cd ",path,"; egrep -iv '^[[:space:]]+;|^;' *.rep | grep ' CUMULATIVE NO. OF FUNC. EVALS.:' | awk -F : '{print $2}'",sep="")
  listtmp<-as.list(as.numeric(system(cmd,intern=T)))
  POS<-length(listtmp)
  #browser()
  if (POS>0) {
    listtmp<-listtmp[[length(listtmp)]]
    names(listtmp)<-c("EVALS")
    tmp$files<-c(tmp$files,listtmp)
  }
  listtmp<-as.list(length(tmp$namesth)+length(tmp$nameset)+length(tmp$err))
  names(listtmp)<-c("nP")
  names(listtmp)<-c("nPSE")
  tmp$files<-c(tmp$files,listtmp)

  tmpnamebase<-names(tmp$esti)
  names(tmp$esti)<-c("DUMMY",tmp$namescor,"DUMMY")
  names(tmp$esti)[1]<-tmpnamebase[1]
  names(tmp$esti)[length(tmp$esti)]<-tmpnamebase[length(names(tmp$esti))]

  #browser()
  tmp$esti.org<-tmp$esti
  estibool<- !tmp$esti[1,]==0
  tmp$esti<-tmp$esti[,estibool]
  if ( hist ) {
    tmp$esti.hist<-tmp$esti.hist[,estibool]
    names(tmp$esti.hist)<-names(tmp$esti)
  }
  if (  !is.na(tmp$esti[2,1]) ) {
    tmp$esti[2,tmp$esti[2,]==1e+10]<-NA
  }

 tmp$files$nP<-length(tmp$esti[1,]) -2
 tmp$files$nPSE<-NA
 #browser()
 if (length(tmp$esti[2,][!is.na(tmp$esti[2,])]) -2 > 0 ) {
    tmp$files$nPSE<-length(tmp$esti[2,][!is.na(tmp$esti[2,])]) -2
 }
  ## extracting and loading used method into DataFrame
  ## only on shrinkage  TRUE

  cmd=paste("cd ",path,"; egrep 'TABLE'  *.ext | tail -n 1 | awk -F ': ' '{print $2}'",sep="")
  tmp$METH<-system(cmd,intern=T)
  #cmd=paste("cd ",path,"; egrep 'NO. OF FUNCTION EVALUATIONS USED:'  *.ext | tail -n 1 | awk -F ': ' '{print $2}'",sep="")
  #tmp$EVAL<-system(cmd,intern=T)

  ### creating ETABAR.txt  ETASHRINK.txt  SIGSHRINK.txt from the report file with extractrep.pl

  #cmd=paste("cd ",path,"; grep ' ETAshrink' -A 0 *.rep | tail -n 1 >ETASHRINK.txt",sep="")
  #do.call(system,list(cmd))
  if (shrinkage==T) {
  cmd=paste("cd ",path,"; extractrep.pl -f *.rep", sep="")
  system(cmd)
  #cmd=paste("cd ",path,"; grep ' EPSshrink' -A 0 *.rep | tail -n 1 >SIGSHRINK.txt",sep="")
  #do.call(system,list(cmd))
  ##browser()
  cmd=paste(path,"/ETASHRINK.txt",sep="")
  if (!is.null(count.fields(cmd))) {
    tmp$shrinketa<-read.table(cmd)[,,drop=F]
    ##browser()
    tmp$shrinketa[1,]<-as.numeric(as.character(tmp$shrinketa[1,]))
    names(tmp$shrinketa)<-tmp$nameset
    tmp$shrinketa.org<-tmp$shrinketa
    ### check for wrong shrinkage
    tmp$shrinketa<- tmp$shrinketa[,!tmp$shrinketa==100&!tmp$shrinketa==0,drop=F]
    tmp$shrinketa<-round(tmp$shrinketa,digits=2)
    ##browser()
  }
  cmd=paste(path,"/SIGSHRINK.txt",sep="")
  if (!is.null(count.fields(cmd))) {
    tmp$shrinksig<-read.table(cmd)[,,drop=F]
    tmp$shrinksig[1,]<-as.numeric(as.character(tmp$shrinksig[1,]))
    names(tmp$shrinksig)<-tmp$nameser
    tmp$shrinksig<-round(tmp$shrinksig,digits=2)
  }
  ##browser()
  }

  cmd=paste("cd ",path,"; grep ' #TERM' -A 60 *.rep | tail -n 60 | grep ' #TERE:' -B 60| egrep '^0|COMPLETED|SIG'",sep="")
  tmp$status<-as.character(as.list(do.call(system,list(cmd,intern=T))))
  cmd=paste("cd ",path,"; grep ' EXIT CODE = 1' PRDERR 2>/dev/null | wc -l",sep="")
  VAL<-as.numeric(system(cmd,intern=T))
  if (VAL>0){
    tmp$status[[length(tmp$status)+1]]<-paste(VAL, " PREDERRS",sep="")
  }
  #cmd=paste("cd ",path,"; grep ' #TERM' -A 30 *.rep | tail -n 30 | grep ' #TERE:' -B 30| egrep 'ETABAR:|SE:|P VAL.:' | awk -F : '{print $2}'>etabar.txt",sep="")
  #system(cmd)
  ### shrinkage = True
  if (shrinkage==T) {
  cmd=paste(path,"/ETABAR.txt",sep="")
  #browser()
  if (!is.null(count.fields(cmd))) {
    #browser()
    tmp$etabar<-read.table(cmd)[,,drop=F]
    names(tmp$etabar)<-tmp$nameset
    dimnames(tmp$etabar)[[1]]<-c("ETABAR","SE","Pval")
    tmp$etabar.org<-tmp$etabar
    #test<<-tmp$etabar
    #browser()
    ## NaN handling by checking that we have no NA in the first LINE
    tmp$etabar<-tmp$etabar[,!tmp$etabar[1,]==0&!is.na(tmp$etabar[1,]),drop=F]
    ### convert to num to bypass NONMEM bug.report
    for ( i in seq(along=dimnames(tmp$etabar)[[2]])) {
      tmp$etabar[,i]<-as.numeric(as.character(tmp$etabar[,i]))
    }
  }
  }
  ##browser()
  tmp$files$PATH<-storepath
  cmd<-paste("cd ",path,"; egrep -iv '^[[:space:]]+;|^;' *.con | grep '$PRO' | perl -ne   's/\\$PR[A-Z]+\\s+//;print $_'",sep="")
  tmp$files$PROBLEM<-as.character(as.list(do.call("system",list(cmd,intern=T))))
  if (length(tmp$files$PROBLEM)==0) {
    tmp$files$PROBLEM<-NA
  }
  ## res.comp creation
  #browser()
  #if ( !is.na(tmp$esti[2,1]) ) {
  #  if ( tmp$esti[2,1]==-1000000001)  {
  #    tmp$status[1]<-"0MINIMIZATION SUCCESSFUL"
  #    cmd=paste("cd '",path, "'; grep '0MINIMIZATION' *.rep",sep="")
  #    MINIMIZATION<-system(cmd,intern=TRUE)
  #    if ( length(MINIMIZATION > 0)) {
  #      tmp$status[1]<-MINIMIZATION
  #    }
  #  }
  #}
  ### Begin res.comp
  #browser()
  res.comp<-tmp$esti[1,c(-1,-dim(tmp$esti)[2]),drop=F]
  ##res.comp<-tmp$esti[1,c(-1),drop=F]
  ## rename ERR too sigma²()
  #browser()
  namestochange<-dimnames(res.comp)[[2]][dimnames(res.comp)[[2]] %in% as.character(tmp$namescorer)]

  dimnames(res.comp)[[2]][dimnames(res.comp)[[2]] %in% as.character(tmp$namescorer)]<-paste("sigma²(",namestochange,")",sep="")
  ## LAST POSITION IS BETTER, BECAUSE the OBJ name changed by method
###  objlist<-as.list(tmp$esti$OBJ[1])
  objlist<-as.list(tmp$esti[1,c(dim(tmp$esti)[2]),drop=F])
  res.comp.list<-list()
  #browser()
  if ( ! length(objlist)==0 ) {
    tmp$esti[[names(objlist)[[1]]]][1]<-as.numeric(as.character(tmp$esti[[names(objlist)[[1]]]][1]))
    #browser()
    #objlist[[1]][1]
    objlist[[1]][1]<-round(as.numeric(as.character(objlist[[1]][1])),digits=2)
    #not needed because is named list
    #names(objlist)<-"OBJ"
    res.comp.list<-c(objlist,as.list(res.comp))
  }
  ##browser()
## for SAEM not needed
#objlistsaem<-as.list(tmp$esti$SAEMOBJ[1])
#  if ( ! length(objlistsaem)==0 ) {
#    tmp$esti$SAEMOBJ[1]<-as.numeric(as.character(tmp$esti$SAEMOBJ[1]))

#    objlistsaem[[1]][1]<-round(tmp$esti$SAEMOBJ[1],digits=2)
#    names(objlistsaem)<-"OBJSAEM"
#    res.comp.list<-c(objlistsaem,as.list(res.comp))
#  }



  temp<-tmp$esti[2,-1,drop=F]
  #browser()
  if ( FALSE %in% is.na(temp) )  {

    BOOL<-(names(temp) %in% tmp$namesth)
    THETACV<-round(abs((temp[,BOOL,drop=F]/res.comp[,BOOL])*100),digits=2)
    names(THETACV)<-paste("CV(",names(THETACV),")",sep="")
    res.comp.list<-c(res.comp.list,as.list(THETACV))
    BOOL<-names(temp) %in% tmp$namescoret
    #browser()
    if ( dim(temp[,BOOL,drop=F])[2]!=0 ) {
    #browser()
      ETACV<-round(abs((temp[,BOOL]/res.comp[,BOOL,drop=F])*100),digits=2)
      names(ETACV)<-paste("CV(",names(ETACV),")",sep="")
      res.comp.list<-c(res.comp.list,as.list(ETACV))
    }
    BOOL<-names(temp) %in% paste(tmp$nameser,":",tmp$nameser,sep="")
    VALINT<-temp[,BOOL,drop=F]
    ##VALINT[VALINT==1e+10]<-NA ## not needed done before for all sem
    ERCV<-try(round(abs(VALINT/res.comp[,BOOL,drop=F]*100),digits=2))
    #names(ERCV)<-sub(":[A-Z]+","",names(ERCV))
    names(ERCV)<-try(paste("CVsigma²(",names(ERCV),")",sep=""))
    #browser()
    if (class(ERCV)!="try-error"){
      res.comp.list<-c(res.comp.list,as.list(ERCV))
    }
  }
    #browser()
  if (shrinkage==T) {
    shrink<-tmp$shrinketa
    if ( !is.null(shrink)) {
    if ( dim(shrink)[2]!=0 )  {
      ## testen wenn matrix 0
      ##browser()
      names(shrink)<-paste("omegaSHRINK(",names(shrink),")",sep="")
      res.comp.list<-c(res.comp.list,as.list(shrink))
    }
    }
    shrink<-tmp$shrinksig
    if ( !is.null(shrink)) {
    if ( dim(shrink)[2]!=0 ) {
      names(shrink)<-paste("sigmaSHRINK(",names(shrink),")",sep="")
      res.comp.list<-c(res.comp.list,as.list(shrink))
    }
    }
  }
    ##browser()
    #status<-as.data.frame(tmp$status,stringsAsFactors=F)
    #dimnames(status)[[2]]<-2
    #status<-t(status)
    #status<-as.data.frame(status,stringsAsFactors=F)
    #dimnames(status)[[2]]<-paste("status",dimnames(status)[[2]],sep="")
    #status$Method<-tmp$METH[1]
    #FILES<-as.data.frame(tmp$files,stringsAsFactors=F)
    statuslist<-list()
    if ( ! length(tmp$status)==0 ) {
      statuslist<-as.list(tmp$status)
      names(statuslist)<-paste("STATUS",1:length(statuslist),sep="")
    } else {
      statuslist[["STATUS1"]]<-"0PROGRAM TERMINATED"
    }
    #statuslist[["STATUS1"]]<-"0PROGRAM TERMINATED"
    methlist<-list()
    methlist[["METHOD"]]<-tmp$METH
    #browser()
    eigenlist<-list()
    eigenlist[["EIGENRATIO"]]<-tmp$esti[4,2]

    corr96list<-list()
    corr96list[["CORRELATION"]]<-NA
    if ( ! is.null(tmp$cor96)) {
      corr96list[["CORRELATION"]]<-"greater 0.96"
    }
  ##dimnames(EIGEN)[[2]]<-fitbase
    INFO<-c(tmp$files,methlist,statuslist,eigenlist,corr96list)
    #browser()
    res.comp.list<-c(INFO,res.comp.list)
    #browser()
  if (shrinkage==T) {
    ETABAR<-tmp$etabar[1,,drop=F]
    if (!is.null(ETABAR)) {
    if ( dim(ETABAR)[2]!=0 ) {
      names(ETABAR)<-paste("ETABAR(",names(ETABAR),")",sep="")
      res.comp.list<-c(res.comp.list,as.list(ETABAR))
    }
    }

      #browser()

    ETAPVAL<-tmp$etabar[3,,drop=F]
    if (!is.null(ETAPVAL)) {

    if ( dim(ETAPVAL)[2]!=0 ) {

      #ETAPVALround<-as.data.frame(t(as.data.frame(as.numeric(as.character(ETAPVAL)))))
      #dimnames(ETAPVALround)<-dimnames(ETAPVAL)
      #browser()


      ETAPVALround<-round(ETAPVAL,digits=2)
      ##browser()
      names(ETAPVALround)<-paste("ETAPVAL(",names(ETAPVAL),")",sep="")
      ##browser()
      res.comp.list<-c(res.comp.list,as.list(ETAPVALround))
    }
    }
  }
    #browser()
    tmp$res.comp<-as.data.frame(res.comp.list,stringsAsFactors=F)
    #browser()
    dimnames(tmp$res.comp)[[2]]<-names(res.comp.list)
    cmd<-paste("cd ",path,"; head catab",num, " -n 4  2>/dev/null | grep -iv TABLE >catab",num,".tmp",sep="")
    #browser()
    #eval(parse(text=cmd))
    #browser()
    system(cmd)
    #browser()
    cmd<-paste("cd ",path,"; head cotab",num, " -n 4 2>/dev/null| grep -iv TABLE >cotab",num,".tmp",sep="")
    #browser()
    #eval(parse(text=cmd))
    system(cmd)
    #browser()
    ftmpname<-paste(path,"/catab",num,".tmp",sep="")
    ftmp<-file.info(ftmpname)
    #browser()
    if ( ftmp[1,1]>0 ) {ls()

      CATAB<-try(getN(path=path,file=paste("catab",num,".tmp",sep="")))
      #BOOLCATAB<-eval(parse(text=paste("xposeobj@Prefs@Xvardef$covariates",sep=""))) %in% dimnames(CATAB)[[2]]
      BOOLCATAB<-xposeobj@Prefs@Xvardef$covariates %in% dimnames(CATAB)[[2]]
      #tmp$catabnames<-eval(parse(text=paste("xposeobj@Prefs@Xvardef$covariates",sep="")))[BOOLCATAB]
      tmp$catabnames<-xposeobj@Prefs@Xvardef$covariates[BOOLCATAB]
      #browser()

    }
    #browser()

    ftmpname<-paste(path,"/cotab",num,".tmp",sep="")
    ftmp<-file.info(ftmpname)
    if ( ftmp[1,1]>0 ) {
      COTAB<-try(getN(path=path,file=paste("cotab",num,".tmp",sep="")))
      #BOOLCOTAB<-eval(parse(text=paste(xposeobj,"@Prefs@Xvardef$covariates",sep=""))) %in% dimnames(COTAB)[[2]]
      BOOLCOTAB<-xposeobj@Prefs@Xvardef$covariates %in% dimnames(COTAB)[[2]]
      #tmp$cotabnames<-eval(parse(text=paste(xposeobj,"@Prefs@Xvardef$covariates",sep="")))[BOOLCOTAB]
      tmp$cotabnames<-xposeobj@Prefs@Xvardef$covariates[BOOLCOTAB]
      #browser()
    }
    cmd=paste("rm ",path,"/*.tmp ",path,"/ETA*.txt ",path,"/SIGSHRINK.txt ",path,"/*.res"," 2>/dev/null",sep="")
    #browser()
    system(cmd)
    #browser()
  return(tmp)
###Returns a list to be stored in the XPOSE object @Esti
###
###[[namesth]]    THETA name vector out of the con file
###
###[[nameset]]    ETA name vector out of the con file
###
###[[nameser]]    ERROR name vector out of the con files
###
###[[namescor]]   CORRELATION name vector out of the con file corresponding to the *.cor file in NONMEM7
###
###[[namescorer]]    CORRELATION name vector (only ERROR part) out of the con file corresponding to the *.cor file in NONMEM7
###
###[[namescoret]]    CORRELATION name vector (only ETA part) out of the con file corresponding to the *.cor file in NONMEM7
###
###[[esti]]    data frame corresponding to the *.err file in NONMEM7
###
###[[files]]    list of useful informations as archive number,name of control and dat file, number of Observation and IDs, etc.
###
###[[METH]]    Method used in the fit
###
###[[shrinketa]]   shrinkage for the ETA
###
###[[shrinksig]]   shrinkage for the Epsilonls()
###
###[[status]]   useful status info's
###
###[[etabar]]   ETAbar of the fit
###
###[[res.comp]]   dataframe to bind with other to create a compare results dataframe
###
###[[initialTHETA]]   starting values of THETA taken from the rep file
}

