printnmr7<-function#Prints summary of fit
###Creates several types of output for the summary of a fit.
###FunctionGroup:
###PopPKCalculation
(obj,
###The fit object run<run number> The output of getnmn7.
pdf=T,
###True for Latex output.
rtf=F,
###True for rtf output. Creates also tex output.
digits=NULL,
###The digits of the output table. A negative value forces scientific output. Set formatted = F, if digits is used.
path="doc/tables",
###Default ./doc/tables is the directory where the plots will be created.   
rmcaption=T,
###True as default. Removes caption from tex table output after the creation of PDF and rtf.
rmtable=T,
###True as default. Removes the latex table environment after the creation of PDF and rtf.
formatted=T,
###True as default. Converts the dataframe to character and print is as they are. Works with digits=NULL. The value digits is ignored.
size="scriptsize",
###The latex code "scriptsize" is the default.
ltheta=F,
###not used in the moment.
cvrename=T
###rename CV into RSE which is more standard
) {
  require("xtable")
  ##define useful variable
  fitbase<-obj@Esti$files$ARCHIVE
  BASEINFO<-c("ARCHIVE","CONFILE","DATFILE","OBS","IDS","PATH","PROBLEM")
  BOOL<-names(obj@Esti$res.comp) %in% BASEINFO
  baseinfo<-as.data.frame(t(obj@Esti$res.comp[,BOOL,drop=F]))  
  dimnames(baseinfo)[[2]]<-"Base Info"
  baseinfo[,1]<- as.character(baseinfo[,1])
  RUNINFO<-c("METHOD","STATUS1","STATUS2","EIGENRATIO","CORRELATION","OBJ","OBJSAEM","SAEMOBJ")
  BOOL<-names(obj@Esti$res.comp) %in% RUNINFO
  runinfo<-as.data.frame(t(obj@Esti$res.comp[,BOOL,drop=F]))
  dimnames(runinfo)[[2]]<-"Run Info"
  runinfo[,1]<- as.character(runinfo[,1])

  ### THETA Dataframe
  BOOLTH<-names(obj@Esti$res.comp) %in% obj@Esti$namesth
  tmptheta<-as.data.frame(t(obj@Esti$res.comp[,BOOLTH,drop=F]))
  #browser()
  dimnames(tmptheta)[[2]]<-"THETA"
  if ( !is.na(obj@Esti$esti[2,1])) {
    BOOLTH<-names(obj@Esti$esti) %in% obj@Esti$namesth
    CMD<-as.data.frame(t(obj@Esti$esti[2,BOOLTH,drop=F]))
    dimnames(CMD)[[2]]<-"SE"
    tmptheta[["SE"]]<-CMD[,1]
    BOOLTH<-dimnames(obj@Esti$res.comp)[[2]] %in% paste("CV(",obj@Esti$namesth,")",sep="")
    CMD<-as.data.frame(t(obj@Esti$res.comp[,BOOLTH,drop=F]))
    #browser()
    dimnames(CMD)[[2]]<-"CV(%)"
    ## ist dies nötig ich glaube nicht 3.2.2011
    try(dimnames(CMD)[[1]]<-obj@Esti$namesth)
    tmptheta[["CV(%)"]]<-CMD[,1]
    tmptheta$LO<-tmptheta$THETA - 2*(tmptheta$SE)
    tmptheta$UP<-tmptheta$THETA + 2*(tmptheta$SE)
  }
  if ( ltheta==T ) {
    tmptheta[["THETA"]]<-exp(tmptheta[["THETA"]])
    tmptheta[["LO"]]<-exp(tmptheta[["LO"]])
    tmptheta[["UP"]]<-exp(tmptheta[["UP"]])
    names(tmptheta)[1]<-"exp(THETA)"
  }
  ### ETA Dataframe
  #browser()
  BOOLET<-names(obj@Esti$res.comp) %in% obj@Esti$namescoret
  
  tmpeta<-as.data.frame(t(obj@Esti$res.comp[,BOOLET,drop=F]))
  dimnames(tmpeta)[[2]]<-"OMEGA"
  tmpetadiagnames<-paste(obj@Esti$nameset,":",obj@Esti$nameset,sep="")
  tmpetadiagnames<-tmpetadiagnames[tmpetadiagnames %in% dimnames(tmpeta)[[1]]]
  tmpetadiag<-tmpeta
  dimnames(tmpetadiag)[[2]]<-c("ETA")
  tmpetadiag<-tmpetadiag[dimnames(tmpetadiag)[[1]] %in% tmpetadiagnames,,drop=F]
  tmpetadiag$ETA<-sqrt(tmpetadiag$ETA)
  #browser()
  #browser()
    
  if ( !is.na(obj@Esti$esti[2,1])) {
    BOOLET<-names(obj@Esti$esti) %in% obj@Esti$namescoret
    CMD<-as.data.frame(t(obj@Esti$esti[2,BOOLET,drop=F]))
    dimnames(CMD)[[2]]<-"SE"
    tmpeta[["SE"]]<-CMD[,1]
    BOOLET<-dimnames(obj@Esti$res.comp)[[2]] %in% paste("CV(",obj@Esti$namescoret,")",sep="")
    CMD<-as.data.frame(t(obj@Esti$res.comp[,BOOLET,drop=F]))
    dimnames(CMD)[[2]]<-"CV(%)"

    dimnames(CMD)[[1]]<-obj@Esti$namescoret[obj@Esti$namescoret %in% dimnames(obj@Esti$esti)[[2]]]
    tmpeta[["CV(%)"]]<-CMD[,1]
    tmpeta$LO<-tmpeta$OMEGA - 2*tmpeta$SE
    tmpeta$UP<-tmpeta$OMEGA + 2*tmpeta$SE

    ##dimnames(tmpeta)[[2]][1]<-"omega*omega"
    ### etadiag for sqrt
    
    tmpetadiag<-tmpeta[,c("OMEGA","LO","UP")]
    dimnames(tmpetadiag)[[2]]<-c("ETA","LO","UP")
    tmpetadiag<-tmpetadiag[dimnames(tmpetadiag)[[1]] %in% tmpetadiagnames,,drop=F]
    tmpetadiag$ETA<-sqrt(tmpetadiag$ETA)
    ## to avoid negativ LOWER(ETA)
    tmpetadiag[["LO"]][tmpetadiag[["LO"]]<0]<-NA
    tmpetadiag[["LO"]]<-sqrt(tmpetadiag[["LO"]])
    tmpetadiag[["UP"]]<-sqrt(tmpetadiag[["UP"]])
  }
  #browser()
  if (!is.null(obj@Esti$etabar)){
  if ( dim(obj@Esti$etabar)[2]!=0 ){
    tmpetabar<-as.data.frame(t(obj@Esti$etabar))
  }
  }
  #browser()
  if (!is.null(obj@Esti$shrinketa)) {
  if ( dim(obj@Esti$shrinketa)[2]!=0 ){
    #browser()
    namesshrink<-paste(names(obj@Esti$shrinketa),":",names(obj@Esti$shrinketa),sep="")
    tmpetadiag[["SHRINKAGE(%)"]][dimnames(tmpetadiag)[[1]] %in% namesshrink]<-round(as.numeric(obj@Esti$shrinketa[1,]),digits=2)
  }
  }
  ##browser()
  BOOLER<-dimnames(obj@Esti$res.comp)[[2]] %in% paste("sigma²(",obj@Esti$namescorer,")",sep="")
  tmpsigma<-as.data.frame(t(obj@Esti$res.comp[,BOOLER,drop=F]))
  dimnames(tmpsigma)[[2]]<-"(sigma)²"
  if ( !is.na(obj@Esti$esti[2,1])) {
    BOOLER<-dimnames(obj@Esti$esti)[[2]] %in% obj@Esti$namescorer
    ##browser()
    ##tmpsigma[["SE"]]<-obj@Esti$res.comp[BOOLER][,1]
    tmpsigma[["SE"]]<-as.numeric(obj@Esti$esti[2,BOOLER,drop=F])

    tmpsigma[["CV(%)"]]<-round((tmpsigma[["SE"]]/tmpsigma[["(sigma)²"]])*100,digits=2)

    tmpsigma$LO<-tmpsigma[["(sigma)²"]] - 2*tmpsigma$SE
    tmpsigma$UP<-tmpsigma[["(sigma)²"]] + 2*tmpsigma$SE
  }
  tmpeps<-tmpsigma[,-c(2,3),drop=F]
  ## shrinkage only on diagonal defined
  filtersigmadiag<-paste("sigma²(",obj@Esti[["nameser"]],":",obj@Esti[["nameser"]],")",sep="")
  #browser()
  # drop=F needed that the result is still a data.frame
  tmpeps<-tmpeps[dimnames(tmpeps)[[1]] %in% filtersigmadiag,,drop=F]
  if ( !is.na(obj@Esti$esti[2,1])) {
    dimnames(tmpeps)[[2]]<-c("sigma","LO","UP")
    ## to avoid negativ LO
    tmpeps[["LO"]][tmpeps[["LO"]]<0]<-NA
    tmpeps[["LO"]]<-sqrt(tmpeps[["LO"]])
    tmpeps[["UP"]]<-sqrt(tmpeps[["UP"]])

  } else {
    dimnames(tmpeps)[[2]]<-c("sigma")
  }
  #browser()
  tmpeps[["sigma"]]<-sqrt(tmpeps[["sigma"]])  
  ##dimnames(tmpeps)[[1]]<-paste("sigma(",obj@Esti$nameser,")",sep="")
  nametochange<-dimnames(tmpeps)[[1]]
  nametochange<-gsub("²","",nametochange)
  dimnames(tmpeps)[[1]]<-nametochange
  #browser()
  BOOL<-dimnames(obj@Esti$res.comp)[[2]] %in% paste("sigmaSHRINK(",obj@Esti$nameser,")",sep="")
   if (!is.null(obj@Esti$shrinksig)){
    
    tmpeps[["SHRINKAGE(%)"]]<-round(as.numeric(t(obj@Esti$res.comp[,BOOL,drop=F])),digits=2)[round(as.numeric(t(obj@Esti$res.comp[,BOOL,drop=F])),digits=2)<100]
   }
  if ( ! is.null(obj@Esti$cor96)) {
    tmpcor96<-obj@Esti$cor96
  }
#browser()
  ### printing the results
  dimnames(tmpeta)[[2]][1]<-"(omega)²"
  dimnames(tmpetadiag)[[2]][1]<-"omega"
  if ( exists("baseinfo")) {
    caption=paste("base info of ",fitbase,sep="")
    if (formatted==T) {
      baseinfo<-format(baseinfo,scientific=F)
    }
    cmd=paste("xtmpbaseinfo<-xtable(baseinfo)",sep="")
    eval(parse(text=cmd))
    print(caption)
    print(baseinfo)
  }
  if ( exists("runinfo")) {
    caption=paste("run info of ",fitbase,sep="")
    if (formatted==T) {
      runinfo<-format(runinfo,scientific=F)
    }
    cmd=paste("xtmpruninfo<-xtable(runinfo)",sep="")
    eval(parse(text=cmd))
    print(caption)
    print(runinfo)
  }

  if ( exists("tmptheta")) {
    caption=paste("Fixed Effect Parameters of ",fitbase,sep="")
    if (formatted==T) {
      #browser()
      tmptheta<-format(tmptheta,scientific=F)

      #browser()
    }
   if (cvrename==T) {
      #tmptheta<<-tmptheta
      dimnames(tmptheta)[[2]]<-gsub("CV","RSE",dimnames(tmptheta)[[2]])
    }
    ALIGN=paste(rep("r",dim(tmptheta)[2]+1),collapse = "",sep="")
    cmd=paste("xtmptheta<-xtable(tmptheta, digits=digits,  align='",ALIGN,"')",sep="")
    #browser()
    eval(parse(text=cmd))
    print(caption)
    print(tmptheta)
    
  }

  ##browser()
    if ( dim(tmpetadiag)[1]==0 ) { 
    rm(tmpetadiag)
    rm(tmpeta)
  }

  if ( exists("tmpeta")) {
    #digitsset<-rep(digits,dim(tmpeta)[2]+1)
    #digitsset[3]<-2
    caption=paste("Random Effects (Omegas) of ",fitbase,sep="")
    if (formatted==T) {
      tmpeta<-format(tmpeta,scientific=F)
    }
    if (cvrename==T) {
      dimnames(tmpeta)[[2]]<-gsub("CV","RSE",dimnames(tmpeta)[[2]])
    }
    ALIGN=paste(rep("r",dim(tmpeta)[2]+1),collapse = "",sep="")
    cmd=paste("xtmpeta<-xtable(tmpeta, digits=digits, align='",ALIGN,"')",sep="")
    eval(parse(text=cmd))
    print(caption)
    print(tmpeta)
  }
  ##browser()
  if ( exists("tmpetadiag")) {
    caption=paste("Random Effects (Etas) of ",fitbase,sep="")
    if (formatted==T) {
      tmpetadiag<-format(tmpetadiag,scientific=F)
    }
    ALIGN=paste(rep("r",dim(tmpetadiag)[2]+1),collapse = "",sep="")
    cmd=paste("xtmpetadiag<-xtable(tmpetadiag, digits=digits, align='",ALIGN,"')",sep="")
    eval(parse(text=cmd))	
    print(caption)
    print(tmpetadiag)
  }
  #browser()
  if ( exists("tmpetabar")) {
    caption=paste("ETA statistic of ",fitbase,sep="")
    if (formatted==T) {
      tmpetabar<-format(tmpetabar,scientific=F)
    }
    ALIGN=paste(rep("r",dim(tmpetabar)[2]+1),collapse = "",sep="")
    cmd=paste("xtmpetabar<-xtable(tmpetabar, digits=digits, align='",ALIGN,"')",sep="")
    eval(parse(text=cmd))
    print(caption)
    print(tmpetabar)
  }
  #()browser()
  if ( exists("tmpsigma")) {
    caption=paste("Random Effects (Sigmas) of ",fitbase,sep="")
    if (formatted==T) {
      tmpsigma<-format(tmpsigma,scientific=F)
    }
    if (cvrename==T) {
      dimnames(tmpsigma)[[2]]<-gsub("CV","RSE",dimnames(tmpsigma)[[2]])
    }
    ALIGN=paste(rep("r",dim(tmpsigma)[2]+1),collapse = "",sep="")
    ##captionfit=paste("fit results of run number: ", fitbase, sep="")
    cmd=paste("xtmpsigma<-xtable(tmpsigma, digits=digits, align='",ALIGN,"')",sep="")
    eval(parse(text=cmd))
    print(caption)
    print(tmpsigma)
  }
  if ( exists("tmpeps")) {
    captionfit=paste("fit results of run number: ", fitbase, sep="")

    ##caption=paste("Random Effects (Epsilons) of ",fitbase,sep="")
    if (formatted==T) {
      tmpeps<-format(tmpeps,scientific=F)
    }
    ALIGN=paste(rep("r",dim(tmpeps)[2]+1),collapse = "",sep="")

    cmd=paste("xtmpeps<-xtable(tmpeps, digits=digits, align='",ALIGN,"',caption='",captionfit,"')",sep="")
    eval(parse(text=cmd))
    print(caption)
    print(tmpeps)
  }
 

  #browser()
  ### print tex for pdf and rtf
  if ( (pdf | rtf)==T ) {
    file=paste(path,"/fittable",fitbase,".texraw",sep="")
    fileA=paste(path,"/fittableA",fitbase,".texraw",sep="")
    fileB=paste(path,"/fittableB",fitbase,".texraw",sep="")
    ## DELETE EXITING FILE
    cmd<-paste("rm ",fileB," 2>/dev/null", sep="")
    system(cmd)
    
    if ( exists("xtmpbaseinfo")) {
      print(xtmpbaseinfo,file=file,size=size,latex.environments="flushleft",floating=TRUE)
      print(xtmpbaseinfo,file=fileA,size=size,latex.environments="flushleft",floating=TRUE)
    }
    #file=paste(path,"/fittable",fitbase,".texraw",sep="")
    if ( exists("xtmpruninfo")) {
      print(xtmpruninfo,file=file,size=size,latex.environments="flushleft",floating=TRUE,append=TRUE)
      print(xtmpruninfo,file=fileA,size=size,latex.environments="flushleft",floating=TRUE,append=TRUE)
    }

    #file=paste(path,"/fittable",fitbase,".tex",sep="")
    if ( exists("xtmptheta")) {
      print(xtmptheta,file=file,size=size,latex.environments="flushleft",floating=TRUE,append=T)
      print(xtmptheta,file=fileA,size=size,latex.environments="flushleft",floating=TRUE,append=T)
    }
    #file=paste(path,"/eta",fitbase,".tex",sep="")
    #browser()
    if ( exists("xtmpeta")) {
      print(xtmpeta,file=file,size=size,latex.environments="flushleft",floating=TRUE,append=T)
      print(xtmpeta,file=fileB,size=size,latex.environments="flushleft",floating=TRUE,append=T)
    }
    #file=paste(path,"/etadiag",fitbase,".tex",sep="")
    if ( exists("xtmpetadiag")) {
      print(xtmpetadiag,file=file,size=size,latex.environments="flushleft",floating=TRUE,append=T)
      print(xtmpetadiag,file=fileB,size=size,latex.environments="flushleft",floating=TRUE,append=T)
    }

    if ( exists("xtmpetabar")) {
      print(xtmpetabar,file=file,size=size,latex.environments="flushleft",floating=TRUE,append=T)
      print(xtmpetabar,file=fileB,size=size,latex.environments="flushleft",floating=TRUE,append=T)
    }


    #file=paste(path,"/sigma",fitbase,".tex",sep="")
    if ( exists("xtmpsigma")) {
      print(xtmpsigma,file=file,size=size,latex.environments="flushleft",floating=TRUE,append=T)
      print(xtmpsigma,file=fileB,size=size,latex.environments="flushleft",floating=TRUE,append=T)
    }
    #file=paste(path,"/eps",fitbase,".tex",sep="")
    if ( exists("xtmpeps")) {
      print(xtmpeps,file=file,size=size,latex.environments="flushleft",floating=TRUE,append=T)
      print(xtmpeps,file=fileB,size=size,latex.environments="flushleft",floating=TRUE,append=T)
    }



    ### delete table from file
    cmd=paste("cd ",path,"; echo '\\begin{table}[ht]'>fittable",fitbase,".tex; grep -iv table fittable",fitbase,".texraw>>fittable",fitbase,".tex; echo '\\end{table}'>>fittable",fitbase,".tex ",sep="")
    #browser()
    system(cmd)
    ## splited textables
    cmd=paste("cd ",path,"; echo '\\begin{table}[ht]'>fittableA",fitbase,".tex; grep -iv table fittableA",fitbase,".texraw>>fittableA",fitbase,".tex; echo '\\end{table}'>>fittableA",fitbase,".tex ",sep="")
    #browser()
    system(cmd)
    cmd=paste("cd ",path,"; echo '\\begin{table}[ht]'>fittableB",fitbase,".tex; grep -iv table fittableB",fitbase,".texraw>>fittableB",fitbase,".tex; echo '\\end{table}'>>fittableB",fitbase,".tex ",sep="")
    #browser()
    system(cmd)
   
    texbegin<-character()
    texbegin[1]<-"\\documentclass[a4paper,10pt]{article}"
    texbegin[2]<-"\\usepackage[utf8x]{inputenc}"
    texbegin[3]<-"\\begin{document}"
    textables<-character()
    
    ##browser()
    i=1
    textables[i]<-paste("\\input{fittable",fitbase,"}",sep="")
    
    texend<-character()
    texend[1]<-"\\end{document}"
    texlist<-appendList(texbegin,textables)
    texlist<-appendList(texlist,texend)
    ##browser()
    texfile<-paste(path,"/run",fitbase,".tex",sep="")
    write(texlist,file=texfile)
    if ( (pdf)==T ){
      ##browser() 
      cmd=paste("cd ",path,"; pdflatex run",fitbase,".tex",sep="")
      system(cmd)
    }
    if ( (rtf)==T ){
      cmd=paste("cd ",path,"; latex2rtf run",fitbase,".tex",sep="")
      system(cmd)
    }
  }
  ## not implemented
  #print(xtmptheta,type="html",file="theta.html")
  #print(xtmpeta,type="html",file="eta.html")
  #print(xtmpetadiag,type="html",file="etadiag.html")
  #print(xtmpsigma,type="html",file="err.html")
  #print(xtmpeps,type="html",file="errdiag.html")
  ##
  if ( rmcaption==T ) {
    cmd=paste("cd ",path,"; LL=`ls fittable*",fitbase,".tex`; for i in $LL; do grep -iv caption $i >$i.tmp; mv  $i.tmp $i; done",sep="")
    system(cmd)
  }

  if ( rmtable==T ) {
    cmd=paste("cd ",path,"; LL=`ls fittable*",fitbase,".tex`; for i in $LL; do grep -iv table $i >$i.tmp; mv  $i.tmp $i; done",sep="")
    system(cmd)
  }
###Print the txt output on the console and creates the result in the filesystem
}
