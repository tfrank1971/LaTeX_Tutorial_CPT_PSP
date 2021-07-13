getN<-function#Reads NONMEM7 data and table files
###With the help of the internal function checkdat this function is able to read csv,TAB,comma separated NONMEM7 files
###FunctionGroup:
###PopPKCalculation
(file="",
###The name of the file as character.
path=".", 
###Where to find the file in the file system
header = TRUE, 
###Default is True, reading the first line as header
sep = ",", 
###Default is , (, or ; or tab) can be detected 
quote="\"", 
###Default "
dec=".",
###Default "."
fill = TRUE, 
###Default is True
comment.char="#",
###Default is "#"
na.strings = c("N" , "."), 
###Default in N or "."
skip=0, 
###Default is 0
anr=NULL,
###read from archive instead of the file system the archive is defined by the anr (archive number). The Default reads the file from the file system.
...
###Additional parameters for read.table()
){
  CALL <- match.call()
  #browser()
  pathfinal=path
  fullfile<-paste(pathfinal,"/",file,sep="")
  
  if (!is.null(anr)) {
    ## memorising the working directory
    WD<-getwd()
    NUM<-sample(1:1000)[1]
    
    ##path detection 
    if ( path=="." ) {
      apath=paste(WD,"/nmnqs",sep="")
      storepath=WD
      DIRTEMP=paste(storepath, "/nmntmp",NUM,sep="")
      pathfinal=DIRTEMP
      fullfile<-paste(DIRTEMP,"/",file,sep="")
    } else {
      ### check for realative path
      if (length(grep("^/",path)==1)==0) {
	      apath=paste(WD,"/",path,"/nmnqs",sep="")
	      storepath=paste(WD,"/",path,sep="")
	      DIRTEMP=paste(storepath, "/nmntmp",NUM,sep="")
              pathfinal=DIRTEMP
              fullfile<-paste(DIRTEMP,"/",file,sep="")
      } else {
	      apath=paste(path,"/nmnqs",sep="")
	      storepath=path
	      DIRTEMP=paste(storepath, "/nmntmp",NUM,sep="")
              pathfinal=DIRTEMP
              fullfile<-paste(DIRTEMP,"/",file,sep="")
      }
      
    }
 
 #browser()
 ## check remote operation and run if possible speed up because no network operation on tar
  cmd=paste("TT=`df -t nfs ",apath, "| grep :`; echo ${TT%%:*}",sep="")
  HOST=system(cmd,intern=T)
  DIRTEMP=paste(storepath, "/nmntmp",NUM,sep="")

  if ( nchar(HOST) > 0 ) {
    #cmd=paste("ssh ",HOST, " 'rm -r ",DIRTEMP, " 2>/dev/null; mkdir ",DIRTEMP, "; cd ",DIRTEMP,"; tar -xzf ", apath, "/*.",anr,".tar.gz 1>/dev/null'",sep="")
    cmd=paste("rm -r ",DIRTEMP, " 2>/dev/null; mkdir ",DIRTEMP, "; cd ",DIRTEMP,"; tar -xzf ", apath, "/*.",anr,".tar.gz 1>/dev/null",sep="")
    system(cmd,intern=T)
    
  } else {
    ## temp dir for tar operations
    #browser() 
    cmd=paste("rm -r ",DIRTEMP, " 2>/dev/null; mkdir ",DIRTEMP, "; cd ",DIRTEMP,"; tar -xzf ", apath, "/*.",anr,".tar.gz 1>/dev/null",sep="")
    system(cmd,intern=T)
  } 
  
 
 
    ## temp dir for tar operations
    #browser()
    

    #browser()
    
  }
  
  #browser()
  sep=checkdat(file,path=pathfinal,skip=skip,comment.char=comment.char)
  

  
  ## bad as return from checkdata, if data is not a matrix else the sep detected ("" or "\t" or ";")
  if ( sep!="bad" ){
    ##browser()
    data <- read.table(file=fullfile, header = header, sep = sep, quote="\"", dec=".",fill = TRUE,na.strings = c("N", "."),skip=skip,comment.char = comment.char, ...)
    attr(data, "call") <- CALL
    if (!is.null(anr)){
      cmd<-paste("rm -r ",DIRTEMP,sep="")
      system(cmd)
    }
    
    invisible(data)
  } else {
    message("Data not a Matrix")
    if (!is.null(anr)){
      cmd<-paste("rm -r ",DIRTEMP,sep="")
      system(cmd)
    }
  }
###Returns a data.frame
}
