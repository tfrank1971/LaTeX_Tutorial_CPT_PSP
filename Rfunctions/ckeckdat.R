checkdat <- function#Check the data for the delimiter
###Accepts the delimiters comma, tab, blank and detects the type used. Useful for NONMEM data files
###FunctionGroup:
###DatamanagementTool
(file,
###Name of the checked file
path=".",
###Path to the file. Full path is the best.
skip=0,
###How many lines to skip before data are coming
comment.char = "#"
###Default is "#"
){
  ## check data
  ##browser()
  
  file=paste(path,"/",file,sep="")
  if (file.exists(file)) {
  CSV<-unique(count.fields(file=file,sep=",",skip=skip,comment.char=comment.char))
  TAB<-unique(count.fields(file=file,sep="\t",skip=skip,comment.char=comment.char))
  BLANK<-unique(count.fields(file=file,sep="",skip=skip,comment.char=comment.char))
  if ( CSV[1]!=1 & length(CSV)==1 ) {
    val=","
  }
  if ( BLANK[1]!=1 & length(BLANK)==1 ) {
    val=""
  }  

  if ( TAB[1]!=1 & length(TAB)==1) {
    val="\t"
  }
  if ( !exists("val")) {
    val="bad"
  }
  return(val)
  }
  else {
  return("bad")
  }
###Returns the delimiter or bad if the delimiter is not accepted
}
