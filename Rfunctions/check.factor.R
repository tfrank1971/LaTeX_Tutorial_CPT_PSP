check.factor<-function #tests data from an XPOSE object in order to correct factor settings
###FunctionGroup:
###PopPKCalculation
(num,
###The number of the archive.
yesfactor=c("EVID","MDV"),
### known items to be a factor
nofactor=c("ID","DV","CONC","TIME","TAD")
### known items not to be a factor
) 
{
  require(plyr)
  ### which columns should be numeric
  #browser()
  nofactor<-unique(c(nofactor,eval(parse(text=paste("fit",num,"@Esti$cotabnames",sep=""))),eval(parse(text=paste("fit",num,"@Prefs@Xvardef$parms",sep="")))))
  ### which numeric columns are used
  nofactor.used<- nofactor[nofactor %in% names(eval(parse(text=paste("fit",num,"@Data",sep=""))))]
  ## names of columns as.factor in the data set
  factor.used<-names(colwise(nlevels)(eval(parse(text=paste("fit",num,"@Data",sep="")))))[colwise(nlevels)(eval(parse(text=paste("fit",num,"@Data",sep=""))))>0]
  ## set the wrong factors to numeric
  if (length(factor.used[factor.used %in% nofactor.used])!=0) {
    for (i in factor.used[factor.used %in% nofactor.used]) {
      cmd=paste("fit",num,"@Data[[i]]<-as.numeric(as.character(fit",num,"@Data[[i]]))",sep="")
      #print(cmd)
      print(paste("set from factor to numeric on:",i,sep=""))
      eval(parse(text=cmd))
    } 
  }  
  ## check if all columns known as factor are factor
  yesfactor<-c(yesfactor,eval(parse(text=paste("fit",num,"@Esti$catabnames",sep=""))))
  ### which factor columns are used in the data set
  yesfactor.used<-yesfactor[yesfactor %in% names(eval(parse(text=paste("fit",num,"@Data",sep=""))))]
  ## names of columns as.factor in the data set
  factor.used<-names(colwise(nlevels)(eval(parse(text=paste("fit",num,"@Data",sep="")))))[colwise(nlevels)(eval(parse(text=paste("fit",num,"@Data",sep=""))))>0]
  if (length(!(yesfactor.used %in% factor.used)!=0)) {
    for (i in yesfactor[(!(yesfactor.used %in% factor.used))]) {
     cmd=paste("fit",num,"@Data[[i]]<-as.numeric(as.character(fit",num,"@Data[[i]]))",sep="") 
     #print(cmd)
     #print(i)
     print(paste("set factor on:",i,sep=""))
     eval(parse(text=cmd))
     }
  }
  cmd=paste("fit",num,"@Data<<-fit",num,"@Data",sep="")
  eval(parse(text=cmd))
}
