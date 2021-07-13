plotlist<-function#Plots a list of plots
###Plots a list of plots, created by other functions like createplotlist
###FunctionGroup:
###Tools
(lst,
###The list of plots
file="unknown",
###Default name for the plots (if not overwritten by attribute psfile in the plot
pdfsplit=F,
###Splits into pdf files
pdf=T,png=F,
###Bool if pdf is requested
dens="600",
###Density of png conversion's from ps to png
subdir="doc/plots",
###Default doc/plots relative to the actual working dir
path=getwd(),
###Default path to create a full file destination to avoid getwd() in makeFootnote
paper="special",
###The target paper size. The choices are "a4", "letter", "legal" (or "us") and "executive" (and these can be capitalized), or "a4r" and "USr" for rotated (‘landscape’). The default is "special", which means that the width and height specify the paper size. A further choice is "default"; if this is selected, the papersize is taken from the option "papersize" if that is set and as "a4" if it is unset or empty. Defaults "special".
width=7,
###The width and height of the graphics region in inches. The default values are 7.
height=7,
###The width and height of the graphics region in inches. The default values are 7.
footnoteText = paste(Sys.getenv("USER"),"; ", path, "/",subdir,"/", file, "; ", format(Sys.time(),"%d %b %Y %T"), sep = ""),
###footnoteText as a parameter
size=0.4
### size parameter for makeFootnote.R
) {
#require(popnew)
## create dir in any case
cmd=paste("mkdir -p ",subdir, sep="")
system(cmd, intern=FALSE,ignore.stdout = TRUE, ignore.stderr = TRUE)
footnoteText.call<-footnoteText
if ( pdf ) {
  pdf(file=paste(subdir,"/",file,".pdf",sep=""),paper=paper,width=width,height=height)

  for ( i in seq(along=lst)) {
    print(lst[[i]])
    filefin=paste(file,".pdf [",i,"]",sep="")
    footnoteText<-sub(file,filefin,footnoteText.call)
    makeFootnote(footnoteText = footnoteText,size=size)
  }
  dev.off()
}
  ##browser()
#### the split option for pdf
  if ( pdfsplit | png ) {
    for ( i in seq(along=lst)) {
      filenew=paste(subdir,"/",file,".",i,sep="") 
      #browser()

      ### read filename out of the plot if defined
      text1<-attr(lst[[i]],"psfile")
      if (is.null(text1)) {
	#res<-strsplit(text1,"/")
        #filebase<-res[[1]][length(res[[1]])]
        #substring2(filebase,".eps")<-""
        ##browser()
        #text2<-attr(lst[[i]],"realcall")
      }
      if ( !is.null(text1) ) {  
        ### this is needed for old trellis plots from aventis   
  	res<-strsplit(text1,"/")
        filebase<-res[[1]][length(res[[1]])]
        substring2(filebase,".eps")<-""
        ##browser()
        text2<-attr(lst[[i]],"realcall")

        textnew1<-text1
	substring2(textnew1,".eps")<-""
        filenew=paste(subdir,"/",filebase,sep="")
        if ( !is.null(text2) ) {
	  textnew2<-first.word(substring2(text2[1],substring.location(text2[1],"fit")$first))
	  filenew=paste(subdir,"/",filebase,".",textnew2,sep="")                
	  }
	}
        if ( pdfsplit ) {
          ##browser()
	  filefin=paste(filenew,".pdf",sep="")
	  pdf(file = filefin,paper=paper,width=width,height=height)
	  print(lst[[i]])
	  footnoteText<-sub(file,filefin,footnoteText.call)
	  makeFootnote(footnoteText = footnoteText, size=size)
	  dev.off()
	  }
        
	if ( png ) {
          #browser()
	  filefin=paste(filenew,".ps",sep="")
          filefinpng=paste(filenew,".png",sep="")
          ##browser()
          
          #setEPS(reset=T)
	  cairo_ps(file = filefin)
	  print(lst[[i]])
	  footnoteText<-sub(file,filefinpng,footnoteText.call)
	  makeFootnote(footnoteText = footnoteText,size=size)
	  dev.off()
          cmd=paste("convert -density ",dens," ", filefin, " -flatten -trim ",filefinpng, ";rm ", filefin, sep="")
          ##browser()
          system(cmd)
	  }
      }
    }
###No return, but reads the attributes psfile and realcall (defined in aventis plots) and plots in the filesystem
}
