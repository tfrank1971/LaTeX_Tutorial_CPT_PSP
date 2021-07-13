#' @title Creating LaTeX tables containing data listings
#' @description Facilitates the construction of LaTeX tables containing data lists.
#' @details The resulting LaTeX table is automatically rendered as pdf file, which is written together with the *.tex file at the same position in the file system, so that the author may see and check the results immediately.
#'
#' @param x a data.frame to be used.
#' @param path  a character string giving the name of the directory, where the LaTeX tables should be written. The default corresponds to the current working directory.
#' @param subdir writes the LaTeX tables in the given subdirectory (optional).
#' @param filename usually, a character string giving the name of the LaTeX file (without name ending) to which the summary statistics should be written. The filename ending ".tex" is automatically added.
#' @param variables character vector defining variables that should be included in the table. Per default, all numeric or factor variables of data are used, depending on type.
#' @param variablelabels labels for the variables. Per default variable names are used as labels.
#' @param digits numeric vector of indicating the number of digits to display in the corresponding columns ("3").
#' @param celldigits number of decimal places to round to. Default is variable length plus 1.
#' @param cellalign alignment of table cells ("c").
#' @param size LaTeX fontsize specifier.
#' @param caption character vector specifying the table's caption; see \code{\link{xtable}} for details.
#' @param label character string specifying the LaTeX label ; see \code{\link{xtable}} for details.
#' @param ... Additional arguments. (Currently ignored.)
#'
#' @return Returns *.tex and *.pdf files.
#' @export
#'
#' @examples num=46892
#' cmd = paste("nca",num,sep='')
#' datalist(eval(parse(text=cmd)), filename='ListingNCA' ,variablelabels=c("ID" , "OCC","Cmax (ng/L)","$t_{max} (h)$","$AUC_{inf} (ng/L.h)$" , "$t_{1/2}(h)$","DGRP"), caption = "Listing of the individual PK parameters", label="tab:ListNCATable1",subdir= paste("nmntmp", num,"/tables",sep = ""))
datalist <- function
(x,
path=".",
subdir="tables",
filename,
variables=names(x),
variablelabels=names(x),
digits=3,
celldigits=NULL,
cellalign=NULL,
size="footnotesize",
caption = "My caption",
label="tab:MyTable1",
...
){

  X<-c(filename)
  nameextract <- grep("*[.]tex$", X)
  if (any(!is.na(nameextract))) stop("Dont' give *.tex file extension!")

  local.df <- x[,variables]
  if (is.null(celldigits)) celldigits=rep(1,length.out=length(variables)+1) else celldigits=celldigits
  if (is.null(cellalign)) cellalign=rep("c",length.out=length(variables)+1) else cellalign=cellalign

  names(local.df) <- variablelabels

  if(!is.null(local.df[["LABL"]]))  {
      local.df[["LABL"]] <- unlist(lapply(local.df[["LABL"]], gsub, pattern = "_", replacement = "\\_", fixed = TRUE))
   }

  local.df <- xtable::xtable(local.df, digits = digits, caption = caption, label=label) ## follow exactly the example in the header of the function, otherwise the caption will not
  align(local.df) <- cellalign ## make sure factor levels don't take up more than x cm

  addtorow          <- list()
  addtorow$pos      <- list()
  addtorow$pos[[1]] <- c(0)
  addtorow$command  <- c(paste("\\hline \n",
                               "\\endfirsthead \n",
                               "\\caption[]{\\em (continued)} \\\\ \n",
                               "\\hline \n",
                               paste(paste(variablelabels, collapse = " & "), " \\\\ \n", sep = ""),
                               "\\hline \n",
                               "\\endhead \n",
                               "\\hline \n",
                               "{\\footnotesize Continued on next page} \n",
                               "\\endfoot \n",
                               "\\endlastfoot \n",sep=""))


  print(local.df,  sanitize.text.function = function(x){x}, caption.placement="top", tabular.environment = "longtable", include.rownames=FALSE, size=size, floating=FALSE, #
        file=  paste(path,"/",subdir,"/",filename,".tex",sep=""),
        #hline.after=NULL, #
        #add.to.row=list(pos=list(-1,0, nrow(local.df)), #
        add.to.row = addtorow,      # this is where you actually make the substitution
        #command=c('\\toprule ', # use nice booktabs formatting
        #          '\\midrule \\endhead ', # with running headers from longtable
        #          '\\bottomrule ')))
        hline.after=c(-1))         # because addtorow will substitute the default hline for the first row


  # create a pdf of the table 'on-the-fly'--------------------------------

  if(path==".")path = getwd() else path=path
  path=paste(path,subdir,sep="/")

  #use echo to include begin table text
  texbegin <- character()
  texbegin[1] <- "\\documentclass[a4paper,10pt]{article}"
  texbegin[2] <- "\\usepackage[T1]{fontenc}"
  texbegin[3] <- "\\usepackage[utf8x]{inputenc}"
  texbegin[4] <- "\\usepackage{lmodern}"
  texbegin[5] <- "\\usepackage{amsmath}"
  texbegin[6] <- "\\usepackage{booktabs}"
  texbegin[7] <- "\\usepackage{longtable}"
  texbegin[8] <- "\\usepackage{pdflscape}"
  texbegin[9] <- "\\begin{document}"
  texbegin[10] <- "\\begin{landscape}"

  textables <- character()
  textables[1] <- paste("\\input{",filename,"}", sep = "")
  texend <- character()
  texend[1] <- "\\end{landscape}"
  texend[2] <- "\\end{document}"
  texlist <- appendList(texbegin, textables)
  texlist <- appendList(texlist, texend)
  texfile <- paste(path, "/run", filename, ".tex", sep = "")
  write(texlist, file = texfile)


  #include original table
  cmd = paste("cd ", path, "; pdflatex run",filename,".tex", "; pdflatex run",filename, ".tex", "; rm run",filename,".aux", "; sleep 1 ;", sep = "") # compile 2 times to get table cell alignment right
  system(cmd)

  message("... table successfully created!")


}

# end datalist()

