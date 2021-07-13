#' @title Creating LaTeX tables containing descriptive statistics
#' @description Facilitates the construction of LaTeX tables summarizing data.
#' @details The resulting LaTeX table is automatically rendered as pdf file, which is written together with the *.tex file at the same position in the file system, so that the author may see and check the results immediately.
#'
#' @param x a data.frame to be used.
#' @param path a character string giving the name of the directory, where the LaTeX tables should be written. The default corresponds to the current working directory.
#' @param subdir writes the LaTeX tables in the given subdirectory (optional).
#' @param filename usually, a character string giving the name of the LaTeX file (without name ending) to which the summary statistics should be written. The filename ending ".tex" is automatically added.
#' @param type print summary table for either \code{numeric} or \code{factor} variables.
#' @param group character specifying a grouping factor. Per default no grouping is applied.
#' @param variables character vector defining variables that should be included in the table. Per default, all numeric or factor variables of data are used, depending on type.
#' @param quantiles adds the five-number summaries (minimum, 25\% quantile, median, 75\% quantile, maximum) to the table ("TRUE")
#' @param test logical or character string. If a group is given, this argument determines whether a test for group differences is computed. Possible values: c("t.test", "wilcox.test") ("FALSE").
#' @param cumulative logical. Should cumulative fractions be displayed? ("FALSE")
#' @param variable.labels labels for the variables. If \code{variable.labels = NULL} (default) variables is used as label. Instead of \code{variable.labels} one can also use \code{labels}.
#' @param labels labels for the variables. If labels = TRUE, \code{labels(data, which = variables)} is used as labels. If labels = NULL (the default) variables is used as label. \code{labels} can also be specified as character vector.
#' @param digits character vector indicating the number of digits to display in the corresponding columns. For defaults see \code{\link{summarize_numeric}} and \code{\link{summarize_factor}}.
#' @param caption character vector specifying the table's caption; see \code{\link{xtable}} for details.
#' @param label character string specifying the LaTeX label ; see \code{\link{xtable}} for details.
#' @param tabular.environment character string. Per default "tabular" is used. For long tables that span over more than one page, one should use "longtable". For more options see \code{\link{print.xtable}}.
#' @param colnames not used.
#' @param size LaTeX fontsize specifier.
#' @param ... Additional arguments. (Currently ignored.)
#'
#' @return Returns *.tex and *.pdf files.
#' @export
#'
#' @examples data(Orthodont)
#' dataoverview(Orthodont, path="/workfra/trial/tables", subdir=NULL, filename="ausw1", variables=c("distance", "age"))
#' dataoverview(Orthodont, path="/workfra/trial/tables", subdir=NULL, filename="ausw2", group="Sex")
#' dataoverview(Orthodont, path="/workfra/trial/tables", subdir=NULL, filename="ausw5", type="factor", variables="Sex")
#' dataoverview(Orthodont, path="/workfra/trial/tables", subdir=NULL, filename="ausw6", type="factor", variables="Subject", group="Sex")
dataoverview <- function
(x,
 path=".",
 subdir="tables",
 filename,
 type="numeric",
 group = NULL,
 variables=NULL,
 quantiles=TRUE,
 test=FALSE,
 cumulative = FALSE,
 variable.labels=NULL,
 labels=NULL,
 digits = NULL,
 caption=NULL,
 label=NULL,
 tabular.environment=NULL,
 colnames = NULL,
 size="footnotesize",
 ...
){

  X<-c(filename)
  nameextract <- grep("*[.]tex$", X)
  if (any(!is.na(nameextract))) stop("Dont' give *.tex file extension!")

  local.df <- x

  if(!is.null(group))  {
    if (!is.factor(local.df[, group])) {
      factor_cols <- group
      for(i in factor_cols){local.df[,i] <- as.factor(local.df[,i])}
    }
  }

  if(type=="factor")  {
    if (!is.factor(local.df[, variables])) {
      factor_cols <- variables
      for(i in factor_cols) {local.df[,i] <- as.factor(local.df[,i])}
    }
  }


  if(is.null(variables))variables=names(local.df)else variables=variables
  if(is.null(tabular.environment))tabular.environment=getOption("xtable.tabular.environment", "tabular")else tabular.environment=tabular.environment

  floating <- as.logical()
  if(tabular.environment=="longtable"){floating = FALSE} else{floating = TRUE}

if(type=="numeric"){
  sum_table <- try(xtable::xtable(papeR::summarize(local.df, type = type,
                                        variables = variables,
                                        variable.labels = variable.labels, labels = labels,
                                        group = group,
                                        test = test, cumulative = cumulative, colnames = colnames, digits = digits, quantiles = quantiles),
                       caption=caption, label=label))

  invisible(capture.output(print(sum_table, file = paste(path,"/",subdir,"/",filename,".tex",sep=""), booktabs=TRUE, floating = floating, size = size, tabular.environment= tabular.environment, sanitize.text.function = function(x){x}, latex.environments = c("center"))))
}

if(type=="factor"){
  sum_table <- try(xtable::xtable(papeR::summarize(local.df, type = type,
                                        variables = variables,
                                        variable.labels = variable.labels, labels = labels,
                                        group = group,
                                        test = test, cumulative = cumulative, colnames = colnames, digits = digits),
                       caption=caption, label=label))

  invisible(capture.output(print(sum_table, file = paste(path,"/",subdir,"/",filename,".tex",sep=""), booktabs=TRUE, floating = floating, size = size, tabular.environment= tabular.environment, sanitize.text.function = function(x){x}, latex.environments = c("center"))))
}

  #--------------------------------------------------------------

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
  texbegin[8] <- "\\usepackage{capt-of}"
  texbegin[9] <- "\\usepackage{pdflscape}"
  texbegin[10] <- "\\begin{document}"
  texbegin[11] <- "\\begin{landscape}"

  textables <- character()
  if(type=="factor") {
    textables[1] <- paste("\\input{","tmp2",filename,"}", sep = "")
  } else textables[1] <- paste("\\input{",filename,"}", sep = "")

  texend <- character()
  texend[1] <- "\\end{landscape}"
  texend[2] <- "\\end{document}"
  texlist <- appendList(texbegin, textables)
  texlist <- appendList(texlist, texend)
  if(type=="factor") {
    texfile <- paste(path, "/runtmp2", filename, ".tex",sep = "")
  } else texfile <- paste(path, "/run", filename, ".tex", sep = "")
  write(texlist, file = texfile)

  #include original table
  if(type=="factor")  {
    cmd = paste("cd ", path, "; cat ",filename, ".tex"," | perl -ne '{ s/ % / \\\\% /g; print $_; }' > ", "tmp",filename,".tex","; cat ","tmp",filename,".tex", " | perl -ne '{ s/linewidth\\}/linewidth\\}\\\\centering /g; print $_; }' > ", "tmp2",filename,".tex", "; pdflatex runtmp2",filename,".tex","; pdflatex runtmp2",filename,".tex", "; cp tmp2",filename,".tex"," ",filename,".tex", "; cp runtmp2",filename,".pdf"," ", "run",filename,".pdf", "; rm tmp2",filename,".tex", "; rm runtmp2",filename,".pdf",  "; rm runtmp2",filename,".aux", "; rm tmp",filename,".tex", "; rm runtmp2",filename,".tex", "; rm runtmp2",filename,".log", "; sleep 1 ;", sep = "")
  }  else cmd = paste("cd ", path, "; pdflatex run",filename, ".tex","; pdflatex run",filename, ".tex", "; rm *.aux", "; sleep 1 ;", sep = "")

  system(cmd, intern=TRUE)


  message("... table step successfully passed!")

}

# end dataoverview()
