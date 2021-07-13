  #' @title Creating a summary table of NONMEM model parameters
#' @description This function creates a summary table out of the object from getnmr7 intended to be used in the main part or synopsis of a report.
#' @details The resulting LaTeX table is automatically rendered as pdf file, which is written together with the *.tex file at the same position in the file system, so that the author may see and check the results immediately. File 'paramsum<number>.tex' contains the LaTeX source code of the intext table that can be pulled into the report with the input command. This file may also be edited further by the user, e.g., adding footnotes or removing 'NA'. The latter indicates missing values (e.g. missing covariance step, fixed parameters).
#'
#' @param obj The fit object 'fit<run number>' (the output of getnmr7).
#' @param pdf True for Latex output.
#' @param rtf True for rtf output. Creates also tex output.
#' @param path writes the LaTeX tables in the given subdirectory.
#' @param thetaVariableNames Default is NULL and will take the names for the THETA-rows out of the getnmr7-object. You can give it a character vector with alternative names for the THETA-rows. The size of the vector must be the same as the number of THETA-rows.
#' @param etaVariableNames Default is NULL and will take the names for the ETA-rows out of the getnmr7-object. You can give it a character vector with alternative names for the ETA-rows. The size of the vector must be the same as the number of ETA-rows.
#' @param sigmaVariableNames Default is NULL and will take the names for the SIGMA-rows out of the getnmr7-object. You can give it a character vector with alternative names for the SIGMA-rows. The size of the vector must be the same as the number of SIGMA-rows.
#' @param formatted True as default. Converts the dataframe to character and print it as it is. Works with digits=NULL. The value digits is ignored.
#' @param size LaTeX fontsize specifier.
#' @param table.placement contain only elements of {"h","t","b","p","!","H"}. Default value is "ht".
#' @param caption character vector specifying the table's caption; see \code{\link{xtable}} for details.
#' @param label character string specifying the LaTeX label ; see \code{\link{xtable}} for details.
#' @param ... Additional arguments. (Currently ignored.)
#'
#' @return Prints the table as text output in the console and creates the .tex-files in the filesystem.
#' @export
#'
#' @examples paramsum(fit46892)
paramsum <- function
(obj,
 pdf = T,
 rtf = F,
 path = "tables",
 thetaVariableNames = NULL,
 etaVariableNames = NULL,
 sigmaVariableNames = NULL,
 formatted = T,
 size = "small",
 table.placement = "ht",
 caption = "Parameter estimates and standard errors from model no. ",
 label = "tab:MyLable1",
 ...
 ){
  options("scipen"=20)
  # Function to round the values
  roundValues<-function(values){
    values[,1]<-ifelse(is.na(values[,1]),0,values[,1])
    values[,1]<-signif(values[,1], 3)
    values[,1]<-ifelse(values[,1]==0,NA,values[,1])
    return(values)
  }

  # Bools and data frame for the standard error values of THETA, ETA and SIGMA
  bool<-names(obj@Esti$esti) %in% obj@Esti$namesth
  booleta<-names(obj@Esti$esti) %in% paste(obj@Esti$nameset,":",obj@Esti$nameset, sep = "")
  boolsigma<-names(obj@Esti$esti) %in% obj@Esti$namescorer

  ster<-as.data.frame(t(obj@Esti$esti[2, bool, drop = F]))
  stereta<-as.data.frame(t(obj@Esti$esti[2, booleta, drop = F]))
  stersigma<-as.data.frame(t(obj@Esti$esti[2, boolsigma, drop = F]))

  dimnames(ster)[[2]]<-"SE"
  dimnames(stereta)[[2]]<-"SE"
  dimnames(stersigma)[[2]]<-"SE"

  # Bools for THETA, ETA and SIGMA
  bool<-names(obj@Esti$res.comp) %in% obj@Esti$namesth
  booleta<-names(obj@Esti$res.comp) %in% paste(obj@Esti$nameset,":",obj@Esti$nameset, sep = "")
  boolsigma<-names(obj@Esti$res.comp) %in% paste("sigma²(",obj@Esti$namescorer,")", sep = "")

  # Defining the final data frame with the THETA and ETA and SIGMA
  # Estimate (%RSE) Values
  theta<-obj@Esti$res.comp[, bool, drop = F]
  eta<-obj@Esti$res.comp[, booleta, drop = F]
  sig<-obj@Esti$res.comp[, boolsigma, drop = F]

  theta<-as.data.frame(t(theta))
  eta<-as.data.frame(t(eta))
  sig<-as.data.frame(t(sig))

  etatmp<-eta
  sigtmp<-sig
  eta[,1]<-sqrt(etatmp[,1])
  sig[,1]<-sqrt(sigtmp[,1])

  # Column: 95%CI
  plustmp<-as.data.frame(theta[,1]+2*ster[,1])
  plustmp<-roundValues(values = plustmp)
  minustmp<-as.data.frame(theta[,1]-2*ster[,1])
  minustmp<-roundValues(values = minustmp)
  ci95theta<-as.data.frame(paste(minustmp[,1]," - ",plustmp[,1], sep = ""))

  plustmp<-as.data.frame(sqrt(etatmp[,1]+2*stereta[,1]))
  plustmp<-roundValues(values = plustmp)
  minustmp<-as.data.frame(sqrt(etatmp[,1]-2*stereta[,1]))
  minustmp<-roundValues(values = minustmp)
  ci95eta<-as.data.frame(paste(minustmp[,1]," - ",plustmp[,1], sep = ""))

  plustmp<-as.data.frame(sqrt(sigtmp[,1]+2*stersigma[,1]))
  plustmp<-roundValues(values = plustmp)
  minustmp<-as.data.frame(sqrt(sigtmp[,1]-2*stersigma[,1]))
  minustmp<-roundValues(values = minustmp)
  ci95sigma<-as.data.frame(paste(minustmp[,1]," - ",plustmp[,1], sep = ""))

  dimnames(ci95theta)[[2]]<-"2"
  dimnames(ci95eta)[[2]]<-"2"
  dimnames(ci95sigma)[[2]]<-"2"

  # Column: Estimate(%RSE)

  tmp<-as.data.frame(100*(ster[,1]/theta[,1]))
  tmp<-roundValues(values = tmp)
  theta<-roundValues(values = theta)
  tmp[,1] <- ifelse(tmp[,1]<0,tmp[,1]*-1,tmp[,1])
  theta[,1]<-paste(theta[,1]," (",tmp[,1],")",sep = "")

  tmp<-as.data.frame(100*(stereta[,1]/etatmp[,1]))
  tmp<-roundValues(values = tmp)
  eta<-roundValues(values = eta)
  tmp[,1] <- ifelse(tmp[,1]<0,tmp[,1]*-1,tmp[,1])
  eta[,1]<-paste(eta[,1]," (",tmp[,1],")",sep = "")

  tmp<-as.data.frame(100*(stersigma[,1]/sigtmp[,1]))
  tmp<-roundValues(values = tmp)
  sig<-roundValues(values = sig)
  tmp[,1] <- ifelse(tmp[,1]<0,tmp[,1]*-1,tmp[,1])
  sig[,1]<-paste(sig[,1]," (",tmp[,1],")",sep = "")

  # Column: Shrinkage
  if(!is.null(obj@Esti$shrinketa)|!is.null(obj@Esti$shrinksig)){
    if(!is.null(obj@Esti$shrinketa)){
      etaShrink<-as.data.frame(t(obj@Esti$shrinketa))
      etaShrink[,1]<-ifelse(is.na(etaShrink[,1]),0,etaShrink[,1])
      etaShrink[,1]<-signif(etaShrink[,1],3)
    }else{
      etaShrink<-as.data.frame(c())
      for (item in eta[,1]){
        etaShrink<-rbind(etaShrink,"0")
      }
    }
    if(!is.null(obj@Esti$shrinksig)){
      sigmaShrink<-roundValues(as.data.frame(t(obj@Esti$shrinksig)))
    }else{
      sigmaShrink<-as.data.frame(c())
      for (item in sig[,1]){
        sigmaShrink<-rbind(sigmaShrink,"0")
      }
    }
    thetaShrink<-as.data.frame(c())
    for (item in theta[,1]){
      thetaShrink<-rbind(thetaShrink,"0")
    }

    dimnames(thetaShrink)[[2]]<-"3"
    dimnames(etaShrink)[[2]]<-"3"
    dimnames(sigmaShrink)[[2]]<-"3"
  }

  # Names for the individual sections
  if(is.null(obj@Esti$shrinketa)){
    strucMod<-as.data.frame(t(c("0","0")))
    iiv<-as.data.frame(t(c("0","0")))
    resErr<-as.data.frame(t(c("0","0")))
    strucMod[,1]<-as.numeric(strucMod[,1])
    strucMod[,2]<-as.numeric(strucMod[,2])
    iiv[,1]<-as.numeric(iiv[,1])
    iiv[,2]<-as.numeric(iiv[,2])
    resErr[,1]<-as.numeric(resErr[,1])
    resErr[,2]<-as.numeric(resErr[,2])
    dimnames(strucMod)[[1]]<-"Structural Model:"
    dimnames(strucMod)[[2]]<-c("1","2")
    dimnames(iiv)[[1]]<-"Inter-individual Variability (omega):"
    dimnames(iiv)[[2]]<-c("1","2")
    dimnames(resErr)[[1]]<-"Residual Error:"
    dimnames(resErr)[[2]]<-c("1","2")
  }else{
    strucMod<-as.data.frame(t(c("0","0","0")))
    iiv<-as.data.frame(t(c("0","0","0")))
    resErr<-as.data.frame(t(c("0","0","0")))
    strucMod[,1]<-as.numeric(strucMod[,1])
    strucMod[,2]<-as.numeric(strucMod[,2])
    strucMod[,3]<-as.numeric(strucMod[,3])
    iiv[,1]<-as.numeric(iiv[,1])
    iiv[,2]<-as.numeric(iiv[,2])
    iiv[,3]<-as.numeric(iiv[,3])
    resErr[,1]<-as.numeric(resErr[,1])
    resErr[,2]<-as.numeric(resErr[,2])
    resErr[,3]<-as.numeric(resErr[,3])
    dimnames(strucMod)[[1]]<-"Structural Model:"
    dimnames(strucMod)[[2]]<-c("1","2","3")
    dimnames(iiv)[[1]]<-"Inter-individual Variability (omega):"
    dimnames(iiv)[[2]]<-c("1","2","3")
    dimnames(resErr)[[1]]<-"Residual Error:"
    dimnames(resErr)[[2]]<-c("1","2","3")
  }

  # Pasting the Columns and Rows
  if(!is.null(obj@Esti$shrinketa)|!is.null(obj@Esti$shrinksig)){
    thetaRows<-cbind(theta,ci95theta,thetaShrink)
  }else{
    thetaRows<-cbind(theta,ci95theta)
  }
  thetaRows<-rbind(strucMod,thetaRows)
  if(!is.null(thetaVariableNames)){
    thetaVariableNames<-c("Structural Model:",thetaVariableNames)
    dimnames(thetaRows)[[1]]<-thetaVariableNames
  }

  if(!is.null(obj@Esti$shrinketa)|!is.null(obj@Esti$shrinksig)){
    etaRows<-cbind(eta,ci95eta,etaShrink)
  }else{
    etaRows<-cbind(eta,ci95eta)
  }
  etaRows<-rbind(iiv,etaRows)
  if(!is.null(etaVariableNames)){
    etaVariableNames<-c("Inter-individual Variability (omega):",etaVariableNames)
    dimnames(etaRows)[[1]]<-etaVariableNames
  }

  if(!is.null(obj@Esti$shrinketa)|!is.null(obj@Esti$shrinksig)){
    sigRows<-cbind(sig,ci95sigma,sigmaShrink)
  }else{
    sigRows<-cbind(sig,ci95sigma)
  }
  sigRows<-rbind(resErr,sigRows)
  if(!is.null(sigmaVariableNames)){
    sigmaVariableNames<-c("Residual Error:",sigmaVariableNames)
    dimnames(sigRows)[[1]]<-sigmaVariableNames
  }

  tableContent<-rbind(thetaRows,etaRows,sigRows)

  # Define table axis names
  # Columnnames
  if(!is.null(tableContent[1,3])){
    dimnames(tableContent)[[2]]<-c("Estimate (%RSE)","95% CI","Shrinkage(%)")
    tableContent["Structural Model:",]<-c("","","")
    tableContent["Inter-individual Variability (omega):",]<-c("","","")
    tableContent["Residual Error:",]<-c("","","")
    tableContent[2:6,"Shrinkage(%)"]<-ifelse(tableContent[2:6,"Shrinkage(%)"] == 0,"",tableContent[2:6,"Shrinkage(%)"])
  }else{
    dimnames(tableContent)[[2]]<-c("Estimate (%RSE)","95% CI")
    tableContent["Structural Model:",]<-c("","")
    tableContent["Inter-individual Variability (omega):",]<-c("","")
    tableContent["Residual Error:",]<-c("","")
  }

  # Here finds the document-creation-magic place
  # Prints the table informations
  fitNumber<-obj@Esti$files$ARCHIVE

  if (exists("tableContent")) {
    caption = paste(caption, fitNumber, sep = "")
    label <- label
    if (formatted == T) {
      tableContent <- format(tableContent, scientific = F)
    }
#    if(!is.null(tableContent[1,3])) {
#      cmd = paste("tmpTableContent<-xtable::xtable(tableContent, label='",label,"', caption='",caption,"', align='lccc')", sep = "")
#    }else{
#      cmd = paste("tmpTableContent<-xtable::xtable(tableContent, label='",label,"', caption='",caption,"', align='lcc')", sep = "")
#    }
    if(!is.null(tableContent[1,3])) {
      cmd = paste("tmpTableContent<-xtable::xtable(tableContent,'",caption,"','",label,"', 'lccc')", sep = "")
    }else{
      cmd = paste("tmpTableContent<-xtable::xtable(tableContent,'",caption,"','",label,"', 'lcc')", sep = "")
    }
    eval(parse(text = cmd))
    print(cmd)
    print(caption)
    print(label)
    print(tableContent)
  }

  # Creates LaTeX document
  if ((pdf | rtf) == T) {
    file = paste(path, "/paramsum", fitNumber, ".texraw", sep = "")
    if (exists("tmpTableContent")) {
      print(tmpTableContent, file = file, size = size, caption.placement = "top", label.placement = "top", latex.environments = "center", floating = TRUE, append = FALSE, table.placement = table.placement)
    }

    # Creates the .tex-file with the used packages and the table reference
    texbegin <- character()
    texbegin[1] <- "\\documentclass[a4paper,10pt]{article}"
    texbegin[2] <- "\\usepackage[utf8x]{inputenc}"
    texbegin[3] <- "\\usepackage{threeparttable}"
    texbegin[4] <- "\\usepackage{lmodern}"
    texbegin[5] <- "\\usepackage{amsmath}"
    texbegin[6] <- "\\usepackage{booktabs}"
    texbegin[7] <- "\\usepackage{longtable}"
    texbegin[8] <- "\\usepackage{capt-of}"
    texbegin[9] <- "\\usepackage{pdflscape}"
    texbegin[10] <- "\\begin{document}"
    textables <- character()
    i = 1
    textables[i] <- paste("\\input{paramsum", fitNumber, "}", sep = "")
    texend <- character()
    texend[1] <- "\\end{document}"
    texlist <- appendList(texbegin, textables)
    texlist <- appendList(texlist, texend)
    texfile <- paste(path, "/runparam", fitNumber, ".tex", sep = "")
    write(texlist, file = texfile)
    # Creates the .tex-file
       cmd = paste("cd ", path, "; echo '\\begin{table}[ht]'>paramsum",fitNumber, ".tex; echo '\\begin{threeparttable}'>>paramsum",fitNumber,".tex; grep -iv table paramsum", fitNumber,".texraw>>paramsum", fitNumber, ".tex; echo '\\begin{tablenotes}\\footnotesize'>>paramsum", fitNumber,".tex; echo '\\item[]'>>paramsum",fitNumber,".tex; echo '\\item[] RSE = relative standard error, SD = standard deviation; SE = standard error; CI = confidence interval calculated as 95\\%~CI = Point estimate pm 2 cdot SE; NA = not applicable.'>>paramsum",fitNumber,".tex; echo '\\item[] RSE of parameter estimate is calculated as 100 × (SE/typical value).'>>paramsum",fitNumber,".tex; echo '\\item[] RSE of inter-individual variability magnitude is presented on \\%CV scale and approximated as 100 × (SE/variance estimate)/2.'>>paramsum",fitNumber,".tex; echo '\\item[] Shrinkage is calculated as 100 × (1 – SD of post hocs/omega), with omega = sqrt(variance estimate).'>>paramsum",fitNumber,".tex; echo '\\end{tablenotes}'>>paramsum",fitNumber ,".tex; echo '\\end{threeparttable}'>>paramsum",fitNumber,".tex; echo '\\end{table}'>>paramsum", fitNumber, ".tex ", sep = "")
    system(cmd)
    cmd = paste("cd ", path, "; perl -pi.back -e 's/omega/\\$\\\\omega\\$/g;' paramsum", fitNumber, ".tex", sep = "")
    system(cmd)
    cmd = paste("cd ", path, "; perl -pi.back -e 's/pm/\\$\\\\pm/g;' paramsum", fitNumber, ".tex", sep = "")
    system(cmd)
    cmd = paste("cd ", path, "; perl -pi.back -e 's/cdot/\\\\cdot\\$/g;' paramsum", fitNumber, ".tex", sep = "")
    system(cmd)
    cmd = paste("cd ", path, "; perl -pi.back -e 's/Residual Error:/Residual Error (\\$\\\\sigma\\$):/g;' paramsum", fitNumber, ".tex", sep = "")
    system(cmd)
    cmd = paste("cd ", path, "; perl -pi.back -e 's/sigma²/ /g;' paramsum", fitNumber, ".tex", sep = "")
    system(cmd)
    cmd = paste("cd ", path, "; rm paramsum", fitNumber, ".tex.back", sep = "")
    system(cmd)
      # Creates pdf out of LaTeX document
    if ((pdf) == T) {
      cmd = paste("cd ", path, "; pdflatex runparam", fitNumber, ".tex", sep = "")
      system(cmd)
    }
    # Creates LaTeX document out of LaTeX document
    if ((rtf) == T) {
      cmd = paste("cd ", path, "; latex2rtf runparam", fitNumber, ".tex", sep = "")
      system(cmd)
    }
  }

}

# end paramsum()
