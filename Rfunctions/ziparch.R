#' @title Zip archive directory
#' @description Zips the created files (tables, plots) back into the archive.
#' @param num defines the number of the archive
#' @param del defines with T (default) or F whether the temporarily created directories (see 'unziparch') should be deleted.
#'
#' @return Returns an updated archive that contains additional output such as tables and plots.
#' @export
#'
#' @examples num=46892 ; ziparch(num)
ziparch <- function
(num,
 del=T
){
WD <- getwd()
apath = paste(WD, "/nmnqs", sep = "")
cmd1 = paste("cd ", apath, "; basename *.", num, ".tar.gz", sep = "")
tarname = system(cmd1, intern = TRUE)
TEMPDIR = paste("nmntmp",num,sep="")
cmd2 = paste("cd ", TEMPDIR, "/; tar -czvf ", apath, "/", tarname, " *", sep = "")
system(cmd2)
if(del==T){cmd3 = paste("sleep 1; rm -r ", TEMPDIR, sep = "")
system(cmd3)}
}

# end ziparch()
