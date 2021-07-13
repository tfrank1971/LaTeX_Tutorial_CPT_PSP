#' @title Unzip archive directory
#' @description The function 1) unzips an archive into a temporary directory 'nmntmp', 2) creates subdirectories 'plots' and 'tables'.
#' @param num archive number.
#'
#' @return creates temporary directories in the file system.
#' @export
#'
#' @examples num=46892 ; unziparch(num)
unziparch <- function
(num
){

WD <- getwd()
apath = paste(WD, "/nmnqs", sep = "")
TEMPDIR=paste("nmntmp",num,sep="")

cmd = paste("cd ", apath, "; basename *.", num, ".tar.gz", sep = "")
tarname = system(cmd, intern = TRUE)

cmd1=paste("rm -r ",TEMPDIR, " 2>/dev/null; mkdir ",TEMPDIR, "; cd ",TEMPDIR,"; tar -xzvf ", apath, "/*.",num,".tar.gz 1>/dev/null",sep="")
system(cmd1)

cmd2 = paste('mkdir ', TEMPDIR, '/tables',sep='')
system(cmd2)

cmd3 = paste('mkdir ', TEMPDIR, '/plots',sep='')
system(cmd3)
}

# end unziparch()
