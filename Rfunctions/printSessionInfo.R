#' @title Collect information about the current R session
#' @description Prints information about R version, attached or loaded packages, and OS.
#' @details The function is a wrapper of the \code{\link{sessionInfo}} function.
#' @param x an object of class "sessionInfo".
#' @param filename a character string giving the name of the ASCII file to which the sessionInfo should be written.
#' @param subdir writes the ASCII file in the given subdirectory (optional).
#'
#' @return Returns *.txt file.
#' @export
#'
#' @examples printSessionInfo(subdir="/workfra/ave0010/AVE0010/EFC12703/POH0355/tables")
printSessionInfo <- function
(x,
filename="RsessionInfo.txt",
subdir="tables"
)
{
si <- sessionInfo()
si[] <- lapply(si, function(x) if (is.list(x)) x[sort(names(x))] else sort(x))

sink(paste(subdir,"/",filename,sep=""))
cat('R session information: \n')
cat('------------------------------------------------------------------ \n');
print(si)
sink()

}

# end printSessionInfo()

