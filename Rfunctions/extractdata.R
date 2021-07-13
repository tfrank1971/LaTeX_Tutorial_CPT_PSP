#' @title Extract the data set from the XPOSE object list (created using the getnmr7 function) to attach it to the global R environment.
#'
#' @param num archive number.
#'
#' @return data.frame attached to global workspace
#' @export
#'
#' @examples num=46892; extractdata(num)
extractdata <- function(
  num
  ){

filename = paste('fit',num,sep='')
cmd = paste(filename,"@Data", sep="")
dat <- eval(parse(text=cmd))
cmd=paste("dat ->>dat",num,sep = "")
eval(parse(text=cmd))

}

# end extractdata()
