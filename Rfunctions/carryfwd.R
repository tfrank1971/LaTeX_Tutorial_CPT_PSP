
"carryfwd" <-
    function#To carry forward values of a vector
### values of 0 or NA can be carried forward from the first value not equal 0 or NA. To fill missing values like WT.
###FunctionClass:
###DatamanagementTool
##references<< \url{http://popkin:popkin@frasapp125.fra.pharma.aventis.com/popnew/R/popnew}
##details<< see git clone  /archiv/gits/GitProjects/Popnew
(x,###Vector to carry forward
idx = FALSE,###on TRUE returns the position of the original vector withe carry forwrad
na.only=FALSE)
{
	if (na.only) {   
		sel <- !is.na(x)
	}
	else {
		sel <- !is.na(x) & x != 0
	}
    val <- x[sel]
    x[!sel] <- 0
    x[sel] <- 1
    nsel <- cumsum(x)
    nsel[nsel == 0] <- 1

    if(idx)
        ##(1:length(sel))[sel][nsel]
        (seq(along = sel))[sel][nsel]  ## Ligges(2007) 'Programmieren in R' S.53
    else
        val[nsel]

} # end carryfwd()
attr(carryfwd,"ex") <- function(){
# test vector 
# 0  0  0  0  0  5  6 NA NA NA 10  0  0  0  0  0  0  0  0  0  0  1  0  0
# 
# function carryfwd
tt<-c(rep(0,5),5,6,rep(NA,3),10,rep(0,10),1,0,0)
carryfwd(tt,na.only=TRUE) # prints 

#[1]  0  0  0  0  0  5  6  6  6  6 10  0  0  0  0  0  0  0  0  0  0  1  0  0
carryfwd(tt,na.only=FALSE) # prints 

#[1]  5  5  5  5  5  5  6  6  6  6 10 10 10 10 10 10 10 10 10 10 10  1  1  1

}
