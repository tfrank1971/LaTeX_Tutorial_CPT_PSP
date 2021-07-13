# Splits a character string into a vector of character strings
#  representing fields in the original string separated by a given
#  character string.
"unpaste"<-    function#Split a Character String into Fields
###Splits a character string into a vector of character strings representing fields in the original string separated by a given character string.
###If sep is not the empty string "" , unpaste uses sep to determine field boundaries, and extracts the contents of each field as a character string. If sep is the empty string, unpaste uses the first and width argument to determine the beginning and width of each field, then uses the substring function to extract the contents of each fixed-format field. Often, it is convenient to return a character vector instead of a list; this can be accomplished by wrapping the call to unpaste inside a call to unlist.
###FunctionGroup:
###Tools
(str, 
###List or vector of character strings, such as might be returned from a database query.
sep = "/"
###A character string specifying the separator between fields in str.
){

    mat <- c()

    ##for(i in 1:length(str)){
    for(i in seq(along = str)){
	anzsep <- 0

        for(j in 1:nchar(str[i])) {
            if( substr(str[i], j, j) == sep ){
                anzsep <- anzsep + 1
            }
        }

	w <- rep(1, anzsep + 1)
        
        p <- 1
        r <- 1
        for(j in 1:nchar(str[i])) {
            if( substr(str[i], j, j) == sep ){
                if( j == 1 ){
                    w[r] <- ""
                }
                if( j != 1 ){
                    w[r] <- substr(str[i], p, j - 1)
                }
                p <- j + 1
                r <- r + 1
            }
            j <- j + 1
        }

        
        if( substr(str[i], nchar(str[i]), nchar(str[i])) == sep){
            w[anzsep+1] <- ""
        }
        else{
            w[anzsep+1] <- substr(str[i], p, nchar(str[i]))
        }

        mat <- cbind(mat,w)
    }

    dimnames(mat) <- c(NULL,NULL)
    nr <- nrow(mat)
    ans <- vector("list", nr)

    for (j in 1:nr)
        ans[[j]] <- mat[j, ]

    
    return( ans )
###A list containing the separated fields.
###Contrary to unPaste, "" is also return at the end if sep is the last character
} # end unpaste()


