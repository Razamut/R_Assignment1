## This function reads a directory that contains 332 files and returns the number
## of completely observed cases for each file.
## You may also choose any particular file(s) or any number of files from the 
## directory you would like to work with

complete <- function(directory, id=1:332){
        mywd <- getwd()
        idlength <- length(id)
        myfilenamevec <- character(length = idlength)
        for(k in seq_along(id)){
                myfilenamevec[k]<- paste(mywd,"/",directory,"/",sprintf("%03d",id[k]),".csv", sep = "")
        }
        countvec <- numeric(length = idlength)
        for(k in seq_along(id)) {
                myreadvec <- read.csv(myfilenamevec[k])
                w <- complete.cases(myreadvec)
                x <- dim(myreadvec[w,])
                countvec[k] <- x[1]                        
        }
        return(data.frame(id = id, nobs = countvec)) 
}















