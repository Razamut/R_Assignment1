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















