## This function returns the mean of a given pollutant in a given 
## directory and file(s) id number(s)

pollutantmean <- function(directory, pollutant, id=1:332){
        mywd <- getwd()
        idlength <- length(id)
        myfilenamevec <- character(length = idlength)
        myreadvec <- data.frame()
        for(k in seq_along(id)){
                myfilenamevec[k]<- paste(mywd,"/",directory,"/",sprintf("%03d",id[k]),".csv", sep = "")
                myreadvec <- rbind(myreadvec,read.csv(myfilenamevec[k]))
                if(pollutant == "sulfate"){
                        my_mean <- mean(myreadvec$sulfate, na.rm = TRUE)
                }
                
                if(pollutant == "nitrate"){
                        my_mean <- mean(myreadvec$nitrate, na.rm = TRUE)
                }
        }
        return(my_mean)       
}