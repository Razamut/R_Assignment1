## This function takes a given directory of files and a threshold to compute the 
## correlation between two pollutants, namely sulfate and nitrate. 
## It performs this computation only for monitor locations for which the 
## number of completely observed cases exceeds the given threshold

corr <- function(directory, threshold = 0){
        id <- 1:332
        mywd <- getwd()
        idlength <- length(id)
        myfilenamevec <- character(length = idlength)
        mycor1 <- data.frame()
        countvec <- numeric(length = idlength)
        for(k in seq_along(id)){
                myfilenamevec[k]<- paste(mywd,"/",directory,"/",sprintf("%03d",id[k]),".csv", sep = "")
       
                myreadvec <-read.csv(myfilenamevec[k])
                w <- complete.cases(myreadvec)
                x <- dim(myreadvec[w,])
                countvec[k] <- x[1]
                if(countvec[k] >= threshold){
                        mycor1 <- rbind(mycor1,cor(myreadvec$sulfate,myreadvec$nitrate, use = "pairwise.complete.obs"),deparse.level = 0)
                        mycorvec <- mycor1[,1]
                }
                if(all(countvec < threshold)){
                        mycorvec <- numeric()
                }
        }
        
        
        return(mycorvec)
        
}















