pollutantmean <- function(directory, pollutant, id = 1:332) {
        setwd(directory)
        total <- 0
        n <- 0
        i <- 1
        j <- length(id) + 1
        while (i < j) {
                if (id[i] < 10) {filename <- paste("00",id[i])}
                        else if (id[i] < 100) {filename <- paste("0",id[i])}
                               else {filename <- as.character(id[i])}      
                filename <- gsub(" ","",paste(filename,".csv"))
                filedata <- read.csv(filename,header=TRUE,na.strings=c("NA","NaN", " ") )
                filedata <- na.omit(filedata)
                total <- total + sum(filedata[,pollutant])
                n <- n + nrow(filedata)
                i <- i+1
        }
        mean <- total / n
        mean
}