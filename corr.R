corr <- function(directory, threshold = 0) {
    
    filesInDir <- list.files(directory, pattern = "*.csv")
    filesInDir <- paste("/Users/laurianewales/datasciencecoursera/", directory, "/", filesInDir, sep = "")
    
    CorList <- vector("numeric")
    dataComplete <- vector("numeric")
    
    for (i in 1:length(filesInDir)) {
        data <- read.csv(filesInDir[i], header = TRUE)
        dataComplete <- data[complete.cases(data), ]
   
        if (nrow(dataComplete) > threshold) {
            ThisCor <- cor(dataComplete[ ,2], dataComplete[ ,3])
            CorList <- c(CorList, ThisCor)
        }
    }
    
    CorList
}
