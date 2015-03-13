complete <- function(directory, id = 1:332) {
    
    filesInDir <- list.files(directory, pattern = "*.csv")
    filesNeeded <- filesInDir[id]
    filesNeeded <- paste("/Users/laurianewales/datasciencecoursera/", directory, "/", filesNeeded, sep = "")
    
    CompleteCases <- vector("numeric")
    
    for (i in 1:length(id)) {
        data <- read.csv(filesNeeded[i], header = TRUE)
        dataComplete <- data[complete.cases(data), ]
        CompleteCases <- c(CompleteCases, nrow(dataComplete))
    }
    
    ReturnFrame <- cbind(id, CompleteCases)
    ReturnFrame <- data.frame(ReturnFrame)
    Labels <- c("id", "nobs")
    colnames(ReturnFrame) <- Labels
    
    ReturnFrame

}
