pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    filesInDir <- list.files(directory, pattern = "*.csv")
    filesNeeded <- filesInDir[id]
    filesNeeded <- paste("/Users/laurianewales/datasciencecoursera/", directory, "/", filesNeeded, sep = "")
    
    dataKeep <- vector("numeric")
    
    if (identical(pollutant, "sulfate")) {
        polCol <- 2
    } else if (identical(pollutant, "nitrate")) {
        polCol <- 3
    } else {
        return("wrong pollutant name, try again")
    }
    
    for (i in 1:length(id)) {
        data <- read.csv(filesNeeded[i], header = TRUE)
        dataNeeded <- data[ ,polCol]
        dataKeep <- c(dataKeep, dataNeeded)
    }
    
    mean(dataKeep, na.rm = TRUE)

}