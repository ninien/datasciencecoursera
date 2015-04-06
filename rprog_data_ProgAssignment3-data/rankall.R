rankall <- function(outcome, num = "best") {
    ## Read outcome data
    my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    j <- 0
    if (outcome == "heart attack") {
        j <- 11
    } else if (outcome == "heart failure") {
        j <- 17
    } else if (outcome == "pneumonia") {
        j <- 23
    } else {
        stop(print("invalid outcome"))
    }
    
    ## Take only bits you need
    my_data <- my_data[ ,c(2,7,j)]
    my_data[ ,3] <- as.numeric(my_data[ ,3])
    my_data <- my_data[complete.cases(my_data), ]
    
    ## Split data by state
    split_by_state <- split(my_data, my_data$State)

    ## order
    for (i in 1:length(split_by_state)) {
        split_by_state[[i]] <- split_by_state[[i]][order(split_by_state[[i]][ ,2], 
                                                         split_by_state[[i]][ ,3], 
                                                         split_by_state[[i]][ ,1]), ]
    }
    
    ## For each state, find the hospital of the given rank
    hospital <- vector()
    state <- vector()
    for (i in 1:length(split_by_state)) {
        if (num == "best") {
            this <- split_by_state[[i]][1,1]
        } else if (num == "worst") {
            this <- split_by_state[[i]][nrow(split_by_state[[i]]),1]
        } else {
            this <- split_by_state[[i]][num,1]
        }
        hospital <- append(hospital, this)
    }
    
    hospital <<- hospital
    states <<- names(split_by_state)
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    answer <- cbind(hospital, states)
    answer <- data.frame(answer)
    colnames(answer) <- c("hospital", "state")
    rownames(answer) <- states
    
    answer
}