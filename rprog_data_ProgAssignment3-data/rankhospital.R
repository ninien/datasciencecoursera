rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Split data by state
    split_by_state <- split(my_data, my_data$State)
    
    ## Pick out state I want
    state_data <- split_by_state[[state]]
    
    ## Check that state is valid
    if (is.null(state_data)) {
        stop(print("invalid state"))
    }
    
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    my_data <- state_data[ ,c(1,2,j)]
    my_data[ ,3] <- as.numeric(my_data[ ,3])
    my_data <- my_data[ order(my_data[ ,3], my_data[ ,2]), ]
    my_data <- my_data[complete.cases(my_data), ]
    
    hospital <- vector()
    if (num == "best") {
        hospital <- my_data[1,2]
    } else if (num == "worst") {
        hospital <- my_data[nrow(my_data),2]
    } else {
        hospital <- my_data[num,2]
    }
    
    hospital
}