best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate, first by alphabetical order if there is a tie
    
        ha_num <- as.numeric(state_data[ ,j])
        
        for (i in 1:length(ha_num)) {
            if (is.na(ha_num)[i] == TRUE) {
                ha_num[i] <- 100
            }
        }
        
        ha_hos <- state_data[ ,2]
        minimum <- min(ha_num, na.rm = TRUE)
        hospitals <- vector()
        
        for (i in 1:length(ha_num)) {
            if (ha_num[i] == minimum) {
                hospitals <- append(hospitals, ha_hos[i])
            }
        }

    hospitals <- sort(hospitals)
    hospitals[1]
}