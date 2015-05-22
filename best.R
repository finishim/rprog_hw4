best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    subset <- list() ## create a list to store the information needed
    subset <- data[c(2, 7, 11, 17, 23)] ## grab hospital name, state and death rates
    colnames(subset)[3:5] <- c("heart attack", "heart failure", "pneumonia") ## simpler colnames
    ## Check that state and outcome are valid
    uniqueState <- unique(subset[[2]]) ## store unique state ids for validation
    if(!is.element(state, uniqueState)) {
        ## throw error because state is not in the data list
    } ## otherwise continue
    if(!is.element(outcome, colnames(subset)[3:5])){
        ## throw error because outcome is not part of the tracked stats
    }
    ## Return hospital name in that state with lowest 30-day death rate
    
 
}