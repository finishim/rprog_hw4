best <- function(state, outcome) {
    ## Read outcome data
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    subset <- list() ## create a list to store the information needed
    subset <- outcome[c(2, 7, 11, 17, 23)] ## grab hospital name, state and death rates
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with lowest 30-day death rate
    
 
}