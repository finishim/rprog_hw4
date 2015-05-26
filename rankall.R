rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    subset <- list() ## create a list to store the information needed
    subset <- data[c(2, 7, 11, 17, 23)] ## grab hospital name, state and death rates
    colnames(subset)[3:5] <- c("heart attack", "heart failure", "pneumonia") ## simpler colnames
    
    ## Check that state and outcome are valid
    
    uniqueState <- unique(subset[[2]]) ## store unique state ids for validation
    if(!is.element(state, uniqueState)) {
        ## throw error because state is not in the data list
        stop("invalid state")
    } ## otherwise continue
    if(!is.element(outcome, colnames(subset)[3:5])){
        ## throw error because outcome is not part of the tracked stats
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    for (i in seq_along(uniqueState)) {
        subsetState <- split(subset,subset[,2])[[uniqueState[[i]]]] ## store only the state given
        subsetState <- subsetState[c("Hospital.Name",outcome)] ## store only the outcome of interest
        subsetState[,2] <- as.numeric(subsetState[,2]) ## set numeric for ordering
        subsetState <- na.omit(subsetState) ## omit the lines with NA for sorting properly
        subsetState <- subsetState[order(subsetState[2], subsetState[1]),] ## order the list
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}
