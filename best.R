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
        stop("invalid state")
    } ## otherwise continue
    if(!is.element(outcome, colnames(subset)[3:5])){
        ## throw error because outcome is not part of the tracked stats
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death rate
    subset <- split(subset,subset[,2])[[state]] ## store only the state given
    subset <- subset[c("Hospital.Name",outcome)] ## store only the outcome of interest
    ## subset[subset == "Not Available"] <- NA ## replace "Not Available" with NA
    subset[,2] <- as.numeric(subset[,2]) ## set numeric for ordering
    subset <- na.omit(subset) ## omit the lines with NA for sorting properly
    subset <- subset[order(subset[2], subset[1]),] ## order the list
    result <- vector()
    result <- subset[1,1] ## return the best hospital in character vector format
    print(result)
 
}