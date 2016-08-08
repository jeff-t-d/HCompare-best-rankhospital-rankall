# See also: Swirl 12 (Looking at Data), 13 (Simulation),
# and 15 (Base Graphics).
#
# rankall() takes two arguments: an
# outcome name ("outcome"), and a hospital ranking ("num"). The
# function reads the outcome-of-care-measures.csv file and returns
# a 2-column data frame containing the hospital in each state that
# has the ranking specified in "num". For example, the function call
# rankall("heart attack", "best") returns a data frame
# containing the names of the hospitals that are the best in their
# respective states for 30-day heart attack death rates. The first
# column in the data frame is named "hospital", which contains the
# hospital name, and the second column is named "state", which
# contains the 2-character abbreviation for the state name.
# Hospitals that do not have data on a particular outcome are
# excluded from the set of hospitals when deciding the rankings.
#
# Ties are handled alphabetically.
#
# The function uses the following template:
#
# rankall <- function(outcome, num = "best") {
#     ## Read outcome data
#    
#     ## Check that state and outcome are valid
#    
#     ## For each state, find the hospital of the given rank
#    
#     ## Return a data frame with the hospital names and the
#     ## (abbreviated) state name
# }
# 
# NOTE: The function does not not call rankhospital().
#
# The function checks the validity of its arguments. If an
# invalid "outcome" value is passed to rankall(), the function
# throws an error via the stop() function with the
# message "invalid outcome". The "num" variable can take values
# "best", "worst", or an integer indicating the ranking (smaller
# numbers are better). If the number given by "num" is larger
# than the number of hospitals in that state, then the function
# returns NA.
#
# Sample output for the function can be found in readme.pdf.
#
######################################################################
#
# rankall()
#
# outcome: "heart attack" (col11), "heart failure" (col17),
#   "pneumonia" (col23)
# num: assumes "best"; takes "worst" or integer
#   If provided value for "num" is higher than range, returns NA.
#   Excludes hospitals that don't have data on a particular outcome.
#
# If tied, breaks using order() and sorts alphabetically
#
# Col1 = "Hospital"
# Col2 = "State"
#
######################################################################
#
rankall <- function(outcome, num="best") {
    outcomes <- read.csv("outcome-of-care-measures.csv",
                         na.strings="Not Available",
                         stringsAsFactors=FALSE)
# check validity of "outcome"; else assign corresponding "outcomes" column
    if(outcome == "heart attack") {column <- 11}
        else if(outcome == "heart failure") {column <- 17}
        else if(outcome == "pneumonia") {column <- 23}
        else{stop("invalid outcome")}
# create subset of "outcomes", stealing Hospital.Names[,2], State[,7] and "column";
# dump NAs
    outcome <- na.omit(outcomes[,c(2,7,column)])
# give columns more convenient names
    colnames(outcome) <- c("hospital", "state", "mortality.rate")
# order "outcome" by rows in $State, then by rows in $Mortality.Rate
    outcome <- outcome[order(outcome$state, outcome$mortality.rate),]
# split "outcome" into separate dataframes for each state
    outcome <- split(outcome, outcome$state)
# print appropriate rows for "num" input
    if(num == "best") {
        x <- lapply(outcome, "[", 1, c(1,2), drop=FALSE)
        do.call("rbind", x)
        }
        else if(num == "worst") {
            x <- matrix()
            for (i in seq_along(outcome)) {
                maxrow <- nrow(as.data.frame(outcome[i]))
                x[i] <- lapply(outcome[i], "[", maxrow, c(1,2), drop=FALSE)
            }
            do.call("rbind", x)
        }
        else {
            x <- lapply(outcome, "[", num, c(1,2), drop=FALSE)
            do.call("rbind", x)
        }
}