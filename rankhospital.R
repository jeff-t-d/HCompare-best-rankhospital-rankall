# See also: Swirl 12 (Looking at Data), 13 (Simulation),
# and 15 (Base Graphics).
#
# Rankhospital() takes three
# arguments: the 2-character abbreviated name of a state
# ("state"), an outcome ("outcome"), and the ranking of a hospital
# in that state for that outcome ("num"). The function reads the
# outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the ranking specified
# by the "num" argument. For example, the call
#
# > rankhospital("MD", "heart failure", 5)
#
# returns a character vector containing the name of the
# hospital with the 5th lowest 30-day death rate for heart
# failure. The "num" argument can take values "best", "worst",
# or an integer indicating the ranking (smaller numbers are better).
# If the number given by "num" is larger than the number of
# hospitals in that state, then the function returns NA.
# Hospitals that do not have data on a particular outcome are be
# excluded from the set of hospitals when deciding the rankings.
#
# If there is a tie for 30-day mortality rate for a given cause
# of death, the tie should be broken by using the hospital name
# that comes first alphabetically. You can use order() to sort
# multiple vectors in this manner (i.e. where one vector is used
# to break ties in another vector).
#
# The function uses the following template:
#
# rankhospital <- function(state, outcome, num = "best") {
#     ## Read outcome data
#     
#     ## Check that state and outcome are valid
#     
#     ## Return hospital name in that state with the given rank
#     ## 30-day death rate
# }
#
# The function checks the validity of its arguments. If
# an invalid "state" value is passed to best(), the function
# throws an error via the stop() function with the
# message "invalid state". If an invalid "outcome" value is
# passed to best(), the function throws an error via the
# stop() function with the message "invalid outcome".
#
# Sample output:
#
# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# "HARDFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA
#
###############################################################
#
# rankhospital()
#
# state: 2-character abbreviated name of a state
# outcome: "heart attack" (col11), "heart failure" (col17),
#   "pneumonia" (col23)
# num: hospital rank (assumes "best"); takes "worst" or integer
#   If provided value for "num" is higher than range, returns NA.
#   Excludes hospitals that don't have data on a particular outcome.
#
# If tied, breaks using order() and sorts alphabetically
#
###############################################################
#
rankhospital <- function(state, outcome, num = "best") {
# read outcome data, change "Not Availables" to NA; don't coerce to factors
    outcomes <- read.csv("outcome-of-care-measures.csv",
                         na.strings="Not Available",
                         stringsAsFactors=FALSE)
# check validity of "state"
    if(!state %in% outcomes[,7]) {stop("invalid state")}
# check validity of "outcome"; else assign corresponding "outcomes" column
    if(outcome == "heart attack") {column <- 11}
        else if(outcome == "heart failure") {column <- 17}
        else if(outcome == "pneumonia") {column <- 23}
        else{stop("invalid outcome")}
# Create "group" containing subset of "outcomes" where rows="state";
# steal column $Hospital.Name and "column"
    group <- outcomes[outcomes$State==state, c(2, column)]
# discard NAs and create "cleangroup"
    cleangroup <- na.omit(group)
# order "cleangroup" by "rows in cleangroup looking at col1" then
# "rows in cleangroup looking at col2", blank column;
# create "sortedcleangroup"
    sortedcleangroup <- cleangroup[order(cleangroup[,2], cleangroup[,1]), ]
# handle "best"/"worst"/num too big, else print [num,1] (hospital name)
    if(num == "best") {(sortedcleangroup[1,1])}
        else if(num == "worst") {(sortedcleangroup[nrow(sortedcleangroup),1])}
        else if(num > nrow(sortedcleangroup)) {stop("NA")}
        else{(sortedcleangroup[num,1])}

}