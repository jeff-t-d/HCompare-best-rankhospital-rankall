# rankhospital() takes three arguments:
# * state : the 2-character abbreviated name of a state
# * outcome : an outcome name ("heart attack", "heart failure", "pneumonia")
# * num : the ranking of a hospital
#
# It reads outcome-of-care-measures.csv and returns a character vector
# with the name of the hospital that has the ranking specified
# by the "num" argument. For example, calling
#
# > rankhospital("MD", "heart failure", 5)
#
# returns a character vector containing the name of the
# hospital with the 5th lowest 30-day death rate for heart
# failure. The "num" argument can take values "best", "worst",
# or an integer indicating the ranking (smaller numbers are better).
# If the number given by "num" is larger than the number of
# hospitals in that state, the function returns NA.
# Hospitals that do not have data on a particular outcome are be
# excluded from the set of hospitals when deciding the rankings.
#
# If there is a tie for 30-day mortality rate for a given cause
# of death, the tie is broken by using the hospital name
# that comes first alphabetically.
#
# The function checks the validity of its arguments. If
# an invalid "state" value is passed, the function
# will stop("invalid state"). If an invalid "outcome" value is
# passed, the function will stop("invalid outcome").
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
#       rankhospital(state, outcome, num="best")
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
