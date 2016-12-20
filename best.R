# best() takes two arguments:
# * state: the 2-character abbreviated name of a state
# * outcome: an outcome name ("heart attack", "heart failure", "pneumonia")
#
# It reads outcome-of-care-measures.cvs and returns a character
# vector with the name of the hospital that has the best
# (i.e. lowest) 30-day mortality rate for the specified outcome
# in that state, via $Hospital.Name. The outcomes can be "heart
# attack," "heart failure," or "pneumonia." Outcome data is
# drawn From the 11th, 17th, and 23rd column of the .cvs file.
# Hospitals that do not have data on a particular outcome
# are excluded from the set of rankings.
#
# It checks the validity of its arguments. If an invalid
# "state" value is passed, the function will
# stop("invalid state"). If an invalid "outcome" value is
# passed, the function will stop("invalid outcome").
#
# Sample output:
#
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "heart attack")
# Error in best("NY", "hert attack") : invalid outcome
#
################################################################
#
#       best(state, outcome)
#
################################################################
#
best <- function(state, outcome) {
# read outcome data; "Not Availables" are NA; don't coerce to factors
    outcomes <- read.csv("outcome-of-care-measures.csv",
                         na.strings="Not Available",
                         stringsAsFactors=FALSE)
# If "state" is invalid, stop
    if(!state %in% outcomes[,7]) {stop("invalid state")}
# Assign appropriate column given "outcome" or stop if invalid
    if(outcome == "heart attack") {column <- 11}
        else if(outcome == "heart failure") {column <- 17}
        else if(outcome == "pneumonia") {column <- 23}
        else {stop("invalid outcome")}
# Create "group" containing the subset of "outcomes" where
# rows="state"; take column2 ($Hospital.Names) and "column"
    group <- outcomes[outcomes$State==state, c(2, column)]
# Print column1 ($Hospital.Names) value of "group" where
# column2 ($column) is min; suppressWarnings() is just QoL
    suppressWarnings(group[which.min(group[, 2]), 1])
}
