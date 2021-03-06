## Summary:
These functions are designed to rank US hospitals using any version of two particular .csv files provided by the Hospital Compare database (http://hospitalcompare.hhs.gov). HCompare is operated by the US Dept of Health & Human Services  to "provide data about the quality of care at over 4,000 Medicare-certified hospitals in the US." The two .csv files (outcome-of-care-measures.csv & hospital-data.csv) report 30-day mortality and readmission rates for three variables (heart attacks, heart failure, and pneumonia) and contain identifying information for each hospital.

--

## Repository contents:

best.R : best(state, outcome) - Reads outcome-of-care-measures.csv and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality rate for the specified outcome in that state.

rankhospital.R : rankhospital(state, outcome, num="best") - Reads outcome-of-care-measures.csv and returns a character containing the name of the hospital in the given state that has the given ranking for the given outcome.

rankall.R : rankall(outcome, num="best") - Reads outcome-of-care-measures.csv and returns a 2-column data frame containing the hospitals in each state that have the ranking specified by "num" for the given outcome.

outcome-of-care-measures.csv : 30-day mortality and readmission rates for heart attacks, heart failure, and pneumonia

hospital-data.csv : Hospital ID information

key for csv files.txt : codebook for outcome-of-care-measures.csv and hospital-data.csv

Hospital_Revsed_flatfiles.pdf : master codebook for ALL files in the HCompare database, most of which are not relevant to these functions
