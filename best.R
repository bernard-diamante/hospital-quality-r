best <- function(abv, outcome_name) {
   data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   # Take only the necessary data
   df <- data.frame(cbind(data[, 2],  # Hospital Name
                          data[, 7],  # State
                          data[, 11], # Heart attack
                          data[, 17], # Heart failure
                          data[, 23]  # Pneumonia
                          )
                    ,stringsAsFactors = FALSE
                   )
   # Set colnames of df
   df_colnames <- c("hospital name", "state", "heart attack", "heart failure", "pneumonia")
   colnames(df) <- df_colnames
   
   ## Check if state and outcome are valid. If not, stop and return error.
   
   # Check state 
   if (!abv %in% df$state) {
      stop("invalid state")
   # Check outcome
   } else if (!outcome_name %in% df_colnames) {
      stop("invalid outcome")
   }
   
   ## Return hospital name in that state with lowest 30-day death rate
      
   # Create subset with only the data with the specified state
   state_df <- subset(df, state == abv)
   # Get column number of outcome
   if (outcome_name == "heart attack") {
      col_n <<- 3
   } else if (outcome_name == "heart failure") {
      col_n <<- 4
   } else if (outcome_name == "pneumonia") {
      col_n <<- 5
   }
   
   #
   df[,col_n] <- as.numeric(df[,col_n])
   # Find row with the least deaths
   row_min_death <- which(state_df[ , col_n] == min(state_df[ , col_n])
                          )

   # Get hospital name with least deaths
   hosp <- state_df[row_min_death, 1]

   # In case of tie, return first hospital based on alphabetical order
   hosp_final <- sort(hosp)
   return(hosp_final[1])
}



best("TX", "heart attack")


