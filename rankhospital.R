rankhospital <- function(state, outcome, num = "best") {
   ## Read outcome data
   dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
   ## Check that state and outcome are valid
   if (!state %in% dat$State){
      stop('invalid state')
   } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
      stop('invalid outcome')
   }
   
   ## Return hospital name in that state with the given rank
   ## 30-day death rate
   
   # Create df with only the hospitals from desired state
   work_df <- subset(dat, dat[7] == state)
   
   # Take only necessary columns
   work_df <- data.frame(cbind(work_df[, 2],  # Hospital Name
                               work_df[, 11], # Heart attack
                               work_df[, 17], # Heart failure
                               work_df[, 23]  # Pneumonia
                              )
                        ,stringsAsFactors = FALSE
                        )
   # Set column names
   df_colnames <- c("hospital name", "heart attack", "heart failure", "pneumonia")
   colnames(work_df) <- df_colnames

   # Sort df by order
   col_n <- which(df_colnames == outcome)
   work_df <- work_df[order(as.numeric(work_df[,col_n]), work_df[,1], na.last = NA),]
   
   # Return output based on num
   if (!is.numeric(num)){
      if (num == "best"){
         return(work_df[1,1])
         
      } else if (num == "worst"){
         return(tail(work_df[,1], n=1))
         
      }
   } else if(is.numeric(num)){
      return(work_df[num,1])
   }
}

rankhospital("MD", "heart attack", "worst")
#rankhospital("TX", "heart failure", 4)

