rankall <- function(outcome,num="best"){
   # Read file
   dat <- read.csv("datasets/outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
   
   # Stop if input is invalid
   disease <- c("heart attack", "heart failure", "pneumonia")
   if(outcome %in% disease == FALSE) {
      stop("invalid outcome")
      }
   
   # Initialize df for output
   final_df <- data.frame(hospital="",state="")
   
   # Get column number of input outcome
   work_df <- data.frame(c_num = c(11,17,23), o = disease)
   dis_cnum <- subset(work_df,work_df$o == outcome)[,1]

   # Get only the necessary columns
   dat1 <- dat[,c(2,7,dis_cnum)]
   
   # Get alphabetically sorted list of state abbreviations
   states <- sort(unique(dat$State))
   
   # Construct the output
   for(i in 1:length(states)){
      state <- states[i]
      # Take hospital data for state i and remove NAs from specified outcome
      work_df <- subset(dat1,dat1[,2] == state & is.na(dat[,dis_cnum]) == FALSE)

      # Sort hospital names by alphabetical order
      work_df1 <- work_df[order(work_df[,1]),]
      
      # Change to numeric and get order of specified outcome data points CHECK
      work_df1[,3] <- as.numeric(work_df1[,3])

      # Sort dataframe CHECK
      work_df1 <- work_df1[order(as.numeric(c(work_df1[,3]))),]
      
      # Add rank column to dataframe (for output reference)
      work_df1$Rank <- c(1:nrow(work_df1))
      if(num == "best") {val <- 1}
      if(num == "worst") {val <- nrow(work_df1)}
      else{val = num}
      
      # If input rank does not exist, return NA for that hospital name datapoint
      # Else: get hospital with specified rank
      if(val > nrow(work_df)){hos_name = NA}
      else{
         work_df1 <- subset(work_df1,work_df1$Rank == val)
         hos_name = work_df1[,1]
      }
      
      # Compile output
      entry <- c(hos_name,state)
      final_df=rbind(final_df,entry)
   }
   # Return output
   return(final_df[-1,])
}

head(rankall("heart attack", 20), 10)
#tail(rankall("pneumonia", "worst"), 3)
#tail(rankall("heart failure"), 10)

