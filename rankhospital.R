rankhospital <- function(state, outcome, num = "best") {
    
    #importing the file
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #checking the validity of outcome
    valid.outc <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% valid.outc) stop("invalid outcome")
    
    #checking the validity of state
    if (!state %in% df[,7]) stop("invalid state")
    
    #creating df with Nums of columns for outcomes (e.g. "heart attack is
    #col 11 in df)
    outcomeCols <- data.frame(cbind(valid.outc, c(11,17,23)))
    
    #getting the column Num to use
    n <- as.numeric(outcomeCols[outcomeCols[1]==outcome][2])
    
    #converting "rate" column to numeric   
    df[,n] <- suppressWarnings(as.numeric(df[,n]))
    
    # subsetting for the State to use
    sbst <- df[df[7]==state, ]
    
    #creating ranking df
    rank.df <- sbst[c(1,2,7,n)]
    
    #removing NAs
    rank.df <- rank.df[!is.na(rank.df[4]), ]
    
    #sorting by outcome and hosp name
    rank.df <- rank.df[order(rank.df[4], rank.df[2]),]
    
    #adding ranking column
    rank.df <- cbind(rank.df, rank=1:length(rank.df[,4]))
    
    #converting num to good integer
    if(num == "best") num <- 1
    if(num == "worst") num <- length(rank.df[,4])
    if(num > length(rank.df[,4])) return(NA)
    
    #returning result
    rank.df[rank.df[5]==num,2]
}
