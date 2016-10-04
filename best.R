best <- function(state, outcome) {
    #checking the validity of outcome
    valid.outc <- c("heart attack", "heart failure", "pneumonia")
    
    if (!outcome %in% valid.outc) {
        stop("invalid outcome")
    }
    
    #importing file
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #checking the validity of state
    if (!state %in% df[,7]) {
        stop("invalid state")
    }
    
    #creating df with Nums of columns for outcomes (e.g. "heart attack is
    #col 11 in df)
    outcomeCols <- data.frame(cbind(valid.outc, c(11,17,23)))
    
    #getting the column Num to use
    n <- as.numeric(outcomeCols[outcomeCols[1]==outcome][2])
    
    #converting "rate" column to numeric   
    df[,n] <- suppressWarnings(as.numeric(df[,n]))
    
    # subsetting for the State to use
    sbst <- df[df[7]==state, ]
    
    #getting the vector of hospital names with minimum rate of outcome
    hosp <- sbst[sbst[n]==min(sbst[,n], na.rm=T) & !is.na(sbst[n]),2]
    
    sort(hosp)[1]
}
