rankall <- function(outcome, num = "best") {
    
    #importing the file
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #checking the validity of outcome
    valid.outc <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% valid.outc) stop("invalid outcome")
    
    #creating df with Nums of columns for outcomes (e.g. "heart attack is
    #col 11 in df)
    outcomeCols <- data.frame(cbind(valid.outc, c(11,17,23)))
    
    #getting the column Num to use
    n <- as.numeric(outcomeCols[outcomeCols[1]==outcome][2])
    
    #converting "rate" column to numeric   
    df[,n] <- suppressWarnings(as.numeric(df[,n]))
    
    #creating ranking df
    rank.df <- df[c(1,2,7,n)]
    names(rank.df)[4] <- paste(outcome, "rate")
    
    #removing NAs
    rank.df <- rank.df[!is.na(rank.df[4]), ]
    
    #creating list by state
    list.all <- split(rank.df, rank.df[3])
    
    #sorting the data frames inside the list + adding ranking column
    list.sorted <- lapply(list.all, function(x) {
        x <- x[order(x[4], x[2]),]
        x <- cbind(x, rank=1:length(x[,4]))
    })
    
    #converting num to good integer for best
    if(num == "best") num <- 1
    
    
    #initiating result data frame
    result.df <- data.frame(hospital = character(0), state = character(0), 
                            stringsAsFactors = F)
    
    
    #looping and building result
    for(i in 1:length(list.sorted)) {
        x1 <- list.sorted[[i]] #take df by state from the list
        
        
        #converting num to good integer for worst
        trigger <- F
        if(num == "worst") {
            num <- length(x1[,4])
            trigger <- T
        }
        
        h <- x1[x1[5]==num,2] #find the hospital of given num rank
        if (length(h)==0) h <- NA #if nothing -> convert to NA
        x2 <- data.frame(h, x1[1,3], stringsAsFactors = F) #make a loop df
        
        #appending to result df
        result.df <- rbind(result.df, setNames(x2, names(result.df)))
        
        if(trigger) num <- "worst"
        
    }
    
    result.df
    
}
