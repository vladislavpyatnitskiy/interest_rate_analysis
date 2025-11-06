compress_interest_periods <- function(df){ 
  
  # Compress data with repetitive values
  
  dates <- as.Date(rownames(df)) # assumes row names are dates
  r <- rle(as.vector(df[,1])) # compress
  ends <- cumsum(r$lengths) 
  starts <- c(1, head(ends, -1) + 1) 
  
  D <- data.frame(
    Interest_Rate = r$values,
    Start_Date = dates[starts],
    End_Date = dates[ends],
    Days_Held = as.numeric(dates[ends] - dates[starts]) + 1
  )
  
  colnames(D) <- c(colnames(df), "Start Date", "End Date", "Days Held")
  
  D # Display
}
compress_interest_periods(cbr_ir_data)
