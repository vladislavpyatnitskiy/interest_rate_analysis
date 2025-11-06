lapply(c("rvest", "timeSeries"), require, character.only = T) # Libs

cbr_interest_rate <- function(s, e){

  if (as.Date(s, format = "%d.%m.%Y") < "2013-09-17") s = "17.09.2013"
 
  L <- sprintf(
    paste(
      "https://www.cbr.ru/eng/hd_base/KeyRate/",
      "?UniDbQuery.Posted=",
      "True&UniDbQuery.From=%s&UniDbQuery.To=%s",
      sep = ""),
    s, e)
  
  B <- read_html(L) %>% html_nodes('table') %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() 
  
  v <- data.frame(
    B[seq(from = 1, to = length(B), by = 2)],
    B[seq(from = 2, to = length(B), by = 2)]
    )
  
  colnames(v) <- c("Date", "Interest Rate")
  
  v$Date <- as.Date(v$Date, format = "%d.%m.%Y")
  
  v <- v[order(v$Date, decreasing = F), ]
  
  dates <- v[,1]
  
  v <- as.data.frame(v[,-1])
  
  rownames(v) <- dates
  colnames(v) <- "Interest Rate"
  
  for (n in 1:ncol(v)){ v[,n] <- as.numeric(v[,n]) }
  
  as.timeSeries(v)
}
cbr_interest_rate("18.11.2024", "28.10.2025")
