lapply(c("quantmod", "timeSeries", "MuMIn", "moexer", "rvest"), require,
       character.only = T) # lib

options(na.action = "na.fail") 

interest_regression <- function(){
  
  y <- c(
    paste(
      c("BZ", "HG", "GC", "SB", "CT", "KC", "CC", "HE", "ZS", "ZR", "NG", "KE"), 
      "=F", sep = ""), "RUB=X") # tickers 
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
  for (A in y){ p <- cbind(p, getSymbols(A, src="yahoo", auto.assign=F)[,4]) 
  
  message(
    sprintf(
      "%s is downloaded; %s from %s", 
      A, which(y == A), length(y)
    )
  )
  }
  
  message("Commodities data has been downloaded successfully")
  
  if (isTRUE(grepl("-", y))){ y <- gsub("-", "", y) }
  if (isTRUE(grepl("=", y))){ y <- gsub("=", "", y) }
  
  colnames(p) <- c(
    "Brent", "Copper", "Gold", "Sugar", "Cotton", "Coffee", "Cocoa", "Hogs", 
    "Soybeans", "Rice", "Gas", "Wheat", "Dollar")
  
  a <- as.timeSeries(p) # Make it time series and display
  
  cir <- function(s, e){
    
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
    colnames(v) <- "Rate"
    
    for (n in 1:ncol(v)){ v[,n] <- as.numeric(v[,n]) }
    
    as.timeSeries(v)
  }
  
  cbr = cir("17.09.2013", as.Date(Sys.Date())) # Interest Rate Data
  
  message("Interest Rate data has been downloaded successfully")
  
  a <- as.timeSeries(cbind(cbr, a)) # Make it time series and display
  
  a <- a[apply(a, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
  p <- a # Join
    
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
  l <- NULL # Subset numeric data
    
  for (n in 1:ncol(p)){ if (isTRUE(is.numeric(p[,n]))){ l <- c(l, n) } }
    
  d <- p[,l] # Write a full regression model with all possible variables
    
  for (n in 2:(ncol(d))){ if (isTRUE(n == 2)){ f1 <- colnames(d)[1]
    
      s1 <- colnames(d)[2] # formulae of regression with all variables
      
      if (isTRUE(grepl(" ", f1))){ f1 <- sprintf("`%s`", f1) }
      
      if (isTRUE(grepl(" ", s1))){ s1 <- sprintf("`%s`", s1) }
      
      L <- sprintf("%s ~ %s", f1, s1) } else { h1 <- colnames(d)[n]
      
      if (isTRUE(grepl(" ", h1))){ h1 <- sprintf("`%s`", h1) }
      
      L <- sprintf("%s + %s", L, h1) } } # Join all variables
      
    D <- as.data.frame(dredge(lm(L, d))[1,]) #Run all regressions & Select best
    
    D <- colnames(D[,apply(D,2,function(x) all(!is.na(x)))]) # Cut false values
    
    D <- D[c(2:(length(D) - 5))] # Select names of regression values
    
    r <- NULL # Run Optimal regression with valid variables
    
    for (n in 1:length(D)){ if (isTRUE(n == 1)){
      
        r <- sprintf("%s ~ %s",f1,D[1]) } else {
        
        r <- sprintf("%s + %s",r,D[n]) } }
    
    R <- summary(lm(r, d)) # Display the most optimal regression model
    
    S <- as.data.frame(R$coefficients[,1]) # Regression coefficients
    
    r <- rownames(S)[-1] # Row names without intercept value
    
    g <- S[1,] # Intercept Value
    
    v <- as.data.frame(a[nrow(a),]) # Select last observation
    
    S <- as.data.frame(S[-1,]) # Reduce first column
    
    rownames(S) <- r # Change row names to one without first row name
    
    v <- t(as.data.frame(v)) # Transpose
    
    v <- as.data.frame(v[order(row.names(v)), ]) # Order alphabetically
    
    v <- v[c(rownames(S)),]
    
    v <- as.data.frame(v)
    
    rownames(v) <- rownames(S)
    
    l <- data.frame(S, v) # Join 
    
    l$var <- l[,1] * l[,2] # Sum Product of two columns
    
    pot_return = round(log(round(sum(l[,3]) + g, 2) / p[nrow(p), 1]), 4) * 100
    
    reg <- list(R)
    
    g <- cbind.data.frame(
      round(sum(l[,3]) + g, 2),
      round(p[nrow(p), 1], 2),
      pot_return,
      nrow(p),
      round(R[[9]], 2)
    )
    
    colnames(g) <- c(
      "Fair Interest", "Current Interest", "Change (%)", "Number of Obs.",
      "Adjusted R^2"
    )
    
    df <- list(reg, g)
    
    names(df[[1]]) <- "Interest Rate" # Assign tickers
    
    names(df) <- c("Regression", "Data Frame") # Names
    
    df
}
interest_regression()
