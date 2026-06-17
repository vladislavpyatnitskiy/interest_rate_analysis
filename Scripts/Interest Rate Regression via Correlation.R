lapply(c("quantmod", "timeSeries", "rvest"), require, character.only = T) # lib

interest_regression_cor <- function(method="spearman", dataframe=F){
  
  y <- c(
    paste(
      c(
        "BZ", "HG", "GC", "SB", "CT", "KC", "CC", "HE", "ZS", "ZR", "NG", "KE",
        "GF", "ZC", "SI", "PL", "PA"
      ), 
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
  
  message("Commodities data has been downloaded successfully (1/4)")
  
  if (isTRUE(grepl("-", y))){ y <- gsub("-", "", y) }
  if (isTRUE(grepl("=", y))){ y <- gsub("=", "", y) }
  
  colnames(p) <- c(
    "Brent", "Copper", "Gold", "Sugar", "Cotton", "Coffee", "Cocoa", "Hogs", 
    "Soybeans", "Rice", "Gas", "Wheat", "Cattle", "Corn", "Silver", "Platinum",
    "Palladium", "Dollar"
    )
  
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
  
  message("Interest Rate data has been downloaded successfully (2/4)")
  
  rouble.yahoo <- function(){
    
    p <- read_html("https://finance.yahoo.com/quote/RUB=X/") %>%
      html_nodes('section') %>% html_nodes('div') %>% html_nodes('span') %>% 
      html_text() %>% .[1]
    
    as.numeric(gsub(" ", "", p))
  }
  rouble_df <- rouble.yahoo()
  
  message("Rouble data has been downloaded successfully (3/4)")
  
  commodities.yahoo2 <- function(){ # Data Frame with Commodity values
    
    p1 <- read_html("https://finance.yahoo.com/commodities/") %>% 
      html_nodes('table') %>% html_nodes('tr') %>% html_nodes('td') %>% 
      html_nodes('div') # Read HTML
    
    p <- p1 %>% html_nodes('span') %>% html_text() # Extract names 
    
    v <- as.numeric(gsub(",", "", p[seq(from = 3, to = length(p), by = 3)]))
    
    tickers <- gsub(" ", "", p[seq(from = 1, to = length(p), by = 3)]) #Tickers
    
    names(v) <- tickers
    
    v <- v[paste(c(
      "BZ", "HG", "GC", "SB", "CT", "KC", "CC", "HE", "ZS", "ZR", "NG", "KE",
      "GF", "ZC", "SI", "PL", "PA"
    ), "=F", sep = "")]
    
    v["ZR=F"] = v["ZR=F"] / 100
    
    v <- c(v, as.numeric(rouble_df))
    
    df <- as.data.frame(v) # merge names with values
    
    rownames(df) <- c(
      "Brent", "Copper", "Gold", "Sugar", "Cotton", "Coffee", "Cocoa", "Hogs", 
      "Soybeans", "Rice", "Gas", "Wheat", "Cattle", "Corn", "Silver", 
      "Platinum", "Palladium", "Dollar"#, "Rate"
      )
    
    colnames(df) <- c("Points") # Column names
    
    for (n in 1:ncol(df)){ df[,n] <- as.numeric(df[,n]) } # Make data numeric
    
    df
  }
  commodities_df <- commodities.yahoo2() # Test
  
  message("Live commodity data has been downloaded successfully (4/4)")
  
  names_factors <- c(
    "Brent", "Copper", "Gold", "Sugar", "Cotton", "Coffee", "Cocoa", "Hogs", 
    "Soybeans", "Rice", "Gas", "Wheat", "Cattle", "Corn", "Silver", "Platinum",
    "Palladium", "Dollar"
    )
  
  names_factors <- sort(names_factors)
  
  a <- as.timeSeries(cbind(cbr, a)) # Make it time series and display
  
  a <- a[apply(a, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  p <- a # Join
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  D <- sort(cor(p, method=method)[1,])
  
  D <- D[(D >= -0.8 & D <= -0.2) | (D >= 0.2 & D <= 0.8)]
  
  D <- D[sort(names(D))]
  
  D_names <- names(D)
  
  all_names <- c("Rate", D_names)
  
  p <- p[,all_names]
  
  r <- NULL # Run Optimal regression with valid variables
  
  for (n in 1:length(D_names)){ if (isTRUE(n == 1)){
    
    r <- sprintf("%s ~ %s", "Rate", D_names[1]) } else {
      
      r <- sprintf("%s + %s", r, D_names[n]) } }
  
  R <- summary(lm(r, p)) # Display the most optimal regression model
  
  S <- as.data.frame(R$coefficients[,1]) # Regression coefficients

  r <- rownames(S)[-1] # Row names without intercept value

  g <- S[1,] # Intercept Value

  if (dataframe){ # Last Observations from data frame

    v <- as.data.frame(a[nrow(a),]) # Select last observation

    v <- t(as.data.frame(v)) } # Transpose

  else { v <- commodities_df } # Values from current values

  S <- as.data.frame(S[-1,]) # Reduce first column

  rownames(S) <- r # Change row names to one without first row name

  v <- as.data.frame(v[order(row.names(v)), ]) # Order alphabetically

  rownames(v) <- names_factors # error

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
interest_rate_reg_cor <- interest_regression_cor()
