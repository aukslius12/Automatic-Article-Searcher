#NASDAQ ir NYSE veikia nuo 16:30 iki 24:00 Lietuvos laiku.
dataGathering <- function (timeInHours = 1,
                           resultsFile = T) {
  if (timeInHours < 1) {
    stop("Time should be more than 1 hour.")
  }
  
  #------ Preparations for the final execution -------
  
  library (RCurl)
  library (XML)
  library (digest)
  library (stringr)
  library (quantmod)
  library(RJSONIO)
  library(readr)
  ##Optional lines of code to make a sound and print the time when an error occurs.
  library (beepr)
  options(
    error = function() {
      beep()
      print(Sys.time())
    }
  )
  ##
  
  RSS.Feeds <-
    read_delim(
      "C:/Users/Jurgis/Desktop/Github/Automatic-article-Searcher/Potencialus plagiatas/Data/RSS Feeds.txt",
      " ",
      escape_double = FALSE,
      col_names = FALSE,
      trim_ws = TRUE
    )
  
  Tickers <-
    read_delim(
      "C:/Users/Jurgis/Desktop/Github/Automatic-article-Searcher/Potencialus plagiatas/Data/Tickers.txt",
      " ",
      escape_double = FALSE,
      na = "NA",
      trim_ws = TRUE
    )
  
  tempData <- keywordSearch (RSS.Feeds, Tickers)
  yahooData <- keywordSearch (RSS.Feeds, Tickers, yahoo = T)
  if (resultsFile == T) {
    results <-
      read_delim(
        "C:/Users/Jurgis/Desktop/Github/Automatic-article-Searcher/Potencialus plagiatas/Results.txt",
        " ",
        escape_double = FALSE,
        trim_ws = TRUE
      )
    class(results[, 3]) <- "character"
  } else {
    #Code below applies if Results.txt file is missing
    results <-
      data.frame (
        titles = as.character(""),
        pubdates = as.character(""),
        links = as.character(""),
        Trade.Time = as.character(""),
        Last = as.character(""),
        Trade.Time = as.character(""),
        Last = as.character(""),
        Trade.Time = as.character(""),
        Last = as.character(""),
        stringsAsFactors = FALSE
      )
  }
  
  min1 <- 0
  min2 <- 0
  tim <- rep(200000, 50)
  tim2 <- rep(200000, 50)
  tick <- list()
  res <- list()
  res20 <-
    data.frame (Trade.Time = as.character(), Last = as.character())
  i <- 1
  resLength <- length(results[, 1]) + 1
  
  #------- Execution Function --------
  
  
  tm <- proc.time()
  while ((proc.time() - tm)[3] < (timeInHours * 3600)) {
    ind1 <- which((proc.time()[3] - tim) > 1200)
    if (length(ind1) > 0) {
      for (k in 1:length(ind1)) {
        res20 <- getQuote0 (unlist(tick[ind1[k]]))[1:2]
        results <-
          rbind (results, setNames(cbind(res[[ind1[k]]], res20, "", ""), names(results)))
      }
      tim2[ind1] <- tim[ind1]
      tim[ind1] <- 2000000
    }
    
    ind2 <- which((proc.time()[3] - tim2) > 3600)
    if (length(ind2) > 0) {
      for (k in 1:length(ind2)) {
        results[resLength:(resLength + length(unlist(tick[ind2[k]])) - 1), 8:9] <-
          sapply(getQuote0 (unlist(tick[ind2[k]]))[, 1:2], as.character)
        resLength <- resLength + length(unlist(tick[ind2[k]]))
      }
      tim2[ind2] <- 2000000
      res[[ind2]] <- NA
    }
    
    if ((proc.time()[3] - min1) > 120) {
      tempData <- keywordSearch (
        webpages = RSS.Feeds,
        compInfo = Tickers,
        digInfo = as.character (unlist (tempData[2]))
      )
      
      if (length (tempData) > 2) {
        tempres <- tickerMatching(info = tempData, compInfo = Tickers)
        res[[i]] <-
          cbind(data.frame(tempData[1]), data.frame(tempres))
        tick[[i]] <- tickers
        tim[i] <- proc.time()[3]
        i <- i + 1
        if (i == 50) {
          i <- 1
        }
      }
      min1 <- proc.time()[3]
    }
    
    #A separate testing algorythm for yahoo data since it updates more frequently
    if ((proc.time()[3] - min2) > 60) {
      yahooData <- keywordSearch (
        webpages = RSS.Feeds,
        compInfo = Tickers,
        digInfo = as.character (unlist (yahooData[2])),
        yahoo = T
      )
      
      if (length (yahooData) > 2) {
        tempres <- tickerMatching(info = yahooData, compInfo = Tickers)
        res[[i]] <-
          cbind(data.frame(yahooData[1]), data.frame(tempres))
        tick[[i]] <- tickers
        tim[i] <- proc.time()[3]
        i <- i + 1
        if (i == 50) {
          i <- 1
        }
      }
      min2 <- proc.time()[3]
    }
  }
  
  #This part gathers all the "1 hour after the article" data after the main script
  #finishes running
  tm <- proc.time()
  while ((proc.time() - tm)[3] < 3600) {
    ind2 <- which((proc.time()[3] - tim2) > 3600)
    if (length(ind2) > 0) {
      for (k in 1:length(ind2)) {
        results[resLength:(resLength + length(unlist(tick[ind2[k]])) - 1), 8:9] <-
          sapply(getQuote0 (unlist(tick[ind2[k]]))[, 1:2], as.character)
        resLength <- resLength + length(unlist(tick[ind2[k]]))
      }
      tim2[ind2] <- 2000000
      res[[ind2]] <- NA
    }
  }
  if (resultsFile == T) {
    rm(tickers)
    return (results[-1, ])
  } else {
    rm(tickers)
    return(results)
  }
}
