#------- Internet Connectivity Function ------

#A simple output of TRUE/FALSE depending on if there is an internet connectivity.
#All credits go to user "eyjo" in the stackoverflow.com forums.
#Source: http://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r

havingInternet <- function() {
  if (class(try(getURL("www.google.com"))) != "try-error") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#------- Keyword Search Function --------

#library (stringr)
#library (digest)

keywordSearch <- function (webpages,
                           compInfo,
                           digInfo = "",
                           yahoo = F) {
  while (havingInternet() == FALSE) {
    Sys.sleep(5)
  }
  rss <-
    data.frame (rssFeed (webpages, yahoo), stringsAsFactors = F) #Returns a rss feed using my function described before.
  if (rss[1, 1] == "try-error") {
    return(list(F, digInfo))
  }
  compInfo <-
    c (as.vector (unlist ((compInfo[, 2]))),
       as.vector (unlist (strsplit (
         unlist(compInfo[, 3]), ", "
       ))),
       as.vector (unlist (strsplit (
         unlist(compInfo[, 4]), ", "
       ))))
  results <-
    data.frame (
      Titles = as.character(),
      Pubdates = as.character(),
      Links = as.character(),
      stringsAsFactors = F
    )
  kwrd <- as.character()
  i <- 1
  test <- digest (rss[i, 1], algo = "sha256")
  res <- digInfo == test
  
  while (res == F & i < nrow (rss)) {
    if (any(str_detect(
      str_to_lower(as.character(rss[i, 1])),
      str_to_lower(as.character(compInfo))
    )))
    {
      test <- digest (as.character(rss[i, 1]), algo = "sha256")
      res <- digInfo == test
      if (res == F) {
        kwrd <-
          c (kwrd, unique(na.omit(str_extract(
            tolower(as.character(rss[i, 1])), tolower(as.character(compInfo))
          ))[1]))
        results <-
          rbind(
            results,
            data.frame(
              Titles = rss[i, 1],
              Pubdates = rss[i, 2],
              Links = rss[i, 3],
              stringsAsFactors = F
            )
          )
        i <- i + 1
      } else{
        next()
      }
    } else {
      i <- i + 1
    }
  }
  if (nrow(results) == 0) {
    return (list(F, digInfo))
  } else {
    return (list (results,
                  digest(as.character(results[1, 1]), algo = "sha256"),
                  kwrd))
    #Returns a list of 3 components: a rss feed which fits the keywords we specified,
    #digested value of the last relevant rss feed article,
    #keywords that were found while searching for relevant articles.
  }
}

#------- getQuote() adjusted ---------

#library(quantmod)

#Since getQuote can't return the quotes from the same ticker more than
#one time I had to make adjustments

#Parameters can be adjusted but everything else is useless for my kind of work
#although it's not that hard so i might do it if someone asks.
getQuote0 <- function (tickers) {
  quotes <- data.frame(as.character(), as.character())
  colnames(quotes) <-
    c(as.character("Trade Time"), as.character("Last"))
  for (i in 1:length(tickers)) {
    quotes <- rbind (quotes, getQuote.google (tickers[i])[1:2])
  }
  return(quotes)
}

#------- getQuote.google() fix -------

#library(RJSONIO)

#Since the outdated function wasnt giving out dates, i made a quick fix to it.
getQuote.google <- function (Symbols, ...)
{
  syms <- gsub(" ", "", unlist(strsplit(Symbols, ",|;")))
  sym.string <- paste(syms, collapse = ",")
  length.of.symbols <- length(syms)
  base.url <- "http://finance.google.com/finance/info?client=ig&q="
  if (length.of.symbols > 100) {
    all.symbols <- lapply(seq(1, length.of.symbols, 100),
                          function(x)
                            na.omit(syms[x:(x + 99)]))
    df <- NULL
    cat("downloading set: ")
    for (i in 1:length(all.symbols)) {
      Sys.sleep(0.1)
      cat(i, ", ")
      df <- rbind(df, getQuote.google(all.symbols[[i]]))
    }
    cat("...done\n")
    return(df)
  }
  L <- fromJSON(gsub("^// ", "", paste(readLines(
    paste(base.url,
          sym.string, sep = "")
  ), collapse = "")))
  do.call(rbind, lapply(L, function(x) {
    data.frame(
      TradeTime = x["lt"],
      Last = as.numeric(gsub(",",
                             "", x["l"])),
      Change = as.numeric(x["c"]),
      PctChg = as.numeric(x["cp"]),
      Exchange = x["e"],
      GoogleID = x["id"],
      row.names = x["t"],
      stringsAsFactors = FALSE
    )
  }))
}

#------- Ticker Matching Function ---------

#library (quantmod)

#info <- keywordSearch (webpages, compInfo, digInfo)

tickerMatching <- function (info, compInfo) {
  if (length (info) < 3) {
    return(info) #No changes in the websites
  } else {
    tickers <<- kwdMatch(as.character(unlist(info[3])), compInfo)
    quotes <- getQuote0 (tickers)
    return (quotes)
  }
  
}

#-------- Keyword Matching Function -------

#library (stringr)

#Input is as follows:
#kwd is keywords that were found by our function while searching for relevant articles
#compInfo is described in keywordSearch.

kwdMatch <- function (kwd, compInfo) {
  res <- as.character()
  for (k in 1:dim(compInfo)[1]) {
    name <-
      c (as.vector (unlist ((compInfo[k, 2]))),
         as.vector (unlist (strsplit (
           unlist(compInfo[k, 3]), ", "
         ))),
         as.vector (unlist (strsplit (
           unlist(compInfo[k, 4]), ", "
         ))))
    for (i in 1:length(kwd)) {
      if (any(str_to_lower(name) == kwd[i])) {
        res <- c (res, k)
      }
    }
  }
  return (as.character(unlist(compInfo[as.integer(res), 1])))
  #Returns row numbers of the companies that were found before.
}

#------ Rss Feed Return Function -------

#library (RCurl)
#library (XML)
#library (digest)

#Input is as follows:
#webpage is an rss feed of a website (ex. http://singletrackworld.com/forum/rss/)

rssFeed <- function (webpages, yahoo = F) {
  if (yahoo == F) {
    webpages <- sapply(webpages, as.character, USE.NAMES = FALSE)
    webpages <- as.vector (webpages)
    feed <-
      data.frame (
        Titles = as.character(),
        Pubdates = as.character(),
        Links = as.character(),
        stringsAsFactors = F
      )
    for (i in 1:length(webpages)) {
      xml.url <- webpages[i]
      script <- try(getURL(xml.url))
      if (class(script) == "try-error") {
        return(class(script))
      }
      doc     <- try(xmlParse(script))
      if (class(doc)[1] != "try-error") {
        Titles    <- xpathSApply(doc, '//item/title', xmlValue)
        Pubdates <- xpathSApply(doc, '//item/pubDate', xmlValue)
        Links <- xpathSApply(doc, '//item/link', xmlValue)
        res <-
          data.frame(Titles, Pubdates, Links, stringsAsFactors = F)
        feed <- rbind (feed, res)
      } else {
        i <- i + 1
      }
    }
    feed <- sapply (feed, as.character)
    feed <- feed[!duplicated(feed), ]
    return(feed)
    #Returns feed from all RSS' provided
  } else {
    #Webpages can be automatized to use tickers from Tickers.txt file !!!!!!!!
    webpages <-
      "https://feeds.finance.yahoo.com/rss/2.0/headline?s=bac,f,fcx,jcp,vale,chk,wfc,pbr,c,pfe,abx,gm,wft,aks,abev,cx,mt,jpm,auy,t,baba,dis,ms,vrx,fcau,x,rad,itub,kgc,ggb,wll,gg,kmi,vz,cfg,clf,kors,syf,twtr,tck,bby,rig,mrk,mro,xom,ete,ko,dnr,ego,bmy,slw,hmy,orcl,oas,hst,azn,ag,lc,eca,nok,schw,jwn,kr,coty,gpt,kss,fit,met,amx,au,p,dal,hpq,bbt,gfi,abbv,exc,gnw,iag,aig,cig,usb,cvs,dow,bcs,hpe,san,ctl,phm,m&region=US&lang=en-US"
    doc <- try(xmlInternalTreeParse(getURL(webpages)))
    if (class(doc)[1] == "try-error") {
      return(class(script))
    }
    Titles    <- xpathSApply(doc, '//item/title', xmlValue)
    Pubdates <- xpathSApply(doc, '//item/pubDate', xmlValue)
    Links <-
      as.vector(unlist(xpathApply(doc, '//item/link', xmlValue)))
    feed <-
      data.frame(Titles, Pubdates, Links, stringsAsFactors = F)
    feed <- sapply (feed, as.character)
    feed <- feed[!duplicated(feed), ]
    return(feed)
    #Returns a feed from yahoo finance with specific tickers
  }
}

#NASDAQ ir NYSE veikia nuo 16:30 iki 23:00 Lietuvos laiku.
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
  library (RJSONIO)
  library (readr)
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
      "D:/Random/Automatic-Article-Searcher-master/Automatic-Article-Searcher-master/Potencialus plagiatas/Data/RSS Feeds.txt",
      " ",
      escape_double = FALSE,
      col_names = FALSE,
      trim_ws = TRUE
    )
  
  Tickers <-
    read_delim(
      "D:/Random/Automatic-Article-Searcher-master/Automatic-Article-Searcher-master/Potencialus plagiatas/Data/Tickers.txt",
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
        "D:/Random/Automatic-Article-Searcher-master/Automatic-Article-Searcher-master/Potencialus plagiatas/Results.txt",
        " ",
        escape_double = FALSE,
        trim_ws = TRUE
      )
    
  } else {
    #Code below applies if Results.txt file is missing
    results <-
      data.frame (
        Titles = as.character(""),
        Pubdates = as.character(""),
        Links = as.character(""),
        Tickers = as.character(""),
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
  resLength <- nrow(results) + 1
  baseLength <<- resLength
  
  #------- Execution Function --------
  
  
  tm <- proc.time()
  while ((proc.time() - tm)[3] < (timeInHours * 3600)) {
    
    while (havingInternet() == FALSE) {
      Sys.sleep(5)
    }
    
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
        results[resLength:(resLength + length(unlist(tick[ind2[k]])) - 1), 9:10] <-
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
          cbind(data.frame(tempData[1]), Ticker = tickers, data.frame(tempres))
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
          cbind(data.frame(yahooData[1]), Ticker = tickers, data.frame(tempres))
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
  
  #This part gathers all the "1 hour after the article release time" data after the main script
  #finishes running
  tm <- proc.time()
  while ((proc.time() - tm)[3] < 3630) {
    
    while (havingInternet() == FALSE) {
      Sys.sleep(5)
    }
    
    ind2 <- which((proc.time()[3] - tim2) > 3600)
    if (length(ind2) > 0) {
      for (k in 1:length(ind2)) {
        results[resLength:(resLength + length(unlist(tick[ind2[k]])) - 1), 9:10] <-
          sapply(getQuote0 (unlist(tick[ind2[k]]))[, 1:2], as.character)
        resLength <- resLength + length(unlist(tick[ind2[k]]))
      }
      tim2[ind2] <- 2000000
      res[[ind2]] <- NA
    }
    
  }
  
  rm(tickers)
  
  if (resultsFile == F) {
    return (results[-1, ])
  } else {
    return(results)
  }
}

Results <-
  dataGathering (timeInHours = 6, resultsFile = T) #This should be 6

write.table(
  Results,
  "D:/Random/Automatic-Article-Searcher-master/Automatic-Article-Searcher-master/Potencialus plagiatas/Results.txt",
  row.names = F
)

## An optional code which sends email to my account with the specified Data.

newData <- Results[baseLength:nrow(Results),]$Titles
write.table(
  newData,
  "D:/Random/Automatic-Article-Searcher-master/Automatic-Article-Searcher-master/Potencialus plagiatas/Executables/DataToSend.txt",
  row.names = F
)

file.copy("D:/Random/Automatic-Article-Searcher-master/Automatic-Article-Searcher-master/Potencialus plagiatas/Executables/AAS.Rout", "D:/Random/Automatic-Article-Searcher-master/Automatic-Article-Searcher-master/Potencialus plagiatas/Executables/AAS.txt")

library(mailR)
send.mail(from = "aukslius@gmail.com",
          to = "aukslius@gmail.com",
          subject = "Script",
          body = paste("Script has finnished sucessfully at ", 
                       Sys.time(), ". There was an addition of ", 
                       (nrow(Results) - baseLength + 1), 
                       " rows added to the data.", 
                       "\nAll the data is in the text file\n\n", 
                       sep = ""),
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
          authenticate = TRUE,
          send = TRUE,
          attach.files = c("D:/Random/Automatic-Article-Searcher-master/Automatic-Article-Searcher-master/Potencialus plagiatas/Executables/AAS.txt","D:/Random/Automatic-Article-Searcher-master/Automatic-Article-Searcher-master/Potencialus plagiatas/Executables/DataToSend.txt"))

##---------
file.copy()