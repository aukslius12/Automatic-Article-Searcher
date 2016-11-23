#------- Keyword Search Function --------

#library (stringr)
#library (digest)

keywordSearch <-
  function (webpages,
            compInfo,
            digInfo = "",
            yahoo = F) {
    compInfo <-
      c (as.vector (unlist ((compInfo[, 2]))), as.vector (unlist (strsplit (
        unlist(compInfo[, 3]), ", "
      ))), as.vector (unlist (strsplit (
        unlist(compInfo[, 4]), ", "
      ))))
    rss <-
      data.frame (rssFeed (webpages, yahoo), stringsAsFactors = F) #Returns a rss feed using my function described before.
    results <-
      data.frame (
        titles = as.character(),
        pubdates = as.character(),
        links = as.character(),
        stringsAsFactors = F
      )
    kwrd <- as.character()
    i <- 1
    test <- digest (rss[i, 1], algo = "sha256")
    res <- digInfo == test
    
    while (res == F & i < length (rss[, 1])) {
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
                titles = rss[i, 1],
                pubdates = rss[i, 2],
                links = rss[i, 3],
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
    if (length(results[, 1]) == 0) {
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
  return (as.character(compInfo[as.integer(res), 1]))
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
        titles = as.character(),
        pubdates = as.character(),
        links = as.character(),
        stringsAsFactors = F
      )
    for (i in 1:length(webpages)) {
      xml.url <- webpages[i]
      script <- getURL(xml.url)
      doc     <- try(xmlParse(script))
      if (class(doc)[1] != "try-error") {
        titles    <- xpathSApply(doc, '//item/title', xmlValue)
        pubdates <- xpathSApply(doc, '//item/pubDate', xmlValue)
        links <- xpathSApply(doc, '//item/link', xmlValue)
        res <-
          data.frame(titles, pubdates, links, stringsAsFactors = F)
        feed <- rbind (feed, res)
      } else {
        i <- i + 1
      }
    }
    feed <- sapply (feed, as.character)
    feed <- feed[!duplicated(feed[, 1]), ]
    return(feed)
    #Returns feed from all RSS' provided
  } else {
    #Webpages can be automatized to use tickers from Tickers.txt file !!!!!!!!
    webpages <-
      "https://feeds.finance.yahoo.com/rss/2.0/headline?s=bac,f,fcx,jcp,vale,chk,wfc,pbr,c,pfe,abx,gm,wft,aks,abev,cx,mt,jpm,auy,t,baba,dis,ms,vrx,fcau,x,rad,itub,kgc,ggb,wll,gg,kmi,vz,cfg,clf,kors,syf,twtr,tck,bby,rig,mrk,mro,xom,ete,ko,dnr,ego,bmy,slw,hmy,orcl,oas,hst,azn,ag,lc,eca,nok,schw,jwn,kr,coty,gpt,kss,fit,met,amx,au,p,dal,hpq,bbt,gfi,abbv,exc,gnw,iag,aig,cig,usb,cvs,dow,bcs,hpe,san,ctl,phm,m&region=US&lang=en-US"
    doc     <- try(htmlParse(getURL(webpages), asText = T))
    titles    <- xpathSApply(doc, '//item/title', xmlValue)
    pubdates <- xpathSApply(doc, '//item/pubdate', xmlValue)
    doc <- try(xmlInternalTreeParse(getURL(webpages)))
    links <-
      as.vector(unlist(xpathApply(doc, '//item/link', xmlValue)))
    feed <-
      data.frame(titles, pubdates, links, stringsAsFactors = F)
    feed <- sapply (feed, as.character)
    feed <- feed[!duplicated(feed[, 1]), ]
    return(feed)
    #Returns a feed from yahoo finance with specific tickers
  }
}
