
#########################################
## Public data api                     ##
## https://www.kraken.com/features/api ##
#########################################

.BasePublicUrl <- 'https://api.kraken.com/0/public/'

#' Main function for sending posts to the kraken api
#' 
#' @seealso \code{.PublicPost}
.PublicGet <- function(call, ...){
  .ServerGet(call, .BasePublicUrl, ...)
}

#' Main function for sending GET to the kraken api
#' 
#' @seelalso \code{.PublicPost}
.PublicPost <- function(call, ...){
  .ServerPost(call, .BasePublicUrl, ...)
}


#' Get server time
#' 
#' @description Get the server time, which may be used to estimate delay between server and client.  
#' Note that an estimate of time of arrival (ToA) will be approximately half the difference between system time and server time.
#' 
#' @return A list with the following values
#' \itemize{
#' \item unixTime
#' \item posixTime 
#' \item skew
#' \item callTime
#' }
#' 
#' @export
getServerTime <- function(){
  st <- Sys.time()
  tt <- system.time(res <- .PublicPost('Time'))
  res[[2]] <- NULL
  res$posixTime <- as.POSIXct(res$unixtime, origin = '1970-01-01 00:00:00:000000000')
  #It seems the time difference is usually less than 1 second
  # But sometimes the sign flips, which is weird.
  res$skew <- abs(res$unixtime - as.numeric(st))
  res$callTime <- tt
  res
}

#' Get asset info
#' 
#' @description 
#' 
#' @param info Info to retrieve. Defaults to NULL = all info
#' @param aclass Asset class to retrieve. Defaults to 'currency'
#' @param asset Assets to retrieve. Either comma delimited or a vector of strings
#' 
#' @return a data.table with columns given chosen through 'info' and rows chosen by 'aclass' and 'asset'.
#' 
#' @examples
#' #Get all info
#' getAssetInfo()
#' 
#' #Extract XLM and XMR
#' getAssetInfo(asset = c('XMR', 'XLM'))
#' #same
#' getAssetInfo(asset = 'XMR,XLM') 
#' 
#' @export
getAssetInfo <- function(info = NULL, 
                         aclass = 'currency', 
                         asset = NULL){
  if(!is.null(asset))
    asset <- paste0(asset, collapse = ',')
  .prettifyDataFrameResult(.PublicPost('Assets', body = list(info = info, aclass = aclass, asset = asset)))
}

#' Get tradable asset pairs
#' 
#' @description Pulls tradable asset pairs from the kraken api. This allows one to find trading fees, margin
#' 
#' @param info information to extract, either 'info', 'all', 'fees', 'leverage' or 'margin'
#' @param pair Asset pairs to retrieve. Either comma delimited or a vector of strings. Defaults to NULL = all asset pairs.
#' 
#' @return a data.table with fields
#' \itemize{
#' \item Pair name: the name of the pair "p1p2"
#' \item altname: alternative pair name
#' \item wsname: WebSocket pair name (if available)
#' \item aclass_base: asset class of pair base
#' \item base: asset id of pair base
#' \item aclass_quote: asset class of pair second
#' \item quote: asset id of pair second
#' \item lot: volume lot size
#' \item pair_decimals: scaling decimal places for pair
#' \item lot_decimals: scaling decimal places for volume
#' \item lot_multiplier: amount to multiply lot volume by to get currency volume
#' \item leverage_buy: vector of leverage amounts available when buying
#' \item leverage_sell: vector of leverage amounts available when selling
#' \item fees: data.table("volume", "percent fee") with volume start and fee amount
#' \item fees_maker: data.table("volume", "percent fee") with volume start and maker fee amount
#' \item fee_volume_currency: volume discount currency
#' \item margin_call: margin call level
#' \item margin_stop: stop-out/liquidation margin level
#' }
#' 
#' @note The api is set up in such a way, that  given a wrong pair no results are returned. This might be taken into account in future versions of the package.
#' 
#' @examples
#' getTradableAssetPairs()
#' #Get only fees for trading pairs ALGOETH and GBPUSD
#' getTradableAssetPairs(info = 'fees', pair = c('ALGOETH', 'GBPUSD'))
#' #Note that order in pairs matter!
#' getTradableAssetPairs(info = 'fees', pair = c('ALGOETH', 'USDGBP'))
#' #One can avoid this by post-pulling the correct row from the total result. 
#' pairs <- getTradableAssetPairs(info = 'leverage')
#' pairs[rownames %in% CJ(a = c('ALGO', 'ETH', 'GDP', 'USD), b = c('ALGO', 'ETH', 'GDP', 'USD))[, paste0(a, b)]]
#' #Alternative one could store a list of pairs and use this in the function call.
#' 
#' @export
getTradableAssetPairs <- function(info = 'info', pair = NULL){
  if(!is.character(info))
    stop('info must be one of "info", "leverage", "fees" or "margin"')
  info <- match.arg(info, c('info', 'all', 'leverage', 'fees', 'margin'), several.ok = FALSE)
  info <- switch(info, info = , all = NULL, info)
  if(!is.null(pair))
    pair <- paste0(pair, collapse = ',')
  res <- .PublicPost('AssetPairs', body = list(info = info, pair = pair))
  nm <- names(res)
  if(is.null(nm))
    return(NULL)
  for(i in seq_along(res)){
    for(j in c('fees', 'fees_maker')){
      if(!is.null(res[[i]][[j]])){
        if(identical(list(), res[[i]][[j]])){
          res[[i]][[j]] <- NULL
        }else{
          res[[i]][[j]] <- data.table::rbindlist(res[[i]][[j]])
          data.table::setnames(res[[i]][[j]], names(res[[i]][[j]]), c('volume', 'fee_percent'))
        }
      }
    }
    for(j in c('leverage_buy', 'leverage_sell'))
      res[[i]][[j]] <- as.numeric(unlist(res[[i]][[j]]))
  }
  .prettifyDataFrameResult(res, nm)
}

#' Get ticker information
#' 
#' @description This function is used to import various trading information such as ask, bid, open and close. This function returns several fields containing lists.  
#' These fields can be exploded using the unnest_dt function. See the examples section.
#' 
#' @param pair Asset pairs to retrieve. Either comma delimited or a vector of strings. If left missing, all pairs are returned.
#' @param reformat Should output be reformed? Information will be 
#' 
#' @return if reformat = TRUE a data.table with column specification
#' \itemize{
#' \item ask: list with fields price, whole lot volume and lot volume of current ask prices
#' \item bid: similar to ask field, but with bid prices
#' \item volume: list with fields today and last 24 hour, specifying the trade volume of trading pair
#' \item price: list same the same fields as volume, but for price
#' \item trade count: list same the same fields as volume, but for price
#' \item low: list same the same fields as volume, but for price
#' \item high: list same the same fields as volume, but for price
#' \item close: list with fields price and lot volume.
#' }
#' otherwise a nested list with naming according to the kraken websocket api. Reformatting takes a bit of time, so this option is available for when speed is of the essence.
#' 
#' @examples 
#' #Get information for all pairs.
#' getTickerInformation()
#' #Get information for all ADA pairs (as of 2020-06-01)
#' (res <- getTickerInformation(c('ADAETH', 'ADAEUR', 'ADAUSD', 'ADAXBT')))
#' #Explode a kapi_llist column result. rownames are used for grouping
#' unnest_dt(res, ask, rownames)
#' #subsetting the column itself will print something similar (without rownames)
#' res$ask
#' #same but without reformatting (faster)
#' (res <- getTickerInformation(c('ADAETH', 'ADAEUR', 'ADAUSD', 'ADAXBT'), reformat = FALSE))[]
#' 
#' @note Fields for today fields start at 00:00:00 UTC
getTickerInformation <- function(pair, reformat = TRUE){
  if(missing(pair) || is.null(pair))
    pair <- getTradableAssetPairs()[,rownames]
  if(!is.null(pair))
    pair <- paste0(pair, collapse = ',')
  res <- .PublicPost('Ticker', body = list(pair = pair))
  if(!reformat)
    return(res)
  res <- lapply(res, .renameTradingInfoPairElements)
  res <- data.table::setattr(.prettifyDataFrameResult(res), 'class', c('nested_kapi_data.table', 'data.table', 'data.frame'))
  .reformatListColumns(res)[]
}

#' Utility function for renaming elements of ticker information.
#' 
#' @param x list with trading info on a single trading pair
#' 
#' @return The same list renamed accordingly.
.renameTradingInfoPairElements <- function(x){
  names(x) <- match.arg(names(x), c('ask', 'bid', 'close', 
                                    'volume', 'price', 'trade count', 
                                    'low', 'high', 'open'),
                        several.ok = TRUE)
  fields <- c('price', 'whole lot volume', 'lot volume')
  for(i in c('ask', 'bid'))
    if(!is.null(x[[i]])){
      names(x[[i]]) <-  fields
    }
  fields <- c('today', 'last 24 hour')
  for(i in c('volume', 'price', 'trade count', 'low', 'high'))
    if(!is.null(x[[i]])){
      names(x[[i]]) <-  fields
    }
  if(!is.null(x[['close']])){
    names(x[[i]]) <-  c('price', 'lot volume')
  }
  x
}

#' Get OHLC data
#' 
#' @description 
#' This function can be used to import trade information for specific data (Open High Low Close prices) as well as volumes and trade counts.
#' Similar to other functions in the package this function fails for invalid trading pairs.
#' 
#' @param pair Asset pair to import data from 
#' @param interval The time interval between trades to import in minutes. 
#' @param since From which point should data be imported (in UNIXtime)
#' 
#' @return a list with fields
#' \itemize{
#' \item pair: contains a data.table with columnes \itemize{
#'  \item time
#'  \item open
#'  \item high
#'  \item low
#'  \item close 
#'  \item vwap 
#'  \item volume
#'  \item count
#'  \item Ticker (pair)
#' }
#' \item last: the unixtime (id) to be used as since for querying new data. Only a limited time frame is supported (roughly 1.5 days).  
#' }
#' 
#' @examples
#' pairs <- getTradableAssetPairs()
#' getOHLCdata(pairs[,rownames[1]])
#' library(foreach)
#' out <- foreach(pair = pairs[, rownames], .combine = c, .multicombine = TRUE, .errorhandling = 'pass') %do% {
#'    Sys.sleep(3)
#'    getOHLCdata(pair)
#' }
#' 
#' @export
getOHLCdata <- function(pair, interval = 1, since = NULL){
  if(missing(pair) || is.null(pair) || !is.character(pair))
    stop('Pair must be specified.')
  if(length(pair) > 1)
    return(lapply(pair, getOHLCdata, since = since, interval = interval))
  res <- .PublicPost('OHLC', body = list(pair = pair))
  if(!is.numeric(interval))
    stop('interval must be numeric.')
  interval <- as.numeric(as.character(cut(interval, 
                                          c(-Inf, 5, 15, 30, 60, 240, 1440, 10080, 21600, Inf), 
                                          right = FALSE, 
                                          include.lowest = TRUE, 
                                          labels = c(1, 5, 15, 30, 60, 240, 1440, 10080, 21600))))
  res <- .PublicPost('OHLC', body = list(pair = pair, interval = interval))
  res[[1]] <- data.table::rbindlist(res[[1]], use.names = FALSE)
  data.table::setnames(res[[1]], c('time', 'open', 'high', 'low', 'close', 'vwap', 'volume', 'count'))
  res
}

#' Get order book
#' 
#' @param pair Asset pair to import data from 
#' @param count Maximum number of bids/asks 
#' 
#' @description Provides functionality to import current trade book 
#' 
#' @return Returns a list of length 2 (ask / bid) each containing a data.table with elements 
#' \itemize{
#' \item price 
#' \item volume 
#' \item timestamp
#' }
#' 
#' @examples
#' pairs <- getTradableAssetPairs()
#' getOrderBook(pairs[,rownames[1]])
#' library(foreach)
#' out <- foreach(pair = pairs[, rownames], .combine = c, .multicombine = TRUE, .errorhandling = 'pass') %do% {
#'    Sys.sleep(3)
#'    getOrderBook(pair)
#' } 
#' 
#' @export 
getOrderBook <- function(pair, count = NULL){
  if(missing(pair) || is.null(pair) || !is.character(pair))
    stop('Pair must be specified.')
  if(length(pair) > 1)
    return(lapply(pair, getOrderBook, count = count))
  res <- .PublicPost('Depth', body = list(pair = pair, count = count))
  res <- list(ask = data.table::rbindlist(res[[1]]$asks), 
              bid = data.table::rbindlist(res[[1]]$bids))
  data.table::setnames(res[[1]], c('price', 'volume', 'timestamp'))
  data.table::setnames(res[[2]], c('price', 'volume', 'timestamp'))
  res
}


#' Get recent trades
#' 
#' @param pair Asset pair to import data from 
#' @param since From which point should data be imported (in UNIXtime)
#' 
#' @return A list of length 2 with elements 
#' \itemize{
#' \item Pairname: A data.table with columns \itemize{
#' \item 'price'
#' \item 'volume'
#' \item 'time'
#' \item 'buy_sell'
#' \item 'market_limit'
#' \item 'miscellaneous' (mostly blank)
#' }
#' \item last: The time to be used next to get "latest" recent trades.
#' }
#' 
#' @description This function allows one to import information about the latest trades performed. Note that every 'buy' will have an equivalent 'sell'.
#' 
#' @examples
#' pairs <- getTradableAssetPairs()
#' getRecentTrades(pairs[,rownames[1]])
#' library(foreach)
#' out <- foreach(pair = pairs[, rownames], .combine = list, .multicombine = TRUE, .errorhandling = 'pass') %do% {
#'    Sys.sleep(3)
#'    getRecentTrades(pair)
#' } 
#' 
#' @export
getRecentTrades <- function(pair, since = NULL){
  if(missing(pair) || is.null(pair) || !is.character(pair))
    stop('Pair must be specified.')
  if(!is.numeric(since))
    since <- as.numeric(since)
  if(length(pair) > 1)
    return(lapply(pair, getOrderBook, since = since))
  res <- .PublicPost('Trades', body = list(pair = pair, since = since))
  res[[1]] <- data.table::rbindlist(res[[1]], use.names = FALSE)
  data.table::setnames(res[[1]], c('price', 'volume', 'time', 'buy_sell', 'market_limit', 'miscellaneous'))
  res
}
#' Get recent spread data
#' 
#' @param pair Asset pair to import data from 
#' @param since From which point should data be imported (in UNIXtime)
#' 
#' @return A list of length 2 with elements 
#' 
#' \itemize{
#' \item Pairname: A data.table with columns \itemize{
#' \item 'time'
#' \item 'ask'
#' \item 'bid'
#' }
#' \item last: The time to be used next to get "latest" recent trades.
#' }
#'  
#' @description This function lets one extract spread in prices for selected asset pairs. Note that at any given time there may be multiple spreads, and that this function (unlike other functions) seem to have a 200 row limit. (As small as 20 minute time frame)
#' 
#' @examples 
#' pairs <- getTradableAssetPairs()
#' getRecentSpreadData(pairs[,rownames[1]])
#' library(foreach)
#' out <- foreach(pair = pairs[, rownames], .combine = list, .multicombine = TRUE, .errorhandling = 'pass') %do% {
#'    Sys.sleep(3)
#'    getRecentSpreadData(pair)
#' } 
#' 
#' @export
getRecentSpreadData <- function(pair, since = NULL){
  if(missing(pair) || is.null(pair) || !is.character(pair))
    stop('Pair must be specified.')
  if(!is.numeric(since))
    since <- as.numeric(since)
  if(length(pair) > 1)
    return(lapply(pair, getOrderBook, since = since))
  res <- .PublicPost('Spread', body = list(pair = pair, since = since))
  res[[1]] <- data.table::rbindlist(res[[1]], use.names = FALSE)
  data.table::setnames(res[[1]], c('time', 'bid', 'ask'))
  res
}



