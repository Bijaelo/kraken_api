
#########################################
## Public data api                     ##
## https://www.kraken.com/features/api ##
#########################################

.BasePublicUrl <- 'https://api.kraken.com/0/public/'

workingPairs <- c('USDTCHF', 'BCHUSDT', 'DAIUSDT', 'USDCUSDT', 'XETCXETH', 'XMLNXETH', 'XREPXETH', 'XETCXXBT', 'XETHXXBT', 'XLTCXXBT', 'XMLNXXBT', 'XREPXXBT', 'XXDGXXBT', 'XXLMXXBT', 'XXMRXXBT', 'XXRPXXBT', 'XZECXXBT', 'XETHZCAD', 'XXBTZCAD', 'XXRPZCAD', 'ZUSDZCAD', 'XETCZEUR', 'XETHZEUR', 'XLTCZEUR', 'XMLNZEUR', 'XREPZEUR', 'XXBTZEUR', 'XXLMZEUR', 'XXMRZEUR', 'XXRPZEUR', 'XZECZEUR', 'XETHZGBP', 'XXBTZGBP', 'XETHZJPY', 'XXBTZJPY', 'XXRPZJPY', 'ZUSDZJPY', 'USDTZUSD', 'XETCZUSD', 'XETHZUSD', 'XLTCZUSD', 'XMLNZUSD', 'XREPZUSD', 'XXBTZUSD', 'XXLMZUSD', 'XXMRZUSD', 'XXRPZUSD', 'XZECZUSD', 'ZEURZUSD', 'ZGBPZUSD')

#' Perform GET call and pre-format output from the kraken API
#' 
#' @description This function provides the simple functionality used for every GET calls for this package.  
#' Performs some basic error checking and error messaging.
#' 
#' @param call the public api to call, excluding kraken_api:::.BasePublicUrl
#' @param ... other parameters passed to httr::GET
#' 
#' @return the httr::content of the result.
#' 
#' @seealso \code{\link{.ServerPost}}
.ServerGet <- function(call, ...){
  res <- httr::GET(paste0(.BasePublicUrl, call), ...)
  cont <- httr::content(res)
  #Todo: Create a proper error handling module, instead of this simplified error message
  if(!identical(list(), cont$error))
    stop(paste0('Unexpected error returned from server call:\n', cont$error[[1]]))
  cont$result
}

#' Perform POST call and pre-format output from the kraken API
#' 
#' @description This function provides the simple functionality used for every POST calls for this package.  
#' Performs some basic error checking and error messaging.
#' 
#' @param call the public api to call, excluding kraken_api:::.BasePublicUrl
#' @param ... other parameters passed to httr::POST
#' 
#' @return the httr::content of the result.
#' 
#' @seealso \code{\link{.ServerGet}}
.ServerPost <- function(call, ...){
  res <- httr::POST(paste0(.BasePublicUrl, call), ...)
  cont <- httr::content(res)
  #Todo: Create a proper error handling module, instead of this simplified error message
  if(!identical(list(), cont$error))
    stop(paste0('Unexpected error returned from server call:\n', cont$error[[1]]))
  cont$result
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
  tt <- system.time(res <- .ServerPost('Time'))
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
  .prettifyDataFrameResult(.ServerPost('Assets', body = list(info = info, aclass = aclass, asset = asset)))
}

#' Convert list of (possibly named) rows into a data.table
#' 
#' @param list a list similar to .ServerPost('Time')
#' 
#' @return A data.table object with the rows given by list. Missing values are filled with NA
.prettifyDataFrameResult <- function(list, nm){
  if(!is.null(list)){
    if(missing(nm))
      nm <- names(list)
    if(is.null(nm))
      return(NULL)
    data.table::setDT(tidyr::unnest_wider(tibble::tibble(list), c(list)))[, rownames := nm][]
  }
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
  res <- .ServerPost('AssetPairs', body = list(info = info, pair = pair))
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
#' \item ask: kapi_list with fields price, whole lot volume and lot volume of current ask prices
#' \item bid: similar to ask field, but with bid prices
#' \item volume: kapi_list with fields today and last 24 hour, specifying the trade volume of trading pair
#' \item price: kapi_list same the same fields as volume, but for price
#' \item trade count: kapi_list same the same fields as volume, but for price
#' \item low: kapi_list same the same fields as volume, but for price
#' \item high: kapi_list same the same fields as volume, but for price
#' \item close: kapi_list with fields price and lot volume.
#' }
#' otherwise a nested list with naming according to the kraken websocket api. Reformatting takes a bit of time, so this option is available for when speed is of the essence.
#' 
#' @examples 
#' #Get information for all pairs.
#' getTickerInformation()
#' #Get information for all ADA pairs (as of 2020-06-01)
#' getTickerInformation(c('ADAETH', 'ADAEUR', 'ADAUSD', 'ADAXBT'))
#' #same but without reformatting
#' (res <- getTickerInformation(c('ADAETH', 'ADAEUR', 'ADAUSD', 'ADAXBT'), reformat = FALSE))[]
#' #Explode a kapi_list column result. rownames are used for grouping
#' unnest_dt(res, dt, rownames)
#' #subsetting the column itself will print something similar (without rownames)
#' res$ask
#' 
#' @note Fields for today fields start at 00:00:00 UTC
getTickerInformation <- function(pair, reformat = TRUE){
  if(missing(pair) || is.null(pair))
    pair <- getTradableAssetPairs()[,rownames]
  if(!is.null(pair))
    pair <- paste0(pair, collapse = ',')
  res <- .ServerPost('Ticker', body = list(pair = pair))
  if(!reformat)
    return(res)
  res <- lapply(res, .renameTradingInfoPairElements)
  res <- data.table::setattr(.prettifyDataFrameResult(res), 'class', c('nested_kapi_data.table', 'data.table', 'data.frame'))
  .reformatListColumns(res)[]
}
#' Reclass list columns of ticker information into a more informative class
#' 
#' @param dt a data.table with ticker information
#' 
#' @return the same data.table (invisibly) but with list columns changed to kapi_llist
.reformatListColumns <- function(dt){
  lc <- dt[, sapply(.SD, is.list)]
  nm <- names(lc)
  for(z in nm[lc]){
    data.table::setattr(dt[[z]], 'class', c('kapi_llist', 'list'))
  }
  dt
}
#' utility function for printing kapi_llist columns
#' 
#' @param x a kapi_llist object
#' @param ... further arguments passed along to data.table:::print.data.table
#' 
#' @return the printed kapi_llist as a data.table (invisibly)
print.kapi_llist <- function(x, ...)
  data.table:::print.data.table(data.table::rbindlist(x, fill = TRUE), ...)


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
      data.table::setattr(x[[i]], 'class', c('kapi_list', 'list'))
    }
  fields <- c('today', 'last 24 hour')
  for(i in c('volume', 'price', 'trade count', 'low', 'high'))
    if(!is.null(x[[i]])){
      names(x[[i]]) <-  fields
      data.table::setattr(x[[i]], 'class', c('kapi_list', 'list'))
    }
  if(!is.null(x[['close']])){
    names(x[[i]]) <-  c('price', 'lot volume')
    data.table::setattr(x[[i]], 'class', c('kapi_list', 'list'))
  }
  x
}

#' Helper function for unnesting list-like columns in data.table.
#'
#' @param dt a data.table
#' @param col column to explode
#' @param possible grouping column
#' 
#' @return a new data.table with columns specified by exploding the lists in col. 
#' 
#' @description Code was repurposed from the code described in \href{https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwj92Oi_lOHpAhX9AxAIHc4RD3gQFjACegQIAhAB&url=https%3A%2F%2Fosf.io%2Ff6pxw%2Fdownload&usg=AOvVaw1YxmRWJ2O3Wfg_kkuHFyIm}{List-columns in data.table: Nesting and unnesting data1 tables and vectors}.
#' 
#' @export
unnest_dt <- function(dt, col, by){ 
  stopifnot(data.table::is.data.table(dt))
  if(missing(by)){
    by <- substitute(seq_len(nrow(dt)))
  }else
    by <- substitute(list(by)) 
  col <- substitute(unlist(col, recursive = FALSE))
  dt[, eval(col), by = eval(by)]
}


#' Get OHLC data
#' 
#' @description 
#' 
#' @param pair 
#' @param interval 
#' @param since 
#' 
#' @return a list with fields
#' \itemize{
#' pair: named pair field
#' last: the unixtime (id) to be used as since for querying new data. 
#'       As of now it does not seem that data is stored past 660 or so  
#' }
getOHLCdata <- function(pair, interval = 1, since = NULL){
  if(missing(pair) || is.null(pair) || !is.character(pair))
    stop('Pair must be specified.')
  if(length(pair) > 1)
    return(lapply(pair, getOHLCdata, since = since, interval = interval))
  res <- .ServerPost('OHLC', body = list(pair = pair))
  if(!is.numeric(interval))
    stop('interval must be numeric.')
  interval <- as.numeric(as.character(cut(interval, 
                                          c(-Inf, 5, 15, 30, 60, 240, 1440, 10080, 21600, Inf), 
                                          right = FALSE, 
                                          include.lowest = TRUE, 
                                          labels = c(1, 5, 15, 30, 60, 240, 1440, 10080, 21600))))
  res <- .ServerPost('OHLC', body = list(pair = pair, interval = interval))
  res[[1]] <- data.table::rbindlist(res[[1]], use.names = FALSE)
  data.table::setnames(res[[1]], c('time', 'open', 'high', 'low', 'close', 'vwap', 'volume', 'count'))
  res
}

#' Get order book
#' 
#' @param pair 
#' @param count 
#' 
#' @return 
#' 
#' @description 
#' 
#' @examples 
#' 
#' @export 
getOrderBook <- function(pair, count = NULL){
  if(missing(pair) || is.null(pair) || !is.character(pair))
    stop('Pair must be specified.')
  if(length(pair) > 1)
    return(lapply(pair, getOrderBook, count = count))
  res <- .ServerPost('Depth', body = list(pair = pair, count = count))
  res
}


#' Get recent trades
#' 
#' @param pair
#' @param since
#' 
#' @return 
#' 
#' @description 
#' 
#' @examples 
#' 
#' @export
getRecentTrades <- function(pair, since = NULL){
  if(missing(pair) || is.null(pair) || !is.character(pair))
    stop('Pair must be specified.')
  if(!is.numeric(since))
    since <- as.numeric(since)
  if(length(pair) > 1)
    return(lapply(pair, getOrderBook, since = since))
  res <- .ServerPost('Trades', body = list(pair = pair, since = since))
  res
}
#' Get recent spread
#' 
#' @param pair
#' @param since
#' 
#' @return 
#' 
#' @description 
#' 
#' @examples 
#' 
#' @export
getRecentTrades <- function(pair, since = NULL){
  if(missing(pair) || is.null(pair) || !is.character(pair))
    stop('Pair must be specified.')
  if(!is.numeric(since))
    since <- as.numeric(since)
  if(length(pair) > 1)
    return(lapply(pair, getOrderBook, since = since))
  res <- .ServerPost('Spread', body = list(pair = pair, since = since))
  res
}

#' Get recent trades
#' 
#' @param pair
#' @param since
#' 
#' @return 
#' 
#' @description 
#' 
#' @examples 
#' 
#' @export
getRecentTrades <- function(pair, since = NULL){
  if(missing(pair) || is.null(pair) || !is.character(pair))
    stop('Pair must be specified.')
  if(!is.numeric(since))
    since <- as.numeric(since)
  if(length(pair) > 1)
    return(lapply(pair, getOrderBook, since = since))
  res <- .ServerPost('Trades', body = list(pair = pair, since = since))
  res
}

works <- pairInfo[-which(pairInfo %in% names(out))]

pairInfo <- getAssetInfo()
pairInfo <- outer(pairInfo$rownames, pairInfo$rownames, paste0)
pairInfo <- as.vector(pairInfo)
out <- vector('list', n <- length(pairInfo))
names(out) <- pairInfo
pairInfo <- pairInfo[sapply(out, is.null)]
for(i in seq_len(n)){
  if(pairInfo[i] %in% works)
    next
  d <- tryCatch(getOHLCdata(pairInfo[i]), error = function(e) NULL)
  if(!is.null(d))
    out[[i]] <- d
}
out[sapply(out, is.null)] <- NULL
names(out)
#' function used to export ohlc list to parquet files
#' 
#' meant for internal use only
exportOHLClist <- function(list, path = 'git_repos/kraken_api/OHLC/'){
  nm <- names(list)
  for(i in seq_along(list)){
    if(!is.null(list[[i]])){
      if(!data.table::is.data.table(list[[i]][[1]]))
        browser()
      data.table::set(list[[i]][[1]], j = 'Ticker', value = rep(nm[i], nrow(list[[i]][[1]])))
      arrow::write_parquet(list[[i]][[1]], paste0(path, nm[i], '_', format(Sys.Date(), '%Y_%m_%d'), '.parquet'))
    }
  }
}
exportOHLClist(out)

## Update functions to use "workingPairs", which should be updated regularly.
