####################
## Runner script. ##
####################

workingPairs <- c('USDTCHF', 'BCHUSDT', 'DAIUSDT', 'USDCUSDT', 'XETCXETH', 'XMLNXETH', 'XREPXETH', 'XETCXXBT', 'XETHXXBT', 'XLTCXXBT', 'XMLNXXBT', 'XREPXXBT', 'XXDGXXBT', 'XXLMXXBT', 'XXMRXXBT', 'XXRPXXBT', 'XZECXXBT', 'XETHZCAD', 'XXBTZCAD', 'XXRPZCAD', 'ZUSDZCAD', 'XETCZEUR', 'XETHZEUR', 'XLTCZEUR', 'XMLNZEUR', 'XREPZEUR', 'XXBTZEUR', 'XXLMZEUR', 'XXMRZEUR', 'XXRPZEUR', 'XZECZEUR', 'XETHZGBP', 'XXBTZGBP', 'XETHZJPY', 'XXBTZJPY', 'XXRPZJPY', 'ZUSDZJPY', 'USDTZUSD', 'XETCZUSD', 'XETHZUSD', 'XLTCZUSD', 'XMLNZUSD', 'XREPZUSD', 'XXBTZUSD', 'XXLMZUSD', 'XXMRZUSD', 'XXRPZUSD', 'XZECZUSD', 'ZEURZUSD', 'ZGBPZUSD')


pairInfo <- getTradableAssetPairs()$rownames
out <- vector('list', n <- length(pairInfo))
names(out) <- pairInfo
pairInfo <- pairInfo[sapply(out, is.null)]
for(i in seq_len(n)){
  d <- tryCatch(getOHLCdata(pairInfo[i]), error = function(e) NULL)
  if(!is.null(d))
    out[[i]] <- d
  Sys.sleep(3)
}

#' function used to export ohlc list to parquet files
#' 
#' meant for internal use only
exportOHLClist <- function(list, path = 'git_repos/kraken_api/OHLC/'){
  nm <- names(list)
  for(i in seq_along(list)){
    if(!is.null(list[[i]])){
      if (!data.table::is.data.table(list[[i]][[1]]) )
        next
      data.table::set(list[[i]][[1]], j = 'Ticker', value = rep(nm[i], nrow(list[[i]][[1]])))
      arrow::write_parquet(list[[i]][[1]], paste0(path, nm[i], '_', format(Sys.Date(), '%Y_%m_%d'), '.parquet'))
    }
  }
}
exportOHLClist(out)



