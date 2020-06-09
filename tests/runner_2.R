pairInfo <- workingPairs
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
      if (!data.table::is.data.table(list[[i]][[1]]) )
        next
      data.table::set(list[[i]][[1]], j = 'Ticker', value = rep(nm[i], nrow(list[[i]][[1]])))
      arrow::write_parquet(list[[i]][[1]], paste0(path, nm[i], '_', format(Sys.Date(), '%Y_%m_%d'), '.parquet'))
    }
  }
}
names(out[[1]]) <- sapply(out[[1]], function(x)names(x)[1])
out2 <- out[-1]
names(out2) <- sapply(out[-1], function(x)names(x)[1])
exportOHLClist(out[[1]])
exportOHLClist(out2)



