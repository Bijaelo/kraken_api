
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
.ServerGet <- function(call, baseURL, ...){
  res <- httr::GET(paste0(baseURL, call), ...)
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
.ServerPost <- function(call, baseURL, ...){
  res <- httr::POST(paste0(baseURL, call), ...)
  cont <- httr::content(res)
  #Todo: Create a proper error handling module, instead of this simplified error message
  if(!identical(list(), cont$error))
    stop(paste0('Unexpected error returned from server call:\n', cont$error[[1]]))
  cont$result
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