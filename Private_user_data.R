

#########################################
## Private user data api               ##
## https://www.kraken.com/features/api ##
#########################################

.onAttach <- function(libname, pkgname){
  httr::set_config(httr::user_agent('R_kraken'))
}
.BasePrivateUrl <- "https://api.kraken.com/0/private/"

#' Main method for posting to the private kraken API. 
#'
#' @param call The private call key to be used.
#' @param secret_container a kraken_key_container object with a valid kraken
#' @param otp 2-factor authenthication password, if activated.
#' @param ... named arguments passed to 
#' @param nonce an ever increasing 64bit unsigned integer counter. See details for more information.
#' 
#' @details This function is the backbone of all calls performed through the private API. For a slight bit of added security public and private keys are stored in Secret_container objects.
#' The function further allows for 2-factor authenthication by passing the authenthication code in the otp argument. 
#' It is important to note that the nonce argument must always be ever increasing integer, otherwise temporary lockout and an "EInternal: invalid nonce" error will be returned. By default an integer is chosen based on the system time.  
#' Due to this restriction it is recommended calls have a delay of 3 - 15 seconds or are performed sequentially to ensure to accomodate possible network issues. If necessary to perform calls in parallel, it might work to use separate public/private keys for each worker although this has not yet been tested.
#' 
#' @import httr
.privatePost <- function(call, secret_container, otp = NULL, ...,  nonce = floor(as.numeric(Sys.time()) * 1e6), DEBUG = FALSE){
  data <- list(...)
  data <- data[which(names(data) != '')]
  if(any(duplicated(names(data))))
    stop('Duplicate data provided. Please format preprocess or remove duplicate arguments.')
  if(missing(otp)) data <- c(list(nonce = nonce), data)
  else data <- c(list(nonce = as.integer(nonce), otp = otp), data)
  sign <- secret_container$sign(paste0(.BasePrivateUrl, call), nonce, data)
  data <- data[!sapply(data, is.null)]
  .ServerPost(call, .BasePrivateUrl, body = data
                , config = httr::add_headers("API-Key" = secret_container$getKey(), "API-Sign" = sign), encode = 'form')
}

