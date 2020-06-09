


#' Class container for public and private keys to be used in every private function call.
#' 
#' @param Secret Private api key
#' @param key Public api key 
#' 
#' @return An R6 kraken_container object.
#' 
#' @description This container is meant to hold all secret 
#' 
#' @export
Secret_container <- R6::R6Class('Kraken_container', 
                                public = list(
                                  initialize = function(secret, key){
                                    if(missing(secret))
                                      stop('secret may not be missing!')
                                    if(!is.character(secret))
                                      stop('The secret should be a character vector!')
                                    if(missing(key))
                                      stop('key may not be missing!')
                                    if(!is.character(key))
                                      stop('The key should be a character vector!')
                                    private$secret <- base64enc::base64decode(secret)
                                    private$key <- key
                                  },
                                  sign = function(uri, nonce, data){
                                    if(missing(data))
                                      data <- list(nonce = nonce)
                                    if(!is.list(data))
                                      data <- as.list(data)
                                    if(!'nonce' %in% names(data))
                                      data <- c(list(nonce = nonce), data)
                                    data <- paste0(names(data), '=', unlist(data), collapse = '&')
                                    encoded <- enc2utf8(paste0(nonce, data))
                                    uri <- gsub('^(.*)?kraken.com', '', uri)
                                    message <- c(charToRaw(uri), digest::digest(encoded, algo = 'sha256', serialize = FALSE, raw = TRUE)) # Her til er den identisk med hashlib.sha256(encoded).hexdigest()
                                    signature <- digest::hmac(private$secret, message, algo = 'sha512', raw = TRUE) #Matches perfectly
                                    return(base64enc::base64encode(signature))
                                  },
                                  print = function(...){
                                    cat('<Kraken_container>\n  Public:\n    initialize: function (secret)\n    sign: function (uri, nonce, data)\n  Private:\n    secret: raw vector')
                                  },
                                  getKey = function(){
                                    private$key
                                  }
                                ),
                                private = list(
                                  secret = character(),
                                  key = character()
                                ),
                                lock_class = TRUE,
                                portable = FALSE, 
                                cloneable = FALSE)