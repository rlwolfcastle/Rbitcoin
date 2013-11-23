
# Rbitcoin - package level data ------------------------------------------------------------------

require(RJSONIO) #market API communication
require(RCurl) #market API communication, address_balance query
require(digest) #market API authorization
require(data.table) #API call request/result unification

#' @title Rbitcoin integration
#'
#' @description Utilities related to Bitcoin. Core functionalities are:
#' \itemize{
#' \item \code{market.api.query} - launch API query on market's API (\code{mtgox}, \code{bitstamp}, \code{btce}, \code{kraken}). Both public and private API calls supported.
#' \item \code{market.api.process} - launch API query after pre-process of API request (if required) and post-process API result. Produce unified data structure.
#' \item \code{blockchain.api.query} - launch API query on blockchain.info interface.
#' \item support HTTP over SSL.
#' \item debug messages of \code{Rbitcoin}, debug messages of \code{RCurl}.
#' }
#' To do not get banned by market's API user should handle the antispam process by \code{Sys.sleep(10)}.\cr
#' At the time of writing the most recent market's API version were used:
#' \itemize{
#' \item mtgox v2
#' \item bitstamp v2 (public) / ? (private)
#' \item btce v2 (public) / "tapi" (private)
#' \item kraken v0
#' }
#' 
#' BTC donation: \url{bitcoin:15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi}
#' 
#' @references Bitcointalk official Rbitcoin thread: \url{https://bitcointalk.org/index.php?topic=343504}
#' @seealso \code{\link{market.api.query}}, \code{\link{blockchain.api.query}}, \code{\link{market.api.process}}
#' @import RJSONIO RCurl digest data.table
#' @docType package
#' @name Rbitcoin
#' @aliases bitcoin btc
#' \url{https://bitcointalk.org/index.php?topic=343504}
NULL

# blockchain api query  -----------------------------------------------------

#' @title Query blockchain.info API
#'
#' @description Query bitcoin related data from blockchain.info.
#'
#' @param method character. Define what kind of data to query from blockchain (currently only \code{'addressbalance'} supported).
#' @param address character. Bitcoin address passed to query.
#' @param ssl.verify logical flag to use HTTP over SSL, if missing certificate file it will be downloaded.
#' @param curl.verbose logical flag to display RCurl debug messages.
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @references \url{https://blockchain.info/q}
#' @seealso \code{\link{market.api.query}}
#' @examples
#' \dontrun{
#' # get address balance
#' blockchain.api.query(method = 'addressbalance', address = '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa')
#' }
blockchain.api.query <- function(method = 'addressbalance', address, ssl.verify = FALSE, curl.verbose = FALSE, debug = 0){
  fun_name <- 'blockchain.api.query'
  if(ssl.verify) {
    if(!file.exists('cacert.pem')){
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': ssl.verify TRUE but no certificate file \'cacert.pem\' in working directory, downloading from: http://curl.haxx.se/ca/cacert.pem',sep='')
      download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
    }
  }
  url <- switch(method,
                'addressbalance' = paste('https://blockchain.info/q',method,address,sep='/'),
                stop(paste0(fun_name,': unsupported method: ',method)))
  #preparing rcurl options
  useragent <- paste0(
    'R ',paste(c(R.version$major,R.version$minor),collapse='.'),
    "::Rbitcoin ",packageVersion("Rbitcoin")
  )
  if(ssl.verify) curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, cainfo = 'cacert.pem', verbose = curl.verbose, useragent = useragent)
  else curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, verbose = curl.verbose, useragent = useragent)
  res <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  res <- switch(method,
                'addressbalance', as.numeric(res) * 1e-8, #satoshi to btc
                stop(paste0(fun_name,': unsupported method: ',method)))
  return(res) 
}

# api.process -----------------------------------------------------------

#' @name api.dict
#' @title API dictionary
#' @description This data set contains a logic of market specific pre processing and post processing of API calls. \code{data(api.dict); api.dict}. If adding new market most of the work need to be performed here on the definition of data manipulation.
#' @note Many of the errors which will not be handled by standard catching (markets may not return 'result' field etc.) will produce \code{stop(str(x))} to debug shape of returned data. Such errors will be subject to change in future development.
#' @docType data
#' @author Jan Gorecki, 2013-11-22
#' @keywords datasets
NULL

#' @title Process market API
#'
#' @param market character, example: \code{kraken}
#' @param action character, defined process to get organized data
#' @param req list with action details (price, amount, etc.) unified across the markets
#' @param \dots objects to be passed to \code{\link{market.api.query}}
#' \itemize{
#' \item auth params: \code{key}, \code{secret}, \code{client_id} (used on bitstamp), \code{method} (used on btce),
#' \item technical params: \code{ssl.verify}, \code{curl.verbose}, \code{on.error}.
#' }
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @param raw.query.res logical skip post-processing are return only after fromJSON processing.
#' @param on.market.error expression to be returned on market level error. For R level error check \code{market.api.query}.
#' @description Processing of API call according to API dictionary \code{data(api.dict); api.dict}. Pre processing of request and post processing of API call results to unified structure across markets. It will result truncation of most (not common across the markets) attributes returned. If you need the full set of data returned by market's API you should use \code{\link{market.api.query}}.
#' @return Returned value depends on the \code{action} param:
#' \itemize{
#' \item \code{'ticker'} returns \code{data.table} object with common attributes,
#' \item \code{'wallet'} returns \code{data.table} object with common attributes,
#' \item \code{'order_book'} returns \code{list} with API call level attributes and sub elements \code{[['asks']]} and \code{[['bids']]} as \code{data.table} objects with order book including already calculated cumulative \code{amount} and \code{price}.
#' }
#' @note The function is currently in development.\cr Available actions are \code{'ticker'}, \code{'wallet'}, \code{'order_book'}.\cr Market level error handling works only partially as not all markets returns API call status info. For unknown error \code{str(x)} is passed to \code{stop()} function.\cr If you find any bugs please report.
#' 
#' @seealso \code{\link{market.api.query}}
#' @export
#' @examples
#' \dontrun{
#' # get ticker from market
#' market.api.process(market = 'kraken', action = 'ticker', debug = 10)
#' # get ticker from all markets and combine
#' ticker_all <- rbind(
#'   market.api.process(market = 'mtgox', action = 'ticker'),
#'   market.api.process(market = 'bitstamp', action = 'ticker'),
#'   market.api.process(market = 'btce', action = 'ticker'),
#'   market.api.process(market = 'kraken', action = 'ticker')
#' )
#' print(ticker_all)
#' 
#' # get wallet from market
#' market.api.process(market = 'kraken', action = 'wallet', debug = 10)
#' # get wallet from all markets and combine
#' wallet_all <- rbind(
#'   market.api.process(market = 'mtgox', action = 'wallet',
#'                      key = '', secret = ''),
#'   market.api.process(market = 'bitstamp', action = 'wallet',
#'                      client_id = '', key = '', secret = ''),
#'   market.api.process(market = 'btce', action = 'wallet',
#'                      method = '', key = '', secret = ''),
#'   market.api.process(market = 'kraken', action = 'wallet',
#'                      key = '', secret = '')
#' )
#' print(wallet_all)
#' 
#' # get order book from market
#' market.api.process(market = 'kraken', action = 'order_book', debug = 10)
#' # get order book for all markets and combine
#' order_book_all <- list(
#'   'mtgox' = market.api.process(market = 'mtgox', action = 'order_book'),
#'   'bitstamp' = market.api.process(market = 'bitstamp', action = 'order_book'),
#'   'btce' = market.api.process(market = 'btce', action = 'order_book'),
#'   'kraken' = market.api.process(market = 'kraken', action = 'order_book')
#' )
#' print(order_book_all[['mtgox']][['bids']])
#' str(order_book_all)
#' }
market.api.process <- function(market, action, req, ..., debug = 0, raw.query.res = FALSE, on.market.error = expression(stop(e))){
  fun_name <- 'market.api.process'
  #if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching market.api.process',sep='')
  if(!(action %in% c('ticker','wallet','order_book'))) stop(paste0(fun_name,': unsupported action - function in development'))
  #R package check warning prevention:
  api.dict <- NULL; pre_process <- NULL; post_process <- NULL; catch_market_error <- NULL
  #load API dictionary
  data("api.dict", package = "Rbitcoin", envir = environment())
  #browser()
  v_market <- market; rm(market) #conflict data.table vars
  v_action <- action; rm(action)
  setkey(api.dict,market,action)
  #preprocess req for market
  if(!missing(req)){
    req <- api.dict[market == eval(v_market) & action == eval(v_action),pre_process][[1]](req)
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request pre-processed',sep='')
  }
  #api.query
  res <- market.api.query(market = v_market, 
                          url = api.dict[market == eval(v_market) & action == eval(v_action),url], 
                          ..., 
                          debug = debug - 1)
  if(raw.query.res){
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': raw.query.res=TRUE, returning raw object fromJSON',sep='')
    return(res)
  } 
  res <- tryCatch(
    expr = {
      #catch market's internal errors
      res <- api.dict[market == eval(v_market) & action == eval(v_action),catch_market_error][[1]](res) #transcode kind of " x[['error']] " to stop()
      #postprocess res from market
      res <- api.dict[market == eval(v_market) & action == eval(v_action),post_process][[1]](res)
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': result post-processed',sep='')
      res
    },
    error = function(e){
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': error catched, will result eval(on.market.error) param',sep='')
      eval(on.market.error) #catch the error as you wish
    })
  return(res)
}

# market api query -----------------------------------------------------

#' @title Send request to market API
#'
#' @description Route a request to particular market function. Also checks if ssl.verify and missing certificate file then downloads it.
#'
#' @param market character which identifies market on which we want to send request: mtgox, bitstamp, btce, kraken.
#' @param \dots objects to be passed to API: \code{url}, \code{key}, \code{secret}, \code{req}, \code{client_id} (used on bitstamp), \code{method} (used on btce).
#' @param ssl.verify logical flag to use HTTP over SSL, if missing certificate file it will be downloaded.
#' @param curl.verbose logical flag to display RCurl debug messages.
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @param on.error expression to be returned on R level error of market specific function for \code{market.api.query}. It does not catch internal market's error returned as valid object.
#' @return R object created by fromJSON decoded result from market's API call.
#' @details To do not spam market's API, use \code{Sys.sleep(10)} between API calls.
#' @note It is advised to use this function instead of calling market's function directly. If calling directly one should ensure to send any numeric values in non-exponential notation: \code{options(scipen=100)}. 
#' @seealso \code{\link{market.api.process}}, \code{\link{market.api.query.mtgox}}, \code{\link{market.api.query.bitstamp}}, \code{\link{market.api.query.btce}}, \code{\link{market.api.query.kraken}}
#' @references API documentation: \url{https://bitbucket.org/nitrous/mtgox-api} \url{https://www.bitstamp.net/api/} \url{https://btc-e.com/api/documentation} \url{https://www.kraken.com/help/api}
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query(market = 'mtgox', 
#'                  url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker')
#' market.api.query(market = 'bitstamp', 
#'                  url = 'https://www.bitstamp.net/api/ticker/')
#' market.api.query(market = 'btce', 
#'                  url = 'https://btc-e.com/api/2/btc_usd/ticker')
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/public/Ticker?pair=XXBTZEUR')
#' # wallet
#' market.api.query(market = 'mtgox', 
#'                  url = 'https://data.mtgox.com/api/2/BTCUSD/money/info', 
#'                  key = '', 
#'                  secret = '')
#' market.api.query(market = 'bitstamp', 
#'                  url = 'https://www.bitstamp.net/api/balance/', 
#'                  client_id = '', 
#'                  key = '', 
#'                  secret = '')
#' market.api.query(market = 'btce', 
#'                  url = 'https://btc-e.com/tapi', 
#'                  method = 'getInfo', 
#'                  key = '', 
#'                  secret = '')
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/private/Balance', 
#'                  key = '', 
#'                  secret = '')
#' # order book
#' market.api.query(market = 'mtgox', 
#'                  url = 'https://data.mtgox.com/api/2/BTCUSD/money/depth/fetch')
#' # place order
#' market.api.query(market = 'mtgox', 
#'                  url = 'https://data.mtgox.com/api/2/BTCUSD/money/order/add', 
#'                  key = '', 
#'                  secret = '',
#'                  req = list(type = 'ask', #sell
#'                             amount_int = trunc(0.1 * 100000000), #0.1 btc
#'                             price_int = trunc(1000 * 100000))) #1000 usd
#' # open orders
#' market.api.query(market = 'mtgox',
#'                  url = 'https://data.mtgox.com/api/2/BTCUSD/money/orders', 
#'                  key = '', 
#'                  secret = '')
#' # debugging
#' market.api.query(market = 'mtgox', 
#'                  url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker_fast', 
#'                  curl.verbose = FALSE, 
#'                  debug = 10)
#' market.api.query(market = 'mtgox', 
#'                  url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker_fast', 
#'                  ssl.verify = FALSE,
#'                  curl.verbose = TRUE, 
#'                  debug = 10)
#' market.api.query(market = 'mtgox', 
#'                  url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker_fast', 
#'                  ssl.verify = TRUE,
#'                  curl.verbose = TRUE, 
#'                  debug = 10)
#' }
market.api.query <- function(market, ..., ssl.verify = FALSE, curl.verbose = FALSE, debug = 0, on.error = expression(stop(e))){
  fun_name <- 'market.api.query'
  if(ssl.verify) {
    if(!file.exists('cacert.pem')){
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': ssl.verify TRUE but no certificate file \'cacert.pem\' in working directory, downloading from: http://curl.haxx.se/ca/cacert.pem',sep='')
      download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
    }
  }
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': switch query for particular market: ',market,sep='')
  old.scipen <- options(scipen=100) #it happend that R send a exponential sci notation which is not supported by markets, setting this localy, will revert after function call to previous value
  query_result <- tryCatch(expr = {
    query_result <- switch(market,
                           'mtgox' = market.api.query.mtgox(..., ssl.verify = ssl.verify, curl.verbose = curl.verbose, debug = debug - 1),
                           'bitstamp' = market.api.query.bitstamp(..., ssl.verify = ssl.verify, curl.verbose = curl.verbose, debug = debug - 1),
                           'btce' = market.api.query.btce(..., ssl.verify = ssl.verify, curl.verbose = curl.verbose, debug = debug - 1),
                           'kraken' = market.api.query.kraken(..., ssl.verify = ssl.verify, curl.verbose = curl.verbose, debug = debug - 1),
                           stop(paste0(fun_name,': unsupported market: ',market)))
    options(old.scipen)
    query_result
  },
           error = function(e){
             options(old.scipen) #revert previous value on error
             eval(on.error) #error handling
                    })
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': query completed',sep='')
  return(query_result)
}

#' @title Send request to mtgox market API
#'
#' @description Send request to mtgox market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param ssl.verify logical flag to use HTTP over SSL.
#' @param curl.verbose logical flag to display RCurl debug messages.
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @return R object created by fromJSON decoded result from market's API call.
#' @references \url{https://bitbucket.org/nitrous/mtgox-api}
#' @seealso \code{\link{market.api.query}}, \code{\link{market.api.query.bitstamp}}, \code{\link{market.api.query.btce}}, \code{\link{market.api.query.kraken}}
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.mtgox(url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker_fast')
#' # wallet
#' market.api.query.mtgox(url = 'https://data.mtgox.com/api/2/BTCUSD/money/info', 
#'                        key = '', 
#'                        secret = '')
#' }
market.api.query.mtgox <- function(url, key, secret, req, ssl.verify = FALSE, curl.verbose = FALSE, debug = 0){
  fun_name <- 'market.api.query.mtgox' #used on printing on debug messages to console
  if(!missing(req) | (!missing(key) & !missing(secret))){
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    tonce <- as.character(as.numeric(Sys.time()) * 1000000)
    post_data <- paste0('tonce=',tonce)
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': tonce calculated',sep='')
    if(!missing(req)){
      for(i in 1:length(names(req))){
        if(nchar(post_data) > 0 & substr(post_data,nchar(post_data),nchar(post_data)) != '&') post_data <- paste0(post_data,'&')
        post_data  <- paste0(post_data , names(req)[i],'=',req[[names(req)[i]]])
      }
    }
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
    if(!missing(key) & !missing(secret)){
      method_path <- substr(url,30,nchar(url))
      sign <- hmac(key = base64Decode(secret,mode='raw'), object = c(charToRaw(method_path),as.raw(0),charToRaw(post_data)), algo = 'sha512', raw = TRUE)
      httpheader <- c('Rest-Key' = key, 'Rest-Sign' = base64Encode(sign))
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
  }
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call with ssl.verify=',ssl.verify,' on url=\'',url,'\'',sep='')
  #preparing rcurl options
  useragent <- paste0(
    'R ',paste(c(R.version$major,R.version$minor),collapse='.'),
    "::Rbitcoin ",packageVersion("Rbitcoin")
  )
  if(ssl.verify) curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, cainfo = 'cacert.pem', verbose = curl.verbose, useragent = useragent)
  else curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, verbose = curl.verbose, useragent = useragent)
  #rcurl call
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(!missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                         postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}

#' @title Send request to bitstamp market API
#'
#' @description Send request to bitstamp market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param client_id character. Bitstamp market specific parameter used in private API call authorization (check reference for more information).
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param ssl.verify logical flag to use HTTP over SSL.
#' @param curl.verbose logical flag to display RCurl debug messages.
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @return R object created by fromJSON decoded result from market's API call.
#' @references \url{https://www.bitstamp.net/api/}
#' @seealso \code{\link{market.api.query}}, \code{\link{market.api.query.mtgox}}, \code{\link{market.api.query.btce}}, \code{\link{market.api.query.kraken}}
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.bitstamp(url = 'https://www.bitstamp.net/api/ticker/')
#' # wallet
#' market.api.query.bitstamp(url = 'https://www.bitstamp.net/api/balance/', 
#'                           client_id = '', 
#'                           key = '', 
#'                           secret = '')
#' }
market.api.query.bitstamp <- function(url, client_id, key, secret, req, ssl.verify = FALSE, curl.verbose = FALSE, debug = 0){
  fun_name <- 'market.api.query.bitstamp'
  if(!missing(req) | (!missing(key) & !missing(secret) & !missing(client_id))){
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(as.numeric(Sys.time()) * 1000000)
    if(!missing(key) & !missing(secret)){
      sign <- toupper(hmac(key = secret, object = paste0(nonce,client_id,key), algo = 'sha256'))
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha256',sep='')
    }
    post_data <- paste0('key=',key,'&signature=',sign,'&nonce=',nonce)
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(!missing(req)){
      for(i in 1:length(names(req))){
        if(nchar(post_data) > 0 & substr(post_data,nchar(post_data),nchar(post_data)) != '&') post_data <- paste0(post_data,'&')
        post_data  <- paste0(post_data , names(req)[i],'=',req[[names(req)[i]]])
      }
    }
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
  }
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call with ssl.verify=',ssl.verify,' on url=\'',url,'\'',sep='')
  #preparing rcurl options
  useragent <- paste0(
    'R ',paste(c(R.version$major,R.version$minor),collapse='.'),
    "::Rbitcoin ",packageVersion("Rbitcoin")
  )
  if(ssl.verify) curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, cainfo = 'cacert.pem', verbose = curl.verbose, useragent = useragent)
  else curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, verbose = curl.verbose, useragent = useragent)
  #rcurl call
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(!missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                         postfields = post_data)) #, httpheader = httpheader
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}

#' @title Send request to btce market API
#'
#' @description Send request to btce market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param method character. Btce market specific parameter used in private API call authorization (check reference for more information).
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param ssl.verify logical flag to use HTTP over SSL.
#' @param curl.verbose logical flag to display RCurl debug messages.
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @return fromJSON decoded result from market's API call.
#' @references \url{https://btc-e.com/api/documentation}
#' @seealso \code{\link{market.api.query}}, \code{\link{market.api.query.mtgox}}, \code{\link{market.api.query.bitstamp}}, \code{\link{market.api.query.kraken}}
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.btce(url = 'https://btc-e.com/api/2/btc_usd/ticker')
#' # wallet
#' market.api.query.btce(url = 'https://btc-e.com/tapi', 
#'                       method = 'getInfo', 
#'                       key = '', 
#'                       secret = '')
#' }
market.api.query.btce <- function(url, method, key, secret, req, ssl.verify = FALSE, curl.verbose = FALSE, debug = 0){
  fun_name <- 'market.api.query.btce'
  if(!missing(req) | (!missing(key) & !missing(secret))){
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    if(missing(method)) stop(paste0(fun_name,': missing \'method\' param which is required in private api calls'))
    post_data <- paste0('method=',method)
    nonce <- as.character(trunc(as.numeric(Sys.time()))) #* 1000000
    post_data <- paste0(post_data,'&nonce=',nonce)
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(!missing(req)){
      for(i in 1:length(names(req))){
        if(nchar(post_data) > 0 & substr(post_data,nchar(post_data),nchar(post_data)) != '&') post_data <- paste0(post_data,'&')
        post_data  <- paste0(post_data , names(req)[i],'=',req[[names(req)[i]]])
      }
    }
    if(!missing(key) & !missing(secret)){
      sign <- hmac(key = secret, object = post_data, algo = 'sha512')
      httpheader <- c('Key' = key, 'Sign' = sign)
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
  }
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call with ssl.verify=',ssl.verify,' on url=\'',url,'\'',sep='')
  #preparing rcurl options
  useragent <- paste0(
    'R ',paste(c(R.version$major,R.version$minor),collapse='.'),
    "::Rbitcoin ",packageVersion("Rbitcoin")
  )
  if(ssl.verify) curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, cainfo = 'cacert.pem', verbose = curl.verbose, useragent = useragent)
  else curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, verbose = curl.verbose, useragent = useragent)
  #rcurl call
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(!missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                         postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}

#' @title Send request to kraken market API
#'
#' @description Send request to kraken market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param ssl.verify logical flag to use HTTP over SSL.
#' @param curl.verbose logical flag to display RCurl debug messages.
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @return R object created by fromJSON decoded result from market's API call.
#' @references \url{https://www.kraken.com/help/api}
#' @seealso \code{\link{market.api.query}}, \code{\link{market.api.query.mtgox}}, \code{\link{market.api.query.bitstamp}}, \code{\link{market.api.query.btce}}
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.kraken(url = 'https://api.kraken.com/0/public/Ticker?pair=XBTCZEUR')
#' # wallet
#' market.api.query.kraken(url = 'https://api.kraken.com/0/private/Balance', 
#'                         key = '', 
#'                         secret = '')
#' }
market.api.query.kraken <- function(url, key, secret, req, ssl.verify = FALSE, curl.verbose = FALSE, debug = 0){
  fun_name <- 'market.api.query.kraken' #used on printing on debug messages to console
  if(!missing(req) | (!missing(key) & !missing(secret))){
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(as.numeric(Sys.time()) * 1000000)
    post_data <- paste0('nonce=',nonce)
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(!missing(req)){
      for(i in 1:length(names(req))){
        if(nchar(post_data) > 0 & substr(post_data,nchar(post_data),nchar(post_data)) != '&') post_data <- paste0(post_data,'&')
        post_data  <- paste0(post_data , names(req)[i],'=',req[[names(req)[i]]])
      }
    }
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
    if(!missing(key) & !missing(secret)){
      method_path <- gsub("^.*?kraken.com","",url)
      sign <- hmac(
        key = base64Decode(secret,mode='raw'),
        object = c(
          charToRaw(method_path),
          digest(object = paste0(nonce,post_data), 
                 algo = 'sha256', serialize = FALSE,
                 raw = TRUE)
        ), 
        algo = 'sha512', 
        raw = TRUE)
      httpheader <- c('API-Key' = key, 'API-Sign' = base64Encode(sign))
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
  }
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call with ssl.verify=',ssl.verify,' on url=\'',url,'\'',sep='')
  #preparing rcurl options
  useragent <- paste0(
    'R ',paste(c(R.version$major,R.version$minor),collapse='.'),
    "::Rbitcoin ",packageVersion("Rbitcoin")
  )
  if(ssl.verify) curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, cainfo = 'cacert.pem', verbose = curl.verbose, useragent = useragent)
  else curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, verbose = curl.verbose, useragent = useragent)
  #rcurl call
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(!missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                         postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}