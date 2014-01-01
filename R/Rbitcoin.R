
# Rbitcoin - package level data ------------------------------------------------------------------

#' @title R & bitcoin integration
#'
#' @description Utilities related to Bitcoin. Core functionalities are:
#' \itemize{
#' \item \code{market.api.query} - launch query on market's API (\code{mtgox}, \code{bitstamp}, \code{btce}, \code{kraken}). Both public and private API calls supported.
#' \item \code{market.api.process} - integration of market's processing structures: pre-process of API request, post-process API result, market error catching. Input and output unified structure. Requires API dictionary definition, package built-in \code{\link{api.dict}} contains 6 currency pairs on 4 markets.
#' \item \code{blockchain.api.query} - launch query on blockchain.info API json interface.
#' \item support HTTP over SSL.
#' \item debug messages of \code{Rbitcoin}, debug messages of \code{RCurl}.
#' }
#' You need to note that \code{digest} package docs states: \emph{Please note that this package is not meant to be deployed for cryptographic purposes for which more comprehensive (and widely tested) libraries such as OpenSSL should be used}.\cr
#' To do not get banned by market's API anti-DDoS protection user should handle the antispam process, example: \code{Sys.sleep(10)}.\cr
#' It is advised to maintain your API keys security level as tight as possible, if you do not need withdraw api method be sure to disable it for api keys.\cr
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
#' @references Package discussion thread: \url{https://bitcointalk.org/index.php?topic=343504}
#' @seealso \code{\link{market.api.query}}, \code{\link{blockchain.api.query}}, \code{\link{market.api.process}}, \code{\link{api.dict}}
#' @docType package
#' @name Rbitcoin
#' @aliases bitcoin btc
NULL

# blockchain.api.query  -----------------------------------------------------

#' @title Query blockchain.info API
#'
#' @description Query bitcoin related data from blockchain.info.
#'
#' @param \dots params passed to blockchain.info API, specific for particular method, example \code{'bitcoin_address'} or \code{'tx_index'}, for more see references or examples.
#' @param method character. For details see references, currently supported \code{'Single Address'} and \code{'Single Transaction'}. If \code{method} missing the function will try to guess it based on first param in \dots.
#' @param ssl.verify logical flag to use HTTP over SSL, if missing certificate file it will be downloaded from \code{http://curl.haxx.se/ca/cacert.pem}.
#' @param curl.verbose logical flag to display RCurl debug messages.
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @references \url{https://blockchain.info/api/blockchain_api}
#' @seealso \code{\link{market.api.query}}
#' @export
#' @examples
#' \dontrun{
#' # query bitcoin address information - 'Single Address' method
#' # Rbitcoin donation address final balance in BTC
#' blockchain.api.query('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi')[['final_balance']]/100000000
#' # Rbitcoin donation address full details
#' blockchain.api.query('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi', debug = 1)
#' # some first wallet final balance in BTC
#' blockchain.api.query('1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa', limit = 0)[['final_balance']]/100000000
#' # some first wallet details (limit to 3 txs, skip two txs)
#' blockchain.api.query(method = 'Single Address',
#'                      bitcoin_address = '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa', limit = 3, offset = 2)
#' 
#' # query bitcoin transaction information - 'Single Transaction' method
#' # Some recent transaction of some first wallet
#' blockchain.api.query('e5c4de1c70cb6d60db53410e871e9cab6a0ba75404360bf4cda1b993e58d45f8', debug = 1)
#' }
blockchain.api.query <- function(... , method, ssl.verify = FALSE, curl.verbose = FALSE, debug = 0){
  fun_name <- 'blockchain.api.query'
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing started',sep='')
  if(ssl.verify) {
    if(!file.exists('cacert.pem')){
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': ssl.verify TRUE but no certificate file \'cacert.pem\' in working directory, downloading from: http://curl.haxx.se/ca/cacert.pem',sep='')
      download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
    }
  }
  input_list <- list(...)
  if(missing(method)){
    if(length(input_list) < 1) stop(paste0('missing method and missing ... param'))
    if(nchar(input_list[[1]]) == 34 | any(names(input_list[1]) == 'bitcoin_address')) method <- 'Single Address' #any used to handle NULL names
    else if(nchar(input_list[[1]]) == 64 | any(names(input_list[1]) == 'tx_index')) method <- 'Single Transaction'
    else stop(paste0('missing method and invalid first ... param'))
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': missing method set as \'',method,'\' based on first param in ...',sep='')
  }
  url <- switch(method,
                'Single Address' = {
                  url <- 'https://blockchain.info/address/'
                  if(!is.null(input_list[['bitcoin_address']])) url <- paste0(url,input_list[['bitcoin_address']],'?format=json')
                  else {
                    if(length(input_list) < 1) stop(paste0('missing params in ... for ',method,' method'))
                    if(nchar(input_list[[1]]) == 34) url <- paste0(url,input_list[[1]],'?format=json')
                    else stop(paste0('invalid params in ... for ',method,' method, when providing unnamed params the first one must be bitcoin address with nchar = 34'))
                  }
                  if(!is.null(input_list[['limit']])) url <- paste0(url,'&limit=',input_list[['limit']])
                  if(!is.null(input_list[['offset']])) url <- paste0(url,'&offset=',input_list[['offset']])
                  url
                  },
                'Single Transaction' = {
                  url <- 'https://blockchain.info/tx-index/'
                  if(!is.null(input_list[['tx_index']])) url <- paste0(url,input_list[['tx_index']],'?format=json')
                  else {
                    if(length(input_list) < 1) stop(paste0('missing params in ... for ',method,' method'))
                    if(nchar(input_list[[1]]) == 64) url <- paste0(url,input_list[[1]],'?format=json')
                    else stop(paste0('invalid params in ... for ',method,' method, when providing unnamed params the first one must be tx index with nchar = 64'))
                  }
                  if(!is.null(input_list[['scripts']])) url <- paste0(url,'&scripts=',tolower(as.character(input_list[['scripts']])))
                  if(!is.null(input_list[['format']])) stop(paste0('format param not supported, hardcoded for \'json\''))
                  url
                },
                stop(paste0(fun_name,': unsupported method: ',method)))
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': url prepared: ',url,sep='')
  #preparing rcurl options
  useragent <- paste0(
    'R ',paste(c(R.version$major,R.version$minor),collapse='.'),
    "::Rbitcoin ",packageVersion("Rbitcoin")
  )
  if(ssl.verify) curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, cainfo = 'cacert.pem', verbose = curl.verbose, useragent = useragent)
  else curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, verbose = curl.verbose, useragent = useragent)
  res_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': API call executed',sep='')
  res <- fromJSON(res_json)
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': call result processed using fromJSON',sep='')
  return(res)
}

# market.api.process -----------------------------------------------------------

#' @name api.dict
#' @title API dictionary
#' @description This data set contains dictionary (data.table object) for market specific url, internal market's method name, logic of pre-process, post-process, markets error catch. Run \code{data(api.dict); api.dict} to print built-in dictionary. Granularity of data is \code{c(market, base, quote, action)}. This dictionary can be edited/extended by user for new currency pairs.\cr Currently supported currency pairs:
#' \itemize{
#' \item \code{mtgox: BTCUSD}
#' \item \code{bitstamp: BTCUSD}
#' \item \code{btce: LTCUSD, LTCBTC}
#' \item \code{kraken: BTCEUR, LTCEUR}
#' }
#' @usage data(api.dict)
#' @note Do not use \code{api.dict} from untrusted source or at least read whole it's code to ensure it is safe! The api dictionary was not fully tested, please follow the examples, if you find any bugs please report.
#' @docType data
#' @author Jan Gorecki, 2013-12-31
#' @keywords datasets
NULL

#' @title Process market API
#'
#' @param market character, example: \code{'mtgox'}.
#' @param currency_pair character vector of length 2, ex. \code{c(base = 'BTC', quote = 'USD')}. Order does matter.
#' @param action character, defined process to get organized data.
#' @param req list with action details (price, amount, tid, oid, etc.) unified across the markets specific per action, see examples.
#' @param \dots objects to be passed to \code{\link{market.api.query}}
#' \itemize{
#' \item auth params: \code{key}, \code{secret}, \code{client_id} (used on bitstamp),
#' \item technical param: \code{curl.verbose}.
#' }
#' @param ssl.verify logical default \code{FALSE}. For details read \code{\link{market.api.query}}.
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @param on.market.error expression to be evaluated on market level error. Rules specified in \code{\link{api.dict}}.
#' @param on.error expression to be evaluated on R level error related to \code{market.api.query}. For details read \code{\link{market.api.query}}.
#' @param api.dict data.table user custom API dictionary definition, if not provided function will use default Rbitcoin \code{\link{api.dict}}.
#' @param raw.query.res logical skip post-processing are return only after fromJSON processing.
#' @description Processing of API call according to API dictionary \code{\link{api.dict}}. Pre processing of request and post processing of API call results to unified structure across markets. It will result truncation of most (not common across the markets) attributes returned. If you need the full set of data returned by market's API you should use \code{\link{market.api.query}}.
#' @return Returned value depends on the \code{action} param. All actions will return market, currency pair (except \code{wallet} and \code{open_orders} which returns all currencies), R timestamp, market timestamp and below data (in case if market not provide particular data, it will result \code{NA} value):
#' \itemize{
#' \item \code{'ticker'} returns \code{data.table} with fields: \code{last}, \code{vwap}, \code{volume}, \code{ask}, \code{bid}.
#' \item \code{'wallet'} returns \code{data.table} with fields: \code{currency}, \code{amount}, \code{fee}.
#' \item \code{'order_book'} returns \code{list} with API call level attributes and sub elements \code{[['asks']]} and \code{[['bids']]} as \code{data.table} objects with order book including already calculated cumulative \code{amount}, \code{price} and \code{value}.
#' \item \code{'open_orders'} returns \code{data.table} with fields: \code{oid}, \code{type}, \code{price}, \code{amount}.
#' \item \code{'place_limit_order'} returns \code{data.table} with fields: \code{oid}, \code{type}, \code{price}, \code{amount}.
#' \item \code{'cancel_order'} returns \code{data.table} with fields: \code{oid}.
#' \item \code{'trades'} returns \code{list} with API call level attributes and sub element \code{[['trades']]} as \code{data.table} (ASC order) with fields: \code{date}, \code{price}, \code{amount}, \code{tid}, \code{type}.
#' }
#' @note The api dictionary was not fully tested, please follow the examples, if you find any bugs please report. Use only api dictionary \code{\link{api.dict}} from trusted source, in case if you use other \code{api.dict} it is advised to review pre-process, post-process and catch_market_error functions for markets and currency pairs you are going to use. Market level error handling might not fully work as not all markets returns API call status information.
#' 
#' @seealso \code{\link{market.api.query}}
#' @export
#' @examples
#' \dontrun{
#' # get ticker from market
#' market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'ticker', 
#'                    debug = 10)
#' # get ticker from all markets and combine
#' ticker_all <- rbindlist(list(
#'   market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'ticker'),
#'   market.api.process(market = 'bitstamp', currency_pair = c('BTC', 'USD'), action = 'ticker'),
#'   market.api.process(market = 'btce', currency_pair = c('LTC', 'USD'), action = 'ticker'),
#'   market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'ticker')
#' ))
#' print(ticker_all)
#' 
#' # get wallet from market
#' market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'wallet', 
#'                    key = '', secret = '', debug = 10)
#' # get wallet from all markets and combine
#' wallet_all <- rbindlist(list(
#'   market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'wallet',
#'                      key = '', secret = ''),
#'   market.api.process(market = 'bitstamp', currency_pair = c('BTC', 'USD'), action = 'wallet',
#'                      client_id = '', key = '', secret = ''),
#'   market.api.process(market = 'btce', currency_pair = c('LTC', 'USD'), action = 'wallet',
#'                      method = '', key = '', secret = ''),
#'   market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'wallet',
#'                      key = '', secret = '')
#' ))
#' print(wallet_all)
#' 
#' # get order book from market
#' market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'order_book', 
#'                    debug = 10)
#' 
#' # get open orders from market
#' market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'open_orders', 
#'                    key = '', secret = '', debug = 10)
#' 
#' # place limit order
#' market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'place_limit_order',
#'                    req = list(type = 'buy', amount = 1, price = 800),
#'                    key = '', secret = '', debug = 10)
#' 
#' # cancel order
#' market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'cancel_order, 
#'                    req = list(oid = ''),
#'                    key = '', secret = '', debug = 10)
#' 
#' # get trades
#' market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'trades',
#'                    debug = 10)
#' }
market.api.process <- function(market, currency_pair, action, req = list(), ..., ssl.verify = FALSE, debug = 0, on.market.error = expression(stop(e[['message']], call. = FALSE)), on.error = expression(stop(e[['message']], call. = FALSE)), api.dict = NULL, raw.query.res = FALSE){
  fun_name <- 'market.api.process'
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing started for ',market,' ',action,sep='')
  #R package check warning NOTE prevention:
  pre_process <- NULL; post_process <- NULL; catch_market_error <- NULL; base <- NULL; quote <- NULL
  if(is.null(api.dict)) data("api.dict", package = "Rbitcoin", envir = environment())
  v_market <- market; rm(market) #conflict data.table vars
  v_action <- action; rm(action)
  setkey(api.dict,market,base,quote,action)
  api.dict.filter <- expression(J(v_market,currency_pair[[1]],currency_pair[[2]],v_action))
  v_url <- api.dict[eval(api.dict.filter)][,url]
  if(is.null(v_url) | is.na(v_url)) stop(paste0('missing api.dict data for particular set: ',v_market,', ',currency_pair[[1]],', ',currency_pair[[2]],', ',v_action))
  #preprocess req for market
  v_req <- api.dict[eval(api.dict.filter)][,pre_process][[1]](req)  #possible overwrite 'v_url' variable for GET or bitstamp place limit order
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': API call request pre-processed',sep='')
  res <- market.api.query(market = v_market, 
                          url = v_url, 
                          req = v_req,
                          ..., 
                          ssl.verify = ssl.verify,
                          debug = debug - 1,
                          on.error = on.error)
  if(raw.query.res){
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': raw.query.res=TRUE, returning raw object fromJSON',sep='')
    return(res)
  }
  #catch market's internal errors
  res <- tryCatch(
    expr = {
      res <- api.dict[eval(api.dict.filter)][,catch_market_error][[1]](res) #transcode kind of " x[['error']] " to stop()
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': market level errors checked, no errors occurred',sep='')
      res
    },
    error = function(e){
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': market level error catched, will result eval(on.market.error) param',sep='')
      eval(on.market.error)
    })
  #postprocess res from market
  res <- api.dict[eval(api.dict.filter)][,post_process][[1]](res)
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': API call result post-processed',sep='')
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing finished for ',v_market,' ',v_action,sep='')
  return(res)
}

# market.api.query -----------------------------------------------------

#' @title Send request to market API
#'
#' @description Route a request to particular market function. Also checks if ssl.verify and missing certificate file then downloads it.
#'
#' @param market character which identifies market on which we want to send request: mtgox, bitstamp, btce, kraken.
#' @param \dots objects to be passed to API: \code{url}, \code{key}, \code{secret}, \code{req}, \code{client_id} (used on bitstamp).
#' @param ssl.verify logical flag to use HTTP over SSL, if missing certificate file it will be downloaded from \code{http://curl.haxx.se/ca/cacert.pem}.
#' @param curl.verbose logical flag to display RCurl debug messages.
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @param on.error expression to be evaluated on R level error of market specific function. It does not catch internal market's error returned as valid object.
#' @return R object created by fromJSON decoded result from market's API call.
#' @details To do not spam market's API, use \code{Sys.sleep(10)} between API calls.
#' @note It is advised to use this function instead of calling market's function directly. If calling directly one should ensure to send any numeric values in non-exponential notation: \code{options(scipen=100)}. 
#' @seealso \code{\link{market.api.process}}, \code{\link{market.api.query.mtgox}}, \code{\link{market.api.query.bitstamp}}, \code{\link{market.api.query.btce}}, \code{\link{market.api.query.kraken}}
#' @references API documentation: \url{https://bitbucket.org/nitrous/mtgox-api}, \url{https://www.bitstamp.net/api/}, \url{https://btc-e.com/api/documentation}, \url{https://www.kraken.com/help/api}
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
#'                  req = list(method = 'getInfo'), 
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
market.api.query <- function(market, ..., ssl.verify = FALSE, curl.verbose = FALSE, debug = 0, on.error = expression(stop(e[['message']], call. = FALSE))){
  fun_name <- 'market.api.query'
  if(ssl.verify) {
    if(!file.exists('cacert.pem')){
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': ssl.verify TRUE but no certificate file \'cacert.pem\' in working directory, downloading from: http://curl.haxx.se/ca/cacert.pem',sep='')
      download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
    }
  }
  #browser()
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
                             options(old.scipen) #revert previous option scipen value on error
                             if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': R level error catched, will result eval(on.error) param',sep='')
                             #if(!is.null(e[['message']])) e <- e[['message']]
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
market.api.query.mtgox <- function(url, key, secret, req = list(), ssl.verify = FALSE, curl.verbose = FALSE, debug = 0){ 
  fun_name <- 'market.api.query.mtgox' #used on printing on debug messages to console
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    tonce <- as.character(as.numeric(Sys.time()) * 1000000)
    post_data <- paste0('tonce=',tonce)
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': tonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
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
  if(ssl.verify) curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, cainfo = 'cacert.pem', verbose = curl.verbose, useragent = paste0("R ",packageVersion("base"),"::Rbitcoin ",packageVersion("Rbitcoin")))
  else curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, verbose = curl.verbose, useragent = paste0("R ",packageVersion("base"),"::Rbitcoin ",packageVersion("Rbitcoin")))
  #rcurl call
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name), call. = FALSE)
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
#' @return R object created by fromJSON decoded result from market's API call. Cancel order is an exception handled by hardcode, as bitstamp will not return json format for that method.
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
market.api.query.bitstamp <- function(url, client_id, key, secret, req = list(), ssl.verify = FALSE, curl.verbose = FALSE, debug = 0){
  fun_name <- 'market.api.query.bitstamp'
  if(length(req) > 0 | (!missing(key) & !missing(secret) & !missing(client_id))){
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(as.numeric(Sys.time()) * 1000000)
    if(!missing(key) & !missing(secret)){
      sign <- toupper(hmac(key = secret, object = paste0(nonce,client_id,key), algo = 'sha256'))
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha256',sep='')
    }
    post_data <- paste0('key=',key,'&signature=',sign,'&nonce=',nonce)
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
  }
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call with ssl.verify=',ssl.verify,' on url=\'',url,'\'',sep='')
  #preparing rcurl options
  if(ssl.verify) curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, cainfo = 'cacert.pem', verbose = curl.verbose, useragent = paste0("R ",packageVersion("base"),"::Rbitcoin ",packageVersion("Rbitcoin")))
  else curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, verbose = curl.verbose, useragent = paste0("R ",packageVersion("base"),"::Rbitcoin ",packageVersion("Rbitcoin")))
  #rcurl call
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  if(url == 'https://www.bitstamp.net/api/cancel_order/') query_result <- query_result_json else query_result <- fromJSON(query_result_json)
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}

#' @title Send request to btce market API
#'
#' @description Send request to btce market API.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc. See note.
#' @param ssl.verify logical flag to use HTTP over SSL.
#' @param curl.verbose logical flag to display RCurl debug messages.
#' @param debug integer. Rbitcoin debug messages if \code{debug > 0}, each subfunction reduce \code{debug} by 1.
#' @return fromJSON decoded result from market's API call.
#' @note Market specific btce \code{method} param should be provided in \code{req} object.
#' @references \url{https://btc-e.com/api/documentation}
#' @seealso \code{\link{market.api.query}}, \code{\link{market.api.query.mtgox}}, \code{\link{market.api.query.bitstamp}}, \code{\link{market.api.query.kraken}}
#' @export
#' @examples
#' \dontrun{
#' # ticker
#' market.api.query.btce(url = 'https://btc-e.com/api/2/btc_usd/ticker')
#' # wallet
#' market.api.query.btce(url = 'https://btc-e.com/tapi', 
#'                       req = list(method = 'getInfo'), 
#'                       key = '', 
#'                       secret = '')
#' }
market.api.query.btce <- function(url, key, secret, req = list(), ssl.verify = FALSE, curl.verbose = FALSE, debug = 0){
  fun_name <- 'market.api.query.btce'
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(trunc(as.numeric(Sys.time()))) #* 1000000
    post_data <- paste0('nonce=',nonce)
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&') 
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
    if(!missing(key) & !missing(secret)){
      sign <- hmac(key = secret, object = post_data, algo = 'sha512')
      httpheader <- c('Key' = key, 'Sign' = sign)
      if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
  }
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call with ssl.verify=',ssl.verify,' on url=\'',url,'\'',sep='')
  #preparing rcurl options
  if(ssl.verify) curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, cainfo = 'cacert.pem', verbose = curl.verbose, useragent = paste0("R ",packageVersion("base"),"::Rbitcoin ",packageVersion("Rbitcoin")))
  else curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, verbose = curl.verbose, useragent = paste0("R ",packageVersion("base"),"::Rbitcoin ",packageVersion("Rbitcoin")))
  #rcurl call
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 | !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
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
market.api.query.kraken <- function(url, key, secret, req = list(), ssl.verify = FALSE, curl.verbose = FALSE, debug = 0){
  fun_name <- 'market.api.query.kraken' #used on printing on debug messages to console
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(as.numeric(Sys.time()) * 1000000)
    post_data <- paste0('nonce=',nonce)
    if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
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
  if(ssl.verify) curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, cainfo = 'cacert.pem', verbose = curl.verbose, useragent = paste0("R ",packageVersion("base"),"::Rbitcoin ",packageVersion("Rbitcoin")))
  else curl <- getCurlHandle(ssl.verifypeer = ssl.verify, ssl.verifyhost = ssl.verify, verbose = curl.verbose, useragent = paste0("R ",packageVersion("base"),"::Rbitcoin ",packageVersion("Rbitcoin")))
  #rcurl call
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 | !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(debug > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}
