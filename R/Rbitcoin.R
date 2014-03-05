
# Rbitcoin - package level data ------------------------------------------------------------------

#' @title R & bitcoin integration
#'
#' @description Utilities related to Bitcoin. Core functionalities are:
#' \itemize{
#' \item \code{market.api.query} - launch query on market's API (\code{mtgox}, \code{bitstamp}, \code{btce}, \code{kraken}). Both public and private API calls supported. All currency pairs supported.
#' \item \code{market.api.process} - integration of market's processing structures: pre-process of API request, post-process API results, market error catching. Input and output unified structure. Requires API dictionary definition, for details of package built-in dictionary see \code{\link{api.dict}}.
#' \item \code{blockchain.api.query} - launch query on blockchain.info API json interface.
#' }
#' You need to note that \code{digest} package docs states: \emph{Please note that this package is not meant to be deployed for cryptographic purposes for which more comprehensive (and widely tested) libraries such as OpenSSL should be used}.\cr
#' To do not get banned by market's API anti-DDoS protection user should handle the antispam process, example: \code{Sys.sleep(10)} between the API calls.\cr
#' It is advised to maintain your API keys security level as tight as possible, if you do not need withdraw api method be sure to disable it for api keys.\cr
#' You can print debug messages of \code{Rbitcoin} to console using verbose argument in FUNs or \code{options("Rbitcoin.verbose" = 1)}.\cr
#' Two params \code{ssl.verify} and \code{curl.verbose} have been deprecated since \code{0.8.5}. They can and should be controlled using \code{options("RCurlOptions")}. SSL verify is by default active.\cr
#' At the time of writing the most recent market's API version were used:
#' \itemize{
#' \item mtgox v2 (market is already dead but public API calls still returns the values)
#' \item bitstamp v2 (public) / ? (private)
#' \item btce v2 (public) / "tapi" (private)
#' \item kraken v0
#' }
#' To set SSL and others package-level options see examples.
#' 
#' BTC donation: \url{bitcoin:15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi}
#' 
#' @references Package discussion thread: \url{https://bitcointalk.org/index.php?topic=343504}
#' @seealso \code{\link{market.api.query}}, \code{\link{blockchain.api.query}}, \code{\link{market.api.process}}, \code{\link{api.dict}}
#' @docType package
#' @import RJSONIO RCurl digest data.table
#' @name Rbitcoin
#' @aliases bitcoin btc
#' used in the package are: \code{"Rbitcoin.verbose"} and \code{"RCurlOptions"=list("ssl.verifypeer","ssl.verifyhost","cainfo","verbose")}. Default SSL support can be here turned off, also print debug messages to console
#' @examples
#' \dontrun{
#' # default options used by Rbitcoin
#' # print Rbitcoin processing to console set "Rbitcoin.verbose" to 1 (or more)
#' # print Rcurl processing to console set RCurlOptions[["verbose"]] to TRUE
#' # to disable SSL set ssl.verify* = FALSE and cainfo = NULL
#' options(Rbitcoin.verbose=0)
#' options(RCurlOptions=list(ssl.verifypeer = TRUE,
#'                           ssl.verifyhost = TRUE,
#'                           cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
#'                           verbose = FALSE))
#' }
NULL

# blockchain.api.query  -----------------------------------------------------

#' @title Query blockchain.info API
#'
#' @description Query bitcoin related data from blockchain.info.
#'
#' @param \dots params passed to blockchain.info API, specific for particular method, example \code{'bitcoin_address'} or \code{'tx_index'}, for more see references or examples.
#' @param method character. For details see references, currently supported \code{'Single Address'} and \code{'Single Transaction'}. If \code{method} missing the function will try to guess it based on first param in \dots.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose")} is used, by default \code{0}.
#' @references \url{https://blockchain.info/api/blockchain_api}
#' @seealso \code{\link{market.api.query}}
#' @export
#' @examples
#' \dontrun{
#' # query bitcoin address information - 'Single Address' method
#' # Rbitcoin donation address final balance in BTC
#' blockchain.api.query('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi',limit=0)[['final_balance']]/100000000
#' # Rbitcoin donation address full details
#' blockchain.api.query('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi',verbose=1)
#' # some first wallet final balance in BTC
#' blockchain.api.query('1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa',limit=0)[['final_balance']]/100000000
#' # some first wallet details (limit to 3 txs, skip two txs)
#' blockchain.api.query(method = 'Single Address',
#'                      bitcoin_address = '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa', limit=3, offset=2)
#' # query bitcoin transaction information - 'Single Transaction' method
#' # Some recent transaction of some first wallet
#' blockchain.api.query('e5c4de1c70cb6d60db53410e871e9cab6a0ba75404360bf4cda1b993e58d45f8')
#' }
blockchain.api.query <- function(... , method, verbose = getOption("Rbitcoin.verbose")){
  fun_name <- 'blockchain.api.query'
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing started',sep='')
  input_list <- list(...)
  if(missing(method)){
    if(length(input_list) < 1) stop(paste0('missing method and missing ... param'))
    if(nchar(input_list[[1]]) == 34 | any(names(input_list[1]) == 'bitcoin_address')) method <- 'Single Address' #any used to handle NULL names
    else if(nchar(input_list[[1]]) == 64 | any(names(input_list[1]) == 'tx_index')) method <- 'Single Transaction'
    else stop(paste0('missing method and invalid first ... param'))
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': missing method set as \'',method,'\' based on first param in ...',sep='')
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
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': url prepared: ',url,sep='')
  curl <- getCurlHandle(useragent = paste(paste("R",packageVersion("base")),paste("Rbitcoin",packageVersion("Rbitcoin")), sep="::"))
  res_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': API call executed',sep='')
  res <- fromJSON(res_json)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': call result processed using fromJSON',sep='')
  return(res)
}

# market.api.process -----------------------------------------------------------

#' @name api.dict
#' @title API dictionary
#' @description This data set contains dictionary (\code{\link{data.table}} object) for \code{\link{market.api.process}} function which perform pre-process API call request, post-process API call results and catch market level errors. Still there is function \code{\link{market.api.query}} that do not require any dictionary and can operate on any currency pairs. Run \code{data(api.dict); api.dict} to print built-in dictionary. Granularity of data is \code{c(market, base, quote, action)}. This dictionary can be edited/extended by user for new currency pairs.\cr Currently supported currency pairs:
#' \itemize{
#' \item \code{mtgox: BTCUSD}
#' \item \code{bitstamp: BTCUSD}
#' \item \code{btce: LTCUSD, LTCBTC, NMCBTC}
#' \item \code{kraken: BTCEUR, LTCEUR, BTCLTC}
#' }
#' @usage data(api.dict)
#' @note Do not use \code{api.dict} from untrusted source or read whole it's code to ensure it is safe! The api dictionary was not fully tested, please follow the examples, if you find any bugs please report.
#' @docType data
#' @author Jan Gorecki, 2014-03-05
#' @keywords datasets
NULL

#' @title Process market API
#'
#' @param market character, example: \code{'kraken'}.
#' @param currency_pair character vector of length 2, ex. \code{c(base = 'BTC', quote = 'EUR')}. Order does matter.
#' @param action character, defined process to get organized data.
#' @param req list with action details (price, amount, tid, oid, etc.) unified across the markets specific per action, see examples.
#' @param \dots objects to be passed to \code{\link{market.api.query}}
#' \itemize{
#' \item auth params: \code{key}, \code{secret}, \code{client_id} (used on bitstamp),
#' }
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose")} is used, by default \code{0}.
#' @param on.market.error expression to be evaluated on market level error. Rules specified in \code{\link{api.dict}}.
#' @param on.error expression to be evaluated on R level error related to \code{market.api.query}. For details read \code{\link{market.api.query}}.
#' @param api.dict data.table user custom API dictionary definition, if not provided function will use default Rbitcoin \code{\link{api.dict}}.
#' @param raw.query.res logical skip post-processing are return results only after fromJSON processing.
#' @details To do not spam market's API, use \code{Sys.sleep(10)} between API calls.
#' @description Processing of API call according to API dictionary \code{\link{api.dict}}. Limited to markets and currency processing defined in \code{api.dict}, use \code{\link{market.api.query}} for all currency pairs on all currently supported markets. This function perform pre processing of request and post processing of API call results to unified structure across markets. It will result truncation of most (not common across the markets) attributes returned. If you need the full set of data returned by market's API you should use \code{\link{market.api.query}}.
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
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action='ticker')
#' # get ticker from all markets and combine
#' ticker_all <- rbindlist(list(
#'   market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action='ticker')
#'   ,market.api.process(market = 'bitstamp', currency_pair = c('BTC', 'USD'), action='ticker')
#'   ,market.api.process(market = 'btce', currency_pair = c('LTC', 'USD'), action='ticker')
#'   ,{Sys.sleep(10);
#'     market.api.process(market = 'btce', currency_pair = c('LTC', 'BTC'), action='ticker')}
#'   ,{Sys.sleep(10);
#'     market.api.process(market = 'btce', currency_pair = c('NMC', 'BTC'), action='ticker')}
#'   ,market.api.process(market = 'kraken', currency_pair = c('BTC','EUR'), action='ticker')
#'   ,{Sys.sleep(10);
#'     market.api.process(market = 'kraken', currency_pair = c('LTC','EUR'), action='ticker')}
#'   ,{Sys.sleep(10);
#'     market.api.process(market = 'kraken', currency_pair = c('BTC','LTC'), action='ticker')}
#' ))
#' print(ticker_all)
#' 
#' # get wallet from market
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'wallet', 
#'                    key = '', secret = '')
#' # get wallet from all markets and combine
#' wallet_all <- rbindlist(list(
#'   #market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'wallet',
#'   #                   key = '', secret = ''),
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
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'order_book')
#' 
#' # get open orders from market
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'open_orders', 
#'                    key = '', secret = '')
#' 
#' # place limit order
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'place_limit_order',
#'                    req = list(type = 'sell', amount = 1, price = 8000), # sell 1 btc for 8000 eur
#'                    key = '', secret = '')
#' 
#' # cancel order
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'cancel_order, 
#'                    req = list(oid = 'oid_from_open_orders'),
#'                    key = '', secret = '')
#' # get trades
#' market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'trades')
#' }
market.api.process <- function(market, currency_pair, action, req = list(), ..., verbose = getOption("Rbitcoin.verbose"), on.market.error = expression(stop(e[['message']], call. = FALSE)), on.error = expression(stop(e[['message']], call. = FALSE)), api.dict = NULL, raw.query.res = FALSE){
  fun_name <- 'market.api.process'
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing started for ',market,' ',action,sep='')
  #R package check warning NOTE prevention:
  pre_process <- NULL; post_process <- NULL; catch_market_error <- NULL; base <- NULL; quote <- NULL
  if(is.null(api.dict)) data("api.dict", package = "Rbitcoin", envir = environment())
  v_market <- market; rm(market) #conflict data.table vars
  v_action <- action; rm(action)
  # bug fixed in 1.9.3 on CRAN should be released as 1.9.4
  if(packageVersion('data.table') < "1.9.4") api.dict <- api.dict[order(market,base,quote,action)]
  # ordering will be removed after dt bug fix
  setkeyv(api.dict,c("market","base","quote","action"))
  api.dict.filter <- expression(J(v_market,currency_pair[[1]],currency_pair[[2]],v_action))
  v_url <- api.dict[eval(api.dict.filter)][,url]
  if(is.null(v_url) | is.na(v_url)) stop(paste0('missing api.dict data for particular set: ',v_market,', ',currency_pair[[1]],', ',currency_pair[[2]],', ',v_action))
  #preprocess req for market
  v_req <- api.dict[eval(api.dict.filter)][,pre_process][[1]](req)  #possible overwrite 'v_url' variable for GET or bitstamp place limit order
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': API call request pre-processed',sep='')
  res <- market.api.query(market = v_market, 
                          url = v_url, 
                          req = v_req,
                          ..., 
                          verbose = verbose - 1,
                          on.error = on.error)
  if(raw.query.res){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': raw.query.res=TRUE, returning raw object fromJSON',sep='')
    return(res)
  }
  #catch market's internal errors
  res <- tryCatch(
    expr = {
      res <- api.dict[eval(api.dict.filter)][,catch_market_error][[1]](res) #transcode kind of " x[['error']] " to stop()
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': market level errors checked, no errors occurred',sep='')
      res
    },
    error = function(e){
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': market level error catched, will result eval(on.market.error) param',sep='')
      eval(on.market.error)
    })
  #postprocess res from market
  res <- api.dict[eval(api.dict.filter)][,post_process][[1]](res)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': API call result post-processed',sep='')
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': processing finished for ',v_market,' ',v_action,sep='')
  return(res)
}

# market.api.query -----------------------------------------------------

#' @title Send request to market API
#'
#' @description Route a request to particular market function.
#'
#' @param market character which identifies market on which we want to send request: mtgox, bitstamp, btce, kraken.
#' @param \dots objects to be passed to API: \code{url}, \code{key}, \code{secret}, \code{req}, \code{client_id} (used on bitstamp).
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose")} is used, by default \code{0}.
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
#'                  key = '', secret = '')
#' market.api.query(market = 'bitstamp', 
#'                  url = 'https://www.bitstamp.net/api/balance/', 
#'                  client_id = '', # bitstamp specific
#'                  key = '', secret = '')
#' market.api.query(market = 'btce', 
#'                  url = 'https://btc-e.com/tapi', 
#'                  req = list(method = 'getInfo'), 
#'                  key = '', secret = '')
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/private/Balance', 
#'                  key = '', secret = '')
#' # order book
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/public/Depth?pair=XXBTZEUR')
#' # open orders
#' market.api.query(market = 'kraken',
#'                  url = 'https://api.kraken.com/0/private/OpenOrders', 
#'                  key = '', secret = '')
#' # place order
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/private/AddOrder', 
#'                  key = '', secret = '',
#'                  req = list(pair = 'XXBTZEUR',
#'                             type = 'sell',
#'                             ordertype = 'limit',
#'                             price = 1200, # 1200 eur
#'                             volume = 0.1)) # 0.1 btc
#' # cancel order
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/private/CancelOrder', 
#'                  key = '', secret = '',
#'                  req = list(txid = 'id_from_open_orders'))
#' # trades
#' market.api.query(market = 'kraken', 
#'                  url = 'https://api.kraken.com/0/public/Trades?pair=XXBTZEUR')
#' }
market.api.query <- function(market, ..., verbose = getOption("Rbitcoin.verbose"), on.error = expression(stop(e[['message']], call. = FALSE))){
  fun_name <- 'market.api.query'
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': switch query for particular market: ',market,sep='')
  old.scipen <- options(scipen=100) #it happend that R send a exponential sci notation which is not supported by markets, setting this localy, will revert after function call to previous value
  query_result <- tryCatch(expr = {
    query_result <- switch(market,
                           'mtgox' = market.api.query.mtgox(..., verbose = verbose - 1),
                           'bitstamp' = market.api.query.bitstamp(..., verbose = verbose - 1),
                           'btce' = market.api.query.btce(..., verbose = verbose - 1),
                           'kraken' = market.api.query.kraken(..., verbose = verbose - 1),
                           stop(paste0(fun_name,': unsupported market: ',market)))
    options(old.scipen)
    query_result
  },
                           error = function(e){
                             options(old.scipen) #revert previous option scipen value on error
                             if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': R level error catched, will result eval(on.error) param',sep='')
                             eval(on.error) #error handling
                           })
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': query completed',sep='')
  return(query_result)
}

#' @title Send request to mtgox market API
#'
#' @description Send request to mtgox market API. MtGox is already closed but public API calls are working. Also it's code/dictionary can be reused in future.
#'
#' @param url character with url on which query needs to be passed.
#' @param key character API key used in private API calls.
#' @param secret character API secret used in private API calls.
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc.
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose")} is used, by default \code{0}.
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
#'                        key = '', secret = '')
#' }
market.api.query.mtgox <- function(url, key, secret, req = list(), verbose = getOption("Rbitcoin.verbose")){ 
  fun_name <- 'market.api.query.mtgox'
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    tonce <- as.character(as.numeric(Sys.time()) * 1000000)
    post_data <- paste0('tonce=',tonce)
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': tonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
    if(!missing(key) & !missing(secret)){
      method_path <- substr(url,30,nchar(url))
      sign <- hmac(key = base64Decode(secret,mode='raw'), object = c(charToRaw(method_path),as.raw(0),charToRaw(post_data)), algo = 'sha512', raw = TRUE)
      httpheader <- c('Rest-Key' = key, 'Rest-Sign' = base64Encode(sign))
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call on url=\'',url,'\'',sep='')
  curl <- getCurlHandle(useragent = paste(paste("R",packageVersion("base")),paste("Rbitcoin",packageVersion("Rbitcoin")), sep="::"))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name), call. = FALSE)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
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
#' @param req list of object passed to API: price and amount of opening order, id of cancelling order, etc..
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose")} is used, by default \code{0}.
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
#'                           key = '', secret = '')
#' }
market.api.query.bitstamp <- function(url, client_id, key, secret, req = list(), verbose = getOption("Rbitcoin.verbose")){
  fun_name <- 'market.api.query.bitstamp'
  if(length(req) > 0 | (!missing(key) & !missing(secret) & !missing(client_id))){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(as.numeric(Sys.time()) * 1000000)
    if(!missing(key) & !missing(secret)){
      sign <- toupper(hmac(key = secret, object = paste0(nonce,client_id,key), algo = 'sha256'))
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha256',sep='')
    }
    post_data <- paste0('key=',key,'&signature=',sign,'&nonce=',nonce)
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call on url=\'',url,'\'',sep='')
  curl <- getCurlHandle(useragent = paste(paste("R",packageVersion("base")),paste("Rbitcoin",packageVersion("Rbitcoin")), sep="::"))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 & !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- if(url == 'https://www.bitstamp.net/api/cancel_order/') query_result_json else fromJSON(query_result_json) #bitstamp exception, cancel order does not return json object
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
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
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose")} is used, by default \code{0}.
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
#'                       key = '', secret = '')
#' }
market.api.query.btce <- function(url, key, secret, req = list(), verbose = getOption("Rbitcoin.verbose")){
  fun_name <- 'market.api.query.btce'
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(trunc(as.numeric(Sys.time()))) # no multiply "* 1000000" due to btce handle nonce a little worse then other markets, we need smaller nonce values
    post_data <- paste0('nonce=',nonce)
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&') 
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
    if(!missing(key) & !missing(secret)){
      sign <- hmac(key = secret, object = post_data, algo = 'sha512')
      httpheader <- c('Key' = key, 'Sign' = sign)
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call on url=\'',url,'\'',sep='')
  curl <- getCurlHandle(useragent = paste(paste("R",packageVersion("base")),paste("Rbitcoin",packageVersion("Rbitcoin")), sep="::"))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 | !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
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
#' @param verbose integer. Rbitcoin processing messages, print to console if \code{verbose > 0}, each subfunction reduce \code{verbose} by 1. If missing then \code{getOption("Rbitcoin.verbose")} is used, by default \code{0}.
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
#'                         key = '', secret = '')
#' }
market.api.query.kraken <- function(url, key, secret, req = list(), verbose = getOption("Rbitcoin.verbose")){
  fun_name <- 'market.api.query.kraken'
  if(length(req) > 0 | (!missing(key) & !missing(secret))){
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': request param or auth keys provided',sep='')
    nonce <- as.character(as.numeric(Sys.time()) * 1000000)
    post_data <- paste0('nonce=',nonce)
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': nonce calculated',sep='')
    if(length(req) > 0) post_data <- paste(post_data, paste(paste(names(req),req,sep='='),collapse='&'),sep='&')
    if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': post data prepared',sep='')
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
      if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call signed with sha512',sep='')
    }
  }
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': launching api call on url=\'',url,'\'',sep='')
  curl <- getCurlHandle(useragent = paste(paste("R",packageVersion("base")),paste("Rbitcoin",packageVersion("Rbitcoin")), sep="::"))
  if(missing(key) | missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE))
  else if(length(post_data) > 0 | !missing(key) & !missing(secret)) query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE,
                                                                                                                 postfields = post_data, httpheader = httpheader))
  else stop(paste0('unhandled case on Rcurl functions calling in ',fun_name))
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call successfully completed',sep='')
  query_result <- fromJSON(query_result_json)
  if(verbose > 0) cat('\n',as.character(Sys.time()),': ',fun_name,': api call results processed from JSON to R object',sep='')
  return(query_result)
}
