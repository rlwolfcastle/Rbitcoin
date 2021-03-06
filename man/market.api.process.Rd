\name{market.api.process}
\alias{market.api.process}
\title{Process market API}
\usage{
market.api.process(market, currency_pair, action, req = list(), ...,
  verbose = getOption("Rbitcoin.verbose"),
  on.market.error = expression(stop(e[["message"]], call. = FALSE)),
  on.error = expression(stop(e[["message"]], call. = FALSE)),
  api.dict = NULL, raw.query.res = FALSE)
}
\arguments{
  \item{market}{character, example: \code{'kraken'}.}

  \item{currency_pair}{character vector of length 2, ex.
  \code{c(base = 'BTC', quote = 'EUR')}. Order does
  matter.}

  \item{action}{character, defined process to get organized
  data.}

  \item{req}{list with action details (price, amount, tid,
  oid, etc.) unified across the markets specific per
  action, see examples.}

  \item{\dots}{objects to be passed to
  \code{\link{market.api.query}} \itemize{ \item auth
  params: \code{key}, \code{secret}, \code{client_id} (used
  on bitstamp), }}

  \item{verbose}{integer. Rbitcoin processing messages,
  print to console if \code{verbose > 0}, each subfunction
  reduce \code{verbose} by 1. If missing then
  \code{getOption("Rbitcoin.verbose")} is used, by default
  \code{0}.}

  \item{on.market.error}{expression to be evaluated on
  market level error. Rules specified in
  \code{\link{api.dict}}.}

  \item{on.error}{expression to be evaluated on R level
  error related to \code{market.api.query}. For details
  read \code{\link{market.api.query}}.}

  \item{api.dict}{data.table user custom API dictionary
  definition, if not provided function will use default
  Rbitcoin \code{\link{api.dict}}.}

  \item{raw.query.res}{logical skip post-processing are
  return results only after fromJSON processing.}
}
\value{
Returned value depends on the \code{action} param. All
actions will return market, currency pair (except
\code{wallet} and \code{open_orders} which returns all
currencies), R timestamp, market timestamp and below data
(in case if market not provide particular data, it will
result \code{NA} value): \itemize{ \item \code{'ticker'}
returns \code{data.table} with fields: \code{last},
\code{vwap}, \code{volume}, \code{ask}, \code{bid}. \item
\code{'wallet'} returns \code{data.table} with fields:
\code{currency}, \code{amount}, \code{fee}. \item
\code{'order_book'} returns \code{list} with API call level
attributes and sub elements \code{[['asks']]} and
\code{[['bids']]} as \code{data.table} objects with order
book including already calculated cumulative \code{amount},
\code{price} and \code{value}. \item \code{'open_orders'}
returns \code{data.table} with fields: \code{oid},
\code{type}, \code{price}, \code{amount}. \item
\code{'place_limit_order'} returns \code{data.table} with
fields: \code{oid}, \code{type}, \code{price},
\code{amount}. \item \code{'cancel_order'} returns
\code{data.table} with fields: \code{oid}. \item
\code{'trades'} returns \code{list} with API call level
attributes and sub element \code{[['trades']]} as
\code{data.table} (ASC order) with fields: \code{date},
\code{price}, \code{amount}, \code{tid}, \code{type}. }
}
\description{
Processing of API call according to API dictionary
\code{\link{api.dict}}. Limited to markets and currency
processing defined in \code{api.dict}, use
\code{\link{market.api.query}} for all currency pairs on
all currently supported markets. This function perform pre
processing of request and post processing of API call
results to unified structure across markets. It will result
truncation of most (not common across the markets)
attributes returned. If you need the full set of data
returned by market's API you should use
\code{\link{market.api.query}}.
}
\details{
To do not spam market's API, use \code{Sys.sleep(10)}
between API calls.
}
\note{
The api dictionary was not fully tested, please follow the
examples, if you find any bugs please report. Use only api
dictionary \code{\link{api.dict}} from trusted source, in
case if you use other \code{api.dict} it is advised to
review pre-process, post-process and catch_market_error
functions for markets and currency pairs you are going to
use. Market level error handling might not fully work as
not all markets returns API call status information.
}
\examples{
\dontrun{
# get ticker from market
market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action='ticker')
# get ticker from all markets and combine
ticker_all <- rbindlist(list(
  market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action='ticker')
  ,market.api.process(market = 'bitstamp', currency_pair = c('BTC', 'USD'), action='ticker')
  ,market.api.process(market = 'btce', currency_pair = c('LTC', 'USD'), action='ticker')
  ,{Sys.sleep(10);
    market.api.process(market = 'btce', currency_pair = c('LTC', 'BTC'), action='ticker')}
  ,{Sys.sleep(10);
    market.api.process(market = 'btce', currency_pair = c('NMC', 'BTC'), action='ticker')}
  ,market.api.process(market = 'kraken', currency_pair = c('BTC','EUR'), action='ticker')
  ,{Sys.sleep(10);
    market.api.process(market = 'kraken', currency_pair = c('LTC','EUR'), action='ticker')}
  ,{Sys.sleep(10);
    market.api.process(market = 'kraken', currency_pair = c('BTC','LTC'), action='ticker')}
))
print(ticker_all)

# get wallet from market
market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'wallet',
                   key = '', secret = '')
# get wallet from all markets and combine
wallet_all <- rbindlist(list(
  #market.api.process(market = 'mtgox', currency_pair = c('BTC', 'USD'), action = 'wallet',
  #                   key = '', secret = ''),
  market.api.process(market = 'bitstamp', currency_pair = c('BTC', 'USD'), action = 'wallet',
                     client_id = '', key = '', secret = ''),
  market.api.process(market = 'btce', currency_pair = c('LTC', 'USD'), action = 'wallet',
                     method = '', key = '', secret = ''),
  market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'wallet',
                     key = '', secret = '')
))
print(wallet_all)

# get order book from market
market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'order_book')

# get open orders from market
market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'open_orders',
                   key = '', secret = '')

# place limit order
market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'place_limit_order',
                   req = list(type = 'sell', amount = 1, price = 8000), # sell 1 btc for 8000 eur
                   key = '', secret = '')

# cancel order
market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'cancel_order,
                   req = list(oid = 'oid_from_open_orders'),
                   key = '', secret = '')
# get trades
market.api.process(market = 'kraken', currency_pair = c('BTC', 'EUR'), action = 'trades')
}
}
\seealso{
\code{\link{market.api.query}}
}

