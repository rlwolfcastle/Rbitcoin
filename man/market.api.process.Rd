\name{market.api.process}
\alias{market.api.process}
\title{Process market API}
\usage{
  market.api.process(market, action, req, ..., debug = 0,
    raw.query.res = FALSE,
    on.market.error = expression(stop(e)))
}
\arguments{
  \item{market}{character, example: \code{kraken}}

  \item{action}{character, defined process to get organized
  data}

  \item{req}{list with action details (price, amount, etc.)
  unified across the markets}

  \item{\dots}{objects to be passed to
  \code{\link{market.api.query}} \itemize{ \item auth
  params: \code{key}, \code{secret}, \code{client_id} (used
  on bitstamp), \code{method} (used on btce), \item
  technical params: \code{ssl.verify}, \code{curl.verbose},
  \code{on.error}. }}

  \item{debug}{integer. Rbitcoin debug messages if
  \code{debug > 0}, each subfunction reduce \code{debug} by
  1.}

  \item{raw.query.res}{logical skip post-processing are
  return only after fromJSON processing.}

  \item{on.market.error}{expression to be returned on
  market level error. For R level error check
  \code{market.api.query}.}
}
\value{
  Returned value depends on the \code{action} param:
  \itemize{ \item \code{'ticker'} returns \code{data.table}
  object with common attributes, \item \code{'wallet'}
  returns \code{data.table} object with common attributes,
  \item \code{'order_book'} returns \code{list} with API
  call level attributes and sub elements \code{[['asks']]}
  and \code{[['bids']]} as \code{data.table} objects with
  order book including already calculated cumulative
  \code{amount} and \code{price}. }
}
\description{
  Processing of API call according to API dictionary
  \code{data(api.dict); api.dict}. Pre processing of
  request and post processing of API call results to
  unified structure across markets. It will result
  truncation of most (not common across the markets)
  attributes returned. If you need the full set of data
  returned by market's API you should use
  \code{\link{market.api.query}}.
}
\note{
  The function is currently in development.\cr Available
  actions are \code{'ticker'}, \code{'wallet'},
  \code{'order_book'}.\cr Market level error handling works
  only partially as not all markets returns API call status
  info. For unknown error \code{str(x)} is passed to
  \code{stop()} function.\cr If you find any bugs please
  report.
}
\examples{
\dontrun{
# get ticker from market
market.api.process(market = 'kraken', action = 'ticker', debug = 10)
# get ticker from all markets and combine
ticker_all <- rbind(
  market.api.process(market = 'mtgox', action = 'ticker'),
  market.api.process(market = 'bitstamp', action = 'ticker'),
  market.api.process(market = 'btce', action = 'ticker'),
  market.api.process(market = 'kraken', action = 'ticker')
)
print(ticker_all)

# get wallet from market
market.api.process(market = 'kraken', action = 'wallet', debug = 10)
# get wallet from all markets and combine
wallet_all <- rbind(
  market.api.process(market = 'mtgox', action = 'wallet',
                     key = '', secret = ''),
  market.api.process(market = 'bitstamp', action = 'wallet',
                     client_id = '', key = '', secret = ''),
  market.api.process(market = 'btce', action = 'wallet',
                     method = '', key = '', secret = ''),
  market.api.process(market = 'kraken', action = 'wallet',
                     key = '', secret = '')
)
print(wallet_all)

# get order book from market
market.api.process(market = 'kraken', action = 'order_book', debug = 10)
# get order book for all markets and combine
order_book_all <- list(
  'mtgox' = market.api.process(market = 'mtgox', action = 'order_book'),
  'bitstamp' = market.api.process(market = 'bitstamp', action = 'order_book'),
  'btce' = market.api.process(market = 'btce', action = 'order_book'),
  'kraken' = market.api.process(market = 'kraken', action = 'order_book')
)
print(order_book_all[['mtgox']][['bids']])
str(order_book_all)
}
}
\seealso{
  \code{\link{market.api.query}}
}

