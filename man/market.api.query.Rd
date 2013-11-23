\name{market.api.query}
\alias{market.api.query}
\title{Send request to market API}
\usage{
  market.api.query(market, ..., ssl.verify = FALSE,
    curl.verbose = FALSE, debug = 0,
    on.error = expression(stop(e)))
}
\arguments{
  \item{market}{character which identifies market on which
  we want to send request: mtgox, bitstamp, btce, kraken.}

  \item{\dots}{objects to be passed to API: \code{url},
  \code{key}, \code{secret}, \code{req}, \code{client_id}
  (used on bitstamp), \code{method} (used on btce).}

  \item{ssl.verify}{logical flag to use HTTP over SSL, if
  missing certificate file it will be downloaded.}

  \item{curl.verbose}{logical flag to display RCurl debug
  messages.}

  \item{debug}{integer. Rbitcoin debug messages if
  \code{debug > 0}, each subfunction reduce \code{debug} by
  1.}

  \item{on.error}{expression to be returned on R level
  error of market specific function for
  \code{market.api.query}. It does not catch internal
  market's error returned as valid object.}
}
\value{
  R object created by fromJSON decoded result from market's
  API call.
}
\description{
  Route a request to particular market function. Also
  checks if ssl.verify and missing certificate file then
  downloads it.
}
\details{
  To do not spam market's API, use \code{Sys.sleep(10)}
  between API calls.
}
\note{
  It is advised to use this function instead of calling
  market's function directly. If calling directly one
  should ensure to send any numeric values in
  non-exponential notation: \code{options(scipen=100)}.
}
\examples{
\dontrun{
# ticker
market.api.query(market = 'mtgox',
                 url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker')
market.api.query(market = 'bitstamp',
                 url = 'https://www.bitstamp.net/api/ticker/')
market.api.query(market = 'btce',
                 url = 'https://btc-e.com/api/2/btc_usd/ticker')
market.api.query(market = 'kraken',
                 url = 'https://api.kraken.com/0/public/Ticker?pair=XXBTZEUR')
# wallet
market.api.query(market = 'mtgox',
                 url = 'https://data.mtgox.com/api/2/BTCUSD/money/info',
                 key = '',
                 secret = '')
market.api.query(market = 'bitstamp',
                 url = 'https://www.bitstamp.net/api/balance/',
                 client_id = '',
                 key = '',
                 secret = '')
market.api.query(market = 'btce',
                 url = 'https://btc-e.com/tapi',
                 method = 'getInfo',
                 key = '',
                 secret = '')
market.api.query(market = 'kraken',
                 url = 'https://api.kraken.com/0/private/Balance',
                 key = '',
                 secret = '')
# order book
market.api.query(market = 'mtgox',
                 url = 'https://data.mtgox.com/api/2/BTCUSD/money/depth/fetch')
# place order
market.api.query(market = 'mtgox',
                 url = 'https://data.mtgox.com/api/2/BTCUSD/money/order/add',
                 key = '',
                 secret = '',
                 req = list(type = 'ask', #sell
                            amount_int = trunc(0.1 * 100000000), #0.1 btc
                            price_int = trunc(1000 * 100000))) #1000 usd
# open orders
market.api.query(market = 'mtgox',
                 url = 'https://data.mtgox.com/api/2/BTCUSD/money/orders',
                 key = '',
                 secret = '')
# debugging
market.api.query(market = 'mtgox',
                 url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker_fast',
                 curl.verbose = FALSE,
                 debug = 10)
market.api.query(market = 'mtgox',
                 url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker_fast',
                 ssl.verify = FALSE,
                 curl.verbose = TRUE,
                 debug = 10)
market.api.query(market = 'mtgox',
                 url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker_fast',
                 ssl.verify = TRUE,
                 curl.verbose = TRUE,
                 debug = 10)
}
}
\references{
  API documentation:
  \url{https://bitbucket.org/nitrous/mtgox-api}
  \url{https://www.bitstamp.net/api/}
  \url{https://btc-e.com/api/documentation}
  \url{https://www.kraken.com/help/api}
}
\seealso{
  \code{\link{market.api.process}},
  \code{\link{market.api.query.mtgox}},
  \code{\link{market.api.query.bitstamp}},
  \code{\link{market.api.query.btce}},
  \code{\link{market.api.query.kraken}}
}

