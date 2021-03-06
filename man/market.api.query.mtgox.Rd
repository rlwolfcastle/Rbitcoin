\name{market.api.query.mtgox}
\alias{market.api.query.mtgox}
\title{Send request to mtgox market API}
\usage{
market.api.query.mtgox(url, key, secret, req = list(),
  verbose = getOption("Rbitcoin.verbose"))
}
\arguments{
  \item{url}{character with url on which query needs to be
  passed.}

  \item{key}{character API key used in private API calls.}

  \item{secret}{character API secret used in private API
  calls.}

  \item{req}{list of object passed to API: price and amount
  of opening order, id of cancelling order, etc.}

  \item{verbose}{integer. Rbitcoin processing messages,
  print to console if \code{verbose > 0}, each subfunction
  reduce \code{verbose} by 1. If missing then
  \code{getOption("Rbitcoin.verbose")} is used, by default
  \code{0}.}
}
\value{
R object created by fromJSON decoded result from market's
API call.
}
\description{
Send request to mtgox market API. MtGox is already closed
but public API calls are working. Also it's code/dictionary
can be reused in future.
}
\examples{
\dontrun{
# ticker
market.api.query.mtgox(url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker_fast')
# wallet
market.api.query.mtgox(url = 'https://data.mtgox.com/api/2/BTCUSD/money/info',
                       key = '', secret = '')
}
}
\references{
\url{https://bitbucket.org/nitrous/mtgox-api}
}
\seealso{
\code{\link{market.api.query}},
\code{\link{market.api.query.bitstamp}},
\code{\link{market.api.query.btce}},
\code{\link{market.api.query.kraken}}
}

