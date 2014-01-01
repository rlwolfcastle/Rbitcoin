\name{market.api.query.mtgox}
\alias{market.api.query.mtgox}
\title{Send request to mtgox market API}
\usage{
market.api.query.mtgox(url, key, secret, req = list(), ssl.verify = FALSE,
  curl.verbose = FALSE, debug = 0)
}
\arguments{
  \item{url}{character with url on which query needs to be
  passed.}

  \item{key}{character API key used in private API calls.}

  \item{secret}{character API secret used in private API
  calls.}

  \item{req}{list of object passed to API: price and amount
  of opening order, id of cancelling order, etc.}

  \item{ssl.verify}{logical flag to use HTTP over SSL.}

  \item{curl.verbose}{logical flag to display RCurl debug
  messages.}

  \item{debug}{integer. Rbitcoin debug messages if
  \code{debug > 0}, each subfunction reduce \code{debug} by
  1.}
}
\value{
R object created by fromJSON decoded result from market's
API call.
}
\description{
Send request to mtgox market API.
}
\examples{
\dontrun{
# ticker
market.api.query.mtgox(url = 'https://data.mtgox.com/api/2/BTCUSD/money/ticker_fast')
# wallet
market.api.query.mtgox(url = 'https://data.mtgox.com/api/2/BTCUSD/money/info',
                       key = '',
                       secret = '')
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

