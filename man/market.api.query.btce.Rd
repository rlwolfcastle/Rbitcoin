\name{market.api.query.btce}
\alias{market.api.query.btce}
\title{Send request to btce market API}
\usage{
market.api.query.btce(url, key, secret, req = list(), ssl.verify = FALSE,
  curl.verbose = FALSE, debug = 0)
}
\arguments{
  \item{url}{character with url on which query needs to be
  passed.}

  \item{key}{character API key used in private API calls.}

  \item{secret}{character API secret used in private API
  calls.}

  \item{req}{list of object passed to API: price and amount
  of opening order, id of cancelling order, etc. See note.}

  \item{ssl.verify}{logical flag to use HTTP over SSL.}

  \item{curl.verbose}{logical flag to display RCurl debug
  messages.}

  \item{debug}{integer. Rbitcoin debug messages if
  \code{debug > 0}, each subfunction reduce \code{debug} by
  1.}
}
\value{
fromJSON decoded result from market's API call.
}
\description{
Send request to btce market API.
}
\note{
Market specific btce \code{method} param should be provided
in \code{req} object.
}
\examples{
\dontrun{
# ticker
market.api.query.btce(url = 'https://btc-e.com/api/2/btc_usd/ticker')
# wallet
market.api.query.btce(url = 'https://btc-e.com/tapi',
                      req = list(method = 'getInfo'),
                      key = '',
                      secret = '')
}
}
\references{
\url{https://btc-e.com/api/documentation}
}
\seealso{
\code{\link{market.api.query}},
\code{\link{market.api.query.mtgox}},
\code{\link{market.api.query.bitstamp}},
\code{\link{market.api.query.kraken}}
}

