\name{market.api.query.btce}
\alias{market.api.query.btce}
\title{Send request to btce market API}
\usage{
  market.api.query.btce(url, method, key, secret, req,
    ssl.verify = FALSE, curl.verbose = FALSE, debug = 0)
}
\arguments{
  \item{url}{character with url on which query needs to be
  passed.}

  \item{method}{character. Btce market specific parameter
  used in private API call authorization (check reference
  for more information).}

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
  fromJSON decoded result from market's API call.
}
\description{
  Send request to btce market API. Pre-processing of req
  and post-processing of res are not performed here.
}
\examples{
\dontrun{
# ticker
market.api.query.btce(url = 'https://btc-e.com/api/2/btc_usd/ticker')
# wallet
market.api.query.btce(url = 'https://btc-e.com/tapi',
                      method = 'getInfo',
                      key = '',
                      secret = '')
}
}
\references{
  \url{https://btc-e.com/api/documentation}
}
\seealso{
  \code{\link{market.api.query}}
}

