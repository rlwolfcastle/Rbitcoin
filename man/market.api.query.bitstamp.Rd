\name{market.api.query.bitstamp}
\alias{market.api.query.bitstamp}
\title{Send request to bitstamp market API}
\usage{
  market.api.query.bitstamp(url, client_id, key, secret,
    req, ssl.verify = FALSE, curl.verbose = FALSE,
    debug = 0)
}
\arguments{
  \item{url}{character with url on which query needs to be
  passed.}

  \item{client_id}{character. Bitstamp market specific
  parameter used in private API call authorization (check
  reference for more information).}

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
  Send request to bitstamp market API. Pre-processing of
  req and post-processing of res are not performed here.
}
\examples{
\dontrun{
# ticker
market.api.query.bitstamp(url = 'https://www.bitstamp.net/api/ticker/')
# wallet
market.api.query.bitstamp(url = 'https://www.bitstamp.net/api/balance/',
                          client_id = '',
                          key = '',
                          secret = '')
}
}
\references{
  \url{https://www.bitstamp.net/api/}
}
\seealso{
  \code{\link{market.api.query}}
}

