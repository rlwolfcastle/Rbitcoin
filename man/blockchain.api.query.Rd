\name{blockchain.api.query}
\alias{blockchain.api.query}
\title{Query blockchain.info API}
\usage{
  blockchain.api.query(method = "addressbalance", address,
    ssl.verify = FALSE, curl.verbose = FALSE, debug = 0)
}
\arguments{
  \item{method}{character. Define what kind of data to
  query from blockchain (currently only
  \code{'addressbalance'} supported).}

  \item{address}{character. Bitcoin address passed to
  query.}

  \item{ssl.verify}{logical flag to use HTTP over SSL, if
  missing certificate file it will be downloaded.}

  \item{curl.verbose}{logical flag to display RCurl debug
  messages.}

  \item{debug}{integer. Rbitcoin debug messages if
  \code{debug > 0}, each subfunction reduce \code{debug} by
  1.}
}
\description{
  Query bitcoin related data from blockchain.info.
}
\examples{
\dontrun{
# get address balance
blockchain.api.query(method = 'addressbalance', address = '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa')
}
}
\references{
  \url{https://blockchain.info/q}
}
\seealso{
  \code{\link{market.api.query}}
}

