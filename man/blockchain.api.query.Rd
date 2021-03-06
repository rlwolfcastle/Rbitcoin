\name{blockchain.api.query}
\alias{blockchain.api.query}
\title{Query blockchain.info API}
\usage{
blockchain.api.query(..., method, verbose = getOption("Rbitcoin.verbose"))
}
\arguments{
  \item{\dots}{params passed to blockchain.info API,
  specific for particular method, example
  \code{'bitcoin_address'} or \code{'tx_index'}, for more
  see references or examples.}

  \item{method}{character. For details see references,
  currently supported \code{'Single Address'} and
  \code{'Single Transaction'}. If \code{method} missing the
  function will try to guess it based on first param in
  \dots.}

  \item{verbose}{integer. Rbitcoin processing messages,
  print to console if \code{verbose > 0}, each subfunction
  reduce \code{verbose} by 1. If missing then
  \code{getOption("Rbitcoin.verbose")} is used, by default
  \code{0}.}
}
\description{
Query bitcoin related data from blockchain.info.
}
\examples{
\dontrun{
# query bitcoin address information - 'Single Address' method
# Rbitcoin donation address final balance in BTC
blockchain.api.query('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi',limit=0)[['final_balance']]/100000000
# Rbitcoin donation address full details
blockchain.api.query('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi',verbose=1)
# some first wallet final balance in BTC
blockchain.api.query('1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa',limit=0)[['final_balance']]/100000000
# some first wallet details (limit to 3 txs, skip two txs)
blockchain.api.query(method = 'Single Address',
                     bitcoin_address = '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa', limit=3, offset=2)
# query bitcoin transaction information - 'Single Transaction' method
# Some recent transaction of some first wallet
blockchain.api.query('e5c4de1c70cb6d60db53410e871e9cab6a0ba75404360bf4cda1b993e58d45f8')
}
}
\references{
\url{https://blockchain.info/api/blockchain_api}
}
\seealso{
\code{\link{market.api.query}}
}

