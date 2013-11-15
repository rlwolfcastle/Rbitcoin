\docType{package}
\name{Rbitcoin}
\alias{bitcoin,}
\alias{btc}
\alias{Rbitcoin}
\alias{Rbitcoin-package}
\title{Rbitcoin}
\description{
  Utilities related to Bitcoin: markets API interface
  (mtgox, bitstamp, btce, kraken). Both public and private
  API calls: ticker, recent trades, order book, place order
  and more. Supports HTTP over SSL. Debug messages of
  Rbitcoin, debug messages of RCurl. To do not get banned
  by market's API one should handle the antispam process by
  \code{Sys.sleep(10)}.

  Future version will be extended for API \code{request}
  pre-processing and API \code{result} post-processing
  which will unify the structure of all in/out object for
  all markets.

  Official bitcointalk thread soon.

  Please find Index link below to list all Rbitcoin object.
}
\seealso{
  \code{\link{market.api.query}},
  \code{\link{blockchain.api.query}}
}

