\docType{package}
\name{Rbitcoin}
\alias{\url{https://bitcointalk.org/index.php?topic=343504}}
\alias{bitcoin}
\alias{btc}
\alias{Rbitcoin}
\alias{Rbitcoin-package}
\title{Rbitcoin integration}
\description{
  Utilities related to Bitcoin. Core functionalities are:
  \itemize{ \item \code{market.api.query} - launch API
  query on market's API (\code{mtgox}, \code{bitstamp},
  \code{btce}, \code{kraken}). Both public and private API
  calls supported. \item \code{market.api.process} - launch
  API query after pre-process of API request (if required)
  and post-process API result. Produce unified data
  structure. \item \code{blockchain.api.query} - launch API
  query on blockchain.info interface. \item support HTTP
  over SSL. \item debug messages of \code{Rbitcoin}, debug
  messages of \code{RCurl}. } To do not get banned by
  market's API user should handle the antispam process by
  \code{Sys.sleep(10)}.\cr At the time of writing the most
  recent market's API version were used: \itemize{ \item
  mtgox v2 \item bitstamp v2 (public) / ? (private) \item
  btce v2 (public) / "tapi" (private) \item kraken v0 }

  BTC donation:
  \url{bitcoin:15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi}
}
\references{
  Bitcointalk official Rbitcoin thread:
  \url{https://bitcointalk.org/index.php?topic=343504}
}
\seealso{
  \code{\link{market.api.query}},
  \code{\link{blockchain.api.query}},
  \code{\link{market.api.process}}
}

