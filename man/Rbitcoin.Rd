\docType{package}
\name{Rbitcoin}
\alias{Rbitcoin}
\alias{Rbitcoin-package}
\alias{bitcoin}
\alias{btc}
\title{R & bitcoin integration}
\description{
Utilities related to Bitcoin. Core functionalities are:
\itemize{ \item \code{market.api.query} - launch query on
market's API (\code{mtgox}, \code{bitstamp}, \code{btce},
\code{kraken}). Both public and private API calls
supported. \item \code{market.api.process} - integration of
market's processing structures: pre-process of API request,
post-process API result, market error catching. Input and
output unified structure. Requires API dictionary
definition, package built-in \code{\link{api.dict}}
contains 6 currency pairs on 4 markets. \item
\code{blockchain.api.query} - launch query on
blockchain.info API json interface. \item support HTTP over
SSL. \item debug messages of \code{Rbitcoin}, debug
messages of \code{RCurl}. } You need to note that
\code{digest} package docs states: \emph{Please note that
this package is not meant to be deployed for cryptographic
purposes for which more comprehensive (and widely tested)
libraries such as OpenSSL should be used}.\cr To do not get
banned by market's API anti-DDoS protection user should
handle the antispam process, example:
\code{Sys.sleep(10)}.\cr It is advised to maintain your API
keys security level as tight as possible, if you do not
need withdraw api method be sure to disable it for api
keys.\cr At the time of writing the most recent market's
API version were used: \itemize{ \item mtgox v2 \item
bitstamp v2 (public) / ? (private) \item btce v2 (public) /
"tapi" (private) \item kraken v0 }

BTC donation:
\url{bitcoin:15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi}
}
\references{
Package discussion thread:
\url{https://bitcointalk.org/index.php?topic=343504}
}
\seealso{
\code{\link{market.api.query}},
\code{\link{blockchain.api.query}},
\code{\link{market.api.process}}, \code{\link{api.dict}}
}

