\docType{package}
\name{Rbitcoin}
\alias{Default}
\alias{Rbitcoin}
\alias{Rbitcoin-package}
\alias{SSL}
\alias{\code{"RCurlOptions"=list("ssl.verifypeer","ssl.verifyhost","cainfo","verbose")}.}
\alias{\code{"Rbitcoin.verbose"}}
\alias{also}
\alias{and}
\alias{are:}
\alias{be}
\alias{bitcoin}
\alias{btc}
\alias{can}
\alias{console}
\alias{debug}
\alias{here}
\alias{in}
\alias{messages}
\alias{off,}
\alias{package}
\alias{print}
\alias{support}
\alias{the}
\alias{to}
\alias{turned}
\alias{used}
\title{R & bitcoin integration}
\description{
Utilities related to Bitcoin. Core functionalities are:
\itemize{ \item \code{market.api.query} - launch query on
market's API (\code{mtgox}, \code{bitstamp}, \code{btce},
\code{kraken}). Both public and private API calls
supported. All currency pairs supported. \item
\code{market.api.process} - integration of market's
processing structures: pre-process of API request,
post-process API results, market error catching. Input and
output unified structure. Requires API dictionary
definition, for details of package built-in dictionary see
\code{\link{api.dict}}. \item \code{blockchain.api.query} -
launch query on blockchain.info API json interface. } You
need to note that \code{digest} package docs states:
\emph{Please note that this package is not meant to be
deployed for cryptographic purposes for which more
comprehensive (and widely tested) libraries such as OpenSSL
should be used}.\cr To do not get banned by market's API
anti-DDoS protection user should handle the antispam
process, example: \code{Sys.sleep(10)} between the API
calls.\cr It is advised to maintain your API keys security
level as tight as possible, if you do not need withdraw api
method be sure to disable it for api keys.\cr You can print
debug messages of \code{Rbitcoin} to console using verbose
argument in FUNs or \code{options("Rbitcoin.verbose" =
1)}.\cr Two params \code{ssl.verify} and
\code{curl.verbose} have been deprecated since
\code{0.8.5}. They can and should be controlled using
\code{options("RCurlOptions")}. SSL verify is by default
active.\cr At the time of writing the most recent market's
API version were used: \itemize{ \item mtgox v2 (market is
already dead but public API calls still returns the values)
\item bitstamp v2 (public) / ? (private) \item btce v2
(public) / "tapi" (private) \item kraken v0 } To set SSL
and others package-level options see examples.

BTC donation:
\url{bitcoin:15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi}
}
\examples{
\dontrun{
# default options used by Rbitcoin
# print Rbitcoin processing to console set "Rbitcoin.verbose" to 1 (or more)
# print Rcurl processing to console set RCurlOptions[["verbose"]] to TRUE
# to disable SSL set ssl.verify* = FALSE and cainfo = NULL
options(Rbitcoin.verbose=0)
options(RCurlOptions=list(ssl.verifypeer = TRUE,
                          ssl.verifyhost = TRUE,
                          cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
                          verbose = FALSE))
}
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

