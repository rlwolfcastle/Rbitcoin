\docType{data}
\name{api.dict}
\alias{api.dict}
\title{API dictionary}
\usage{
data(api.dict)
}
\description{
This data set contains dictionary (data.table object) for
market specific url, internal market's method name, logic
of pre-process, post-process, markets error catch. Run
\code{data(api.dict); api.dict} to print built-in
dictionary. Granularity of data is \code{c(market, base,
quote, action)}. This dictionary can be edited/extended by
user for new currency pairs.\cr Currently supported
currency pairs: \itemize{ \item \code{mtgox: BTCUSD} \item
\code{bitstamp: BTCUSD} \item \code{btce: LTCUSD, LTCBTC}
\item \code{kraken: BTCEUR, LTCEUR} }
}
\note{
Do not use \code{api.dict} from untrusted source or at
least read whole it's code to ensure it is safe! The api
dictionary was not fully tested, please follow the
examples, if you find any bugs please report.
}
\author{
Jan Gorecki, 2013-12-31
}
\keyword{datasets}

