\docType{data}
\name{api.dict}
\alias{api.dict}
\title{API dictionary}
\description{
  This data set contains a logic of market specific pre
  processing and post processing of API calls.
  \code{data(api.dict); api.dict}. If adding new market
  most of the work need to be performed here on the
  definition of data manipulation.
}
\note{
  Many of the errors which will not be handled by standard
  catching (markets may not return 'result' field etc.)
  will produce \code{stop(str(x))} to debug shape of
  returned data. Such errors will be subject to change in
  future development.
}
\author{
  Jan Gorecki, 2013-11-22
}
\keyword{datasets}

