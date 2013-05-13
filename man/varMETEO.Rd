\name{varMETEO}
\alias{varMETEO}
\docType{data}
\title{
Table of Weather Variables (monthly, daily)
}
\description{
This dataset is used for finding the weather variables. You can find the names in the first column. The second column (Code) is not important for the user. 
}
\usage{data(varMETEO)}
\format{
  A data frame with 39 observations on the following 2 variables.
  \describe{
    \item{\code{Var}}{a character vector}
    \item{\code{Code}}{a character vector}
  }
}
\source{
\url{http://www.meteogalicia.com}
}
\examples{
data(varMETEO)
str(varMETEO)
}

