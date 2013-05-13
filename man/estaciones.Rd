\name{estaciones}
\alias{estaciones}
\docType{data}
\title{
Summary of Weather Stations
}
\description{
This dataset is used for finding the Weather Station. You can find the names of every station in the first column. The other columns (Station Code and Region Code) are not important for the user. 
}
\usage{data(estaciones)}
\format{
  A data frame with 101 observations on the following 3 variables.
  \describe{
    \item{\code{Nombre}}{a character vector}
    \item{\code{Code}}{a numeric vector}
    \item{\code{Prov}}{a numeric vector}
  }
}
\source{
\url{http://www.meteogalicia.com}
}
\examples{
data(estaciones)
str(estaciones)
}
\keyword{datasets, weather stations, meteogalicia, galicia}
