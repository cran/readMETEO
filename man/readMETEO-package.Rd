\name{readMETEO-package}
\alias{readMETEO-package}
\alias{readMETEO}
\docType{package}
\title{
Download Weather Data from WWW.METEOGALICIA.COM
}
\description{
This package allows to download weather data from www.meteogalicia.com (10-minutes, daily, monthly) from all stations, automatically.
}
\details{
\tabular{ll}{
Package: \tab readMETEO\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-05-13\cr
License: \tab GPL-2\cr
}
readMeteo - Download weather data\cr

searMETEO - Search for variable table of the weather station

}
\author{
Dominic Roye

Maintainer: Dominic Roye <dominic.roye@gmail.com>

}
\references{
\url{http://www.meteogalicia.com}
}

\keyword{ readMeteo }
\seealso{
\code{\link[XML]{readHTMLTable}}, \code{\link{searMETEO}}, \code{\link{estaciones}}, \code{\link{varMETEO}},  \code{\link{varMETEO10}}, \code{\link{readMeteo}}
}
\examples{
data(estaciones)
data(varMETEO)
data(varMETEO10)

#Example 1 (10-minutes):

readMeteo("diezminutal","Santiago",c("Temperatura media de aire"),"01/01/2012","10/01/2012")

#Example 2 (daily):

readMeteo("diario","Santiago",c("Temperatura media de aire","Lluvia"),"01/01/2012","01/01/2013")

#Example 3 (monthly):

readMeteo("mensual","Santiago",c("Temperatura media de aire","Lluvia"),"01/01/2012","01/01/2013")

#Example 4:

searMETEO("Santiago")

}
