\name{readMeteo}
\alias{readMeteo}
\title{
Download Weather Data from www.meteogalicia.com
}
\description{
You can download weather data from all stations of MeteoGalicia.
}
\usage{
readMeteo(data, est, var, date1, date2)
}
\arguments{
  \item{data}{
Types of data: diezminutal (10-minutes),diario (daily) or mensual (monthly)
}
  \item{est}{
Name of weather station. You can find the names in the dataset data(estaciones). It is not possible to download more than one station at the same time. For several stations use a for-loop. 
}
  \item{var}{
Name(s) of weather variable(s). For daily and monthly data, you can find the names in the dataset data(varMETEO) and for 10-minutes data in the dataset data(varMETEOdia). For search the weather station in varMETEO10 use the function \code{\link{searMETEO}}. 
}
  \item{date1}{
Start Date. DATE FORMAT is "day/month/year"(01/01/2005)
}
  \item{date2}{
End Date. DATE FORMAT is "day/month/year"(01/12/2005)
}
}
\value{
You get a Data.Frame. 
}
\author{
Dominic Roye (dominic.roye@gmail.com)
}
\note{
When using 10-minutes type the download could may take a few minutes or more.
}
\seealso{
\code{\link{searMETEO}}, \code{\link{estaciones}}, \code{\link{varMETEO}},  \code{\link{varMETEO10}}
}
\examples{

#Example 1 (10-minutes):

readMeteo("diezminutal","Santiago",c("Temperatura media de aire"),"01/01/2012","10/01/2012")

#Example 2 (daily):

readMeteo("diario","Santiago",c("Temperatura media de aire","Lluvia"),"01/01/2012","01/01/2013")

#Example 3 (monthly):

readMeteo("mensual","Santiago",c("Temperatura media de aire","Lluvia"),"01/01/2012","01/01/2013")

#Example 3 (monthly, weather stations > 1):

est <- c("Santiago","CampusLugo")
data <- NULL
for(i in 1:2){

data[[i]] <- readMeteo("mensual",est[i],"Temperatura media de aire","01/01/2012","01/01/2013")

}

}
\keyword{ weather }
\keyword{ Galicia }
\keyword{ Spain }
\keyword{ data }
\keyword{ download }
\keyword{ read }
\keyword{ meteogalicia }
\keyword{ meteo }
