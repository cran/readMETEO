\name{varMETEO10}
\alias{varMETEO10}
\docType{data}
\title{
Table of Weather Variables (10-minutes)
}
\description{
This dataset is used for finding the weather variables of each weather station. You can find for each weather station the names in the first column. The second column (Code) is not important for the user.
}
\usage{data(varMETEO10)}
\format{
  The format is:
List of 101
 $ Santiago         :'data.frame':	39 obs. of  2 variables:
  ..$ Var : chr [1:39] "Temperatura media de aire" "Humedad relativa media" "Temperatura de rocio" "Temperatura de suelo 10cm" ...
  ..$ Code: num [1:39] 1310 1311 1489 1324 1325 ...
  .
  .
 }
\details{
Use \code{\link{searMETEO}} for search the station in the table list. 
}
\source{
\url{http://www.meteogalicia.com}
}
\examples{
data(varMETEO10)

}
