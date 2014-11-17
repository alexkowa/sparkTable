\name{export-methods}
\docType{methods}
\alias{export}
\alias{export-methods}
\alias{export,sparkbar-method}
\alias{export,sparkbox-method}
\alias{export,sparkline-method}
\alias{export,sparkhist-method}
\alias{export,sparkTable-method}
\alias{export,geoTable-method}

\title{Saves objects of class 'sparkline', 'sparkbar', 'sparkhist' or 'sparkbox' to a file}
\description{
  Spark-Objects are plotted and saved into different file-formats while for objects of class \code{\linkS4class{sparkTable}} all required graphs are stored and the necessary code to include the graphical table in the desired format is returned to the promt.
}
\usage{
  export(object, ...)
  \S4method{export}{sparkline}(object, outputType="pdf", filename="sparkLine", ...)
  \S4method{export}{sparkbar}(object, outputType="pdf", filename="sparkBar", ...)
  \S4method{export}{sparkhist}(object, outputType="pdf", filename="sparkHist", ...)
  \S4method{export}{sparkbox}(object, outputType="pdf", filename="sparkBox", ...)
  \S4method{export}{sparkTable}(object, outputType="html", filename=NULL, graphNames="out",infonote=TRUE, scaleByCol=FALSE,...)
  \S4method{export}{geoTable}(object, outputType="html", filename=NULL, graphNames="out", transpose=FALSE, include.rownames=FALSE,include.colnames=FALSE,rownames=NULL,colnames=NULL,...)
}
\arguments{
  \item{object}{an object of class 'sparkline', 'sparkbox' or 'sparkbar'.}
  \item{outputType}{for objects of class \code{\linkS4class{sparkline}}, \code{\linkS4class{sparkbar}} or \code{\linkS4class{sparkbox}} a character vector specifying the desired output formats. Supported formats are:
    \itemize{
      \item 'pdf': a pdf image is produced
      \item 'eps': an eps image is procuded
      \item 'png': a png image is procuded
    }
    For objects of class \code{\linkS4class{sparkTable}} a character vector specifying the desired output format, supported types are:
    \itemize{
      \item "tex": latex output is produced
      \item "html": html output is generated
    }
  }
  \item{filename}{the filename of the output (minus '.pdf', 'eps','.eps' for single graphs, minus '.tex' or '.html' for graphical tables}
  \item{graphNames}{the main part of the single graphic files that are produced (minus '-someIndex.extension'}
  \item{infonote}{TRUE/FALSE if the latex command should be included in the output, only used for graphical tables (\code{\linkS4class{sparkTable}})}
  \item{scaleByCol}{Either TRUE/FALSE to set the scaling for all columns or a TRUE/FALSE vector of the same length as the content object. Keeps the scaling the same in all rows of a column. This parameter is used only for graphical tables (\code{\linkS4class{sparkTable}})}
  \item{transpose}{logical vector of length 1 defining if the plot be transposed (for \code{\linkS4class{geoTable}}-objects)}
  \item{include.rownames}{logical vector of length 1 defining if rownames should be included (for \code{\linkS4class{geoTable}}-objects)}
  \item{include.colnames}{logical vector of length 1 defining if colnames should be included (for \code{\linkS4class{geoTable}}-objects)}
  \item{rownames}{optional chracter vector specifying row names (for \code{\linkS4class{geoTable}}-objects)}
  \item{colnames}{optional chracter vector specifying column names (for \code{\linkS4class{geoTable}}-objects)}
  \item{...}{additional parameters to be passed, currently not used}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\examples{
\dontrun{
data(pop)
x <- pop[pop[,2]=="Insgesamt",3]
a <- newSparkLine(values=x, pointWidth=8)
export(a, outputType=c('pdf','png'), filename='myFirstSparkLine')

# simple graphical table
data(pop,package="sparkTable")
b <- newSparkBox()
l <- newSparkLine()
bb <- newSparkBar()
content <- list(function(x) { round(mean(x),2) },
  b,l,bb,function(x) { round(tail(x,1),2)
})
names(content) <- paste("column",1:length(content),sep="")
varType <- rep("value",length(content))
pop <- pop[,c("variable","value","time")]
pop$time <- as.numeric(as.character(pop$time))
xx <- reshapeExt(pop,idvar="variable", varying=list(2))
x1 <- newSparkTable(xx, content, varType)
export(x1, outputType="html", graphNames="o2",filename="t1")
export(x1, outputType="tex", graphNames="o3",filename="t2")

##Geo-Table: EU population and debt
data(popEU,package="sparkTable")
data(debtEU,package="sparkTable")
data(coordsEU,package="sparkTable")
popEU <- popEU[popEU$country\%in\%coordsEU$country,]
debtEU <- debtEU[debtEU$country\%in\%coordsEU$country,]
EU <- cbind(popEU,debtEU[,-1])
EUlong <- reshapeExt(EU,
  idvar="country",v.names=c("pop","debt"),
  varying=list(2:13,14:25),geographicVar="country",timeValues=1999:2010
)
l <- newSparkLine()
l <- setParameter(l, 'lineWidth', 2.5)
content <- list(
  function(x){"Population:"},
  l,function(x){"Debt:"},l)
varType <- c(rep("pop",2),rep("debt",2))
xGeoEU <- newGeoTable(EUlong, content, varType,geographicVar="country",
  geographicInfo=coordsEU)
export(xGeoEU, outputType="html", graphNames="outEU",
  filename="testEUT",transpose=TRUE)
export(xGeoEU, outputType="html", graphNames="outEU1",
  filename="testEU", transpose=FALSE)
 export(xGeoEU, outputType="tex", graphNames="out1",
  filename="testEU",transpose=FALSE)
#export(xGeoEU, outputType="tex", graphNames="out1",
  filename="testEUT",transpose=TRUE)
}
}
