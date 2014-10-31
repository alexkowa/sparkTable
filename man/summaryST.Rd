\name{summaryST}
\alias{summaryST}
\alias{summaryST-methods}
\alias{summaryST,data.frame-method}
\title{
summaryST - data frame in a graphical table
}
\description{
summary for a data frame in a graphical table
}
\usage{
summaryST(.Object, outputType="html", filename=NULL, graphNames="out",
        hist=TRUE,boxplot=TRUE,min=TRUE,quantile=TRUE,median=TRUE,
        mean=TRUE,max=TRUE,changeOrder=NULL,addFun=NULL,digits=2,
        scaleHistByCol=FALSE,scaleBoxByCol=FALSE)
}
\arguments{
  \item{.Object}{a data frame}
\item{outputType}{a character vector of length one specifying the desired output format:
    \itemize{
      \item 'tex': latex output is produced
      \item 'html': html output is procuded
    }
  }
  \item{filename}{ the filename of the output (minus '.tex' or '.html'}
  \item{graphNames}{ the main part of the single graphic files that are produced (minus '-someIndex.extension'}
  \item{hist}{TRUE/FALSE for a histogramm}
  \item{boxplot}{TRUE/FALSE for a boxplot}
  \item{min}{TRUE/FALSE for the minimum}
  \item{quantile}{TRUE/FALSE for 1st and 3rd Quartile}
  \item{median}{TRUE/FALSE for the median}
  \item{mean}{TRUE/FALSE for the mean}
  \item{max}{TRUE/FALSE for the maximum}
  \item{changeOrder}{Indices for reordering the columns of the table}
  \item{addFun}{named list of additional functions e.g. var}
  \item{digits}{number of digits used for rounding}
  \item{scaleHistByCol}{TRUE/FALSE if the histograms of all variables should be on the same x-axis scale}
  \item{scaleBoxByCol}{TRUE/FALSE if the boxplots of all variables should be on the same x-axis scale}


}
\value{
object of class 'sparkTable' for further customizing the output (with setParameter)
}
\author{Alexander Kowarik, Statistics Austria}


\seealso{\code{\link{export}}}
\examples{
data1 <- data.frame(x=rnorm(100),y=rlnorm(100),
  z=rbeta(100,1,1))
#default summary table
summaryST(data1,filename="st1a",graphNames="out1a")
#changing the order of the columns
summaryST(data1,filename="st1b",changeOrder=c(6,7,2,3,1,4,5,8),graphNames="out1b")
#adding a custom column
summaryST(data1,filename="st1c",addFun=list(var=function(x)round(var(x,na.rm=TRUE),2)),
  graphNames="out1c")

data2 <- data.frame(x1=rnorm(100),x2=rnorm(100)+1,x3=rnorm(100)+5)
summaryST(data2,filename="st1d",graphNames="out1d",scaleHistByCol=TRUE,scaleBoxByCol=TRUE)
# the same results in a shiny app:
\dontrun{
showSparkTable(data2)
}
}
