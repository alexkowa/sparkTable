\name{export-methods}
\docType{methods}
\alias{export}
\alias{export-methods}
\alias{export,sparkbar-method}
\alias{export,sparkbox-method}
\alias{export,sparkline-method}
\alias{export,sparkhist-method}
\title{Saves objects of class 'sparkline', 'sparkbar', 'sparkhist' or 'sparkbox' to a file}
\description{
  Spark-Objects are plotted and saved into different file-formats.
}
\usage{
  export(object, outputType='pdf', filename='testSpark', ...)
}
\arguments{
  \item{object}{an object of class 'sparkline', 'sparkbox' or 'sparkbar'.}
  \item{outputType}{a character vector specifying the desired output formats. Supported formats are:
    \itemize{
      \item 'pdf': a pdf image is produced
      \item 'eps': an eps image is procuded
      \item 'png': a png image is procuded
    }
  }
  \item{filename}{the filename of the output (minus '.pdf', 'eps' or '.eps'}
  \item{...}{additional parameters to be passed, currently not used}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\examples{
  data(pop)
  x <- pop[pop[,2]=="Insgesamt",3]
  a <- newSparkLine(values=x, pointWidth=8)
  export(a, outputType=c('pdf','png'), filename='myFirstSparkLine')
}
