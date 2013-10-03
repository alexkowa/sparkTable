\name{plotSparks-methods}
\docType{methods}
\alias{plotSparks}
\alias{plotSparks-methods}
\alias{plotSparks,sparkbar-method}
\alias{plotSparks,sparkbox-method}
\alias{plotSparks,sparkline-method}
\alias{plotSparks,sparkhist-method}
\title{Plot objects of class 'sparkline', 'sparkbar', 'sparkhist' or 'sparkbox'}
\description{
  Function that calls plot-methods for objects of class 'sparkline', 'sparkbar', 'sparkhist' or 'sparkbox'.
}
\usage{
	plotSparks(object, outputType='pdf', filename='testSpark', ...) 
}
\arguments{
  \item{object}{an object of class 'sparkline', 'sparkbox' or 'sparkbar'.}
  \item{outputType}{a character vector of length one specifying the desired output format:
	  \itemize{
	  	\item 'pdf': a pdf image is produced
	  	\item 'eps': an eps image is procuded
	  	\item 'png': a png image is procuded
	  }	  
  }
  \item{filename}{the filename of the output (minus '.pdf', 'eps' or '.eps'}
  \item{...}{additional parameters to be passed}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\examples{
  data(pop)
  x <- pop[pop[,2]=="Insgesamt",3]
  a <- newSparkLine(values=x, pointWidth=8)
  plotSparks(a, outputType='pdf', filename='myFirstSparkLine')
}
