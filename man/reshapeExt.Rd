\name{reshapeExt}
\alias{reshapeExt}
\title{Reshaping datasets}
\description{
  reshapeExt() can be used to transform data that are already in 'long' format
  to the form that the data can be used by \code{\link{newSparkTable}} or \code{\link{newGeoTable}}.
}
\usage{
reshapeExt(data, timeValues=NULL,
       geographicVar=NULL,varying = NULL, v.names = NULL, timevar = "time",
        idvar = "id", ids = 1:NROW(data),
        drop = NULL, new.row.names = NULL,
        sep = ".",
        split = if (sep == "") {
            list(regexp = "[A-Za-z][0-9]", include = TRUE)
        } else {
            list(regexp = sep, include = FALSE, fixed = TRUE)}
        )
}
\arguments{
  \item{data}{a data frame}
  \item{timeValues}{if specified, vector of valid time-points}
  \item{geographicVar}{if specified, name of a variable in x holding regional information.}
  \item{varying}{names of sets of variables in the wide format that correspond to single variables in long format ('time-varying'). This is canonically a list of vectors of variable names, but it can optionally be a matrix of names, or a single vector of names. In each case, the names can be replaced by indices which are interpreted as referring to names(data). See 'Details of ?reshape' for more details and options.}
\item{v.names}{names of variables in the long format that correspond to multiple variables in the wide format. See 'Details of ?reshape'.}

\item{timevar}{the variable in long format that differentiates multiple records from the same group or individual. If more than one record matches, the first will be taken (with a warning).}

\item{idvar}{Names of one or more variables in long format that identify multiple records from the same group/individual. These variables may also be present in wide format.}

\item{ids}{the values to use for a newly created idvar variable in long format.}


\item{drop}{a vector of names of variables to drop before reshaping.}

\item{new.row.names}{character or NULL: a non-null value will be used for the row names of the result.}

\item{sep}{A character vector of length 1, indicating a separating character in the variable names in the wide format. This is used for guessing v.names and times arguments based on the names in varying. If sep == "", the split is just before the first numeral that follows an alphabetic character. This is also used to create variable names when reshaping to wide format.}

\item{split}{A list with three components, regexp, include, and (optionally) fixed. This allows an extended interface to variable name splitting. See 'Details of ?reshape'.}
}
\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }
\note{Wrapper for the stats function reshape.}
\seealso{\code{\link{setParameter}}, \code{\link{getParameter}},\code{\link{reshape}}}

\examples{
  data(pop,package='sparkTable')
  content <- list(
    function(x) { round(mean(x),2) },
    newSparkBox(), newSparkLine(), newSparkBar(),
    function(x) { round(tail(x,1),2) })
  names(content) <- paste('column',1:5,sep='')
  varType <- rep('value',5)
  pop <- pop[,c('variable','value','time')]
  pop$time <- as.numeric(as.character(pop$time))
  xx <- reshapeExt(pop, varying=list(2))
  x1 <- newSparkTable(xx, content, varType)
  #export(x1, outputType='html', graphNames='o2',filename='t1')
}
