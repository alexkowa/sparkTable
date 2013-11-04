\name{getParameter}
\alias{getParameter}
\title{Functions to interact with a Sparkline object}
\description{
  Basic functions to query parameters for objects of class 'sparkline', 'sparkbar', 
  'sparkbox', 'sparkTable' or 'geoTable'.
}
\usage{
	getParameter(object, type)
}
\arguments{
  \item{object}{objects of class 'sparkline', 'sparkbar', 'sparkbox', 'sparkTable' or 'geoTable'}
  \item{type}{one of the following:
	\itemize{
	   \item 'width': query slot 'width' for objects of class 'spark' and classes that directly extend this class.
	   \item 'height': query slot 'height' for objects of class 'spark' and classes that directly extend this class.
	   \item 'values': query slot 'values' for objects of class 'spark' and classes that directly extend this class.
	   \item 'padding': query slot 'padding' for objects of class 'spark' and classes that directly extend this class.
	   \item 'allColors': query slot 'allColors' for objects of class 'sparkline'.
	   \item 'lineWidth': query slot 'lineWidth' for objects of class 'sparkline'.
	   \item 'pointWidth': query slot 'pointWidth' for objects of class 'sparkline'.
	   \item 'showIQR': query slot 'showIQR' for objects of class 'sparkline'.
	   \item 'boxCol': query slot 'boxCol' for objects of class 'sparkbox'.
	   \item 'outCol': query slot 'outCol' for objects of class 'sparkbox'.	 
	   \item 'boxLineWidth': query slot 'boxLineWidth' for objects of class 'sparkbox'.
	   \item 'barCol': query slot 'barCol' for objects of class 'sparkbar'.	 	   
	   \item 'barSpacingPerc': query slot 'barSpacingPerc' for objects of class 'sparkbar'.
	   \item 'dataObj':	query slot 'dataObj' for objects of class 'sparkTable'.	
	   \item 'tableContent': query slot 'tableContent' for objects of class 'sparkTable'.	
	   \item 'varType':	query slot 'varType' for objects of class 'sparkTable'.		   
	   \item 'geographicVar': query slot 'geographicVar' for objects of class 'geoTable'.		   	
	   \item 'geographicInfo': query slot 'geographicInfo' for objects of class 'geoTable'.	
	   \item 'geographicOrder':	query slot 'geographicOrder' for objects of class 'geoTable'.	
	}  
  }
}

\author{Bernhard Meindl, Alexander Kowarik, Statistics Austria }

\seealso{\code{\link{setParameter}}}

\examples{
  data(pop)
  x <- pop[pop[,2]=="Insgesamt",3]
  a <- newSparkLine(values=x, pointWidth=8)
  
  a <- setParameter(a, type='values', value=sample(1:10, 15, replace=TRUE))
  getParameter(a, 'values')

  a <- setParameter(a, type='allColors',
    value=c("darkred", "darkgreen","darkblue", "white", "black", "red"))
  getParameter(a, 'allColors')

  getParameter(a, 'pointWidth')
  a <- setParameter(a, type='pointWidth', value=3)
  getParameter(a, 'pointWidth')
 
  a <- setParameter(a, type='lineWidth', value=1)
  a <- setParameter(a, type='width', value=6)
  a <- setParameter(a, type='height', value=.6)
}
