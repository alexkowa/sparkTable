### Class definitions ###
# Definition of a "standard" class with the absolute required information
# other classes (line, bar, ...) extend the standard class
### default values for outputType html here, tex in newXYZ
setClass(
  Class="spark",
  representation=representation(
      width="numeric",
      height="numeric",
      values="numeric",
      padding="numeric", #top,bottom,left,right
      availableWidth="numeric",
      availableHeight="numeric",
      stepWidth="numeric",
      coordsX="numeric",
      coordsY="numeric"
  ),
  prototype=prototype(
      width=3.5,
      height=1,
      values=rpois(20, 5)*sample(c(1,-1), 20, replace=TRUE),
      padding=c(5,5,5,5), #top,bottom,left,right
      availableWidth=NULL,
      availableHeight=NULL,
      stepWidth=NULL,
      coordsX=NULL,
      coordsY=NULL
  ),
  validity=function(object) {
    if ( length(object@width) != 1 )
      stop("check argument 'width'!\n")
    if ( length(object@height) != 1 )
      stop("check argument 'height'!\n")
    if ( object@width - (object@padding[3]/100)*object@width - (object@padding[4]/100)*object@width <= 0 )
      stop("check argument 'padding'!\n")
    if ( object@height - (object@padding[1]/100)*object@height - (object@padding[2]/100)*object@height <= 0 )
      stop("check argument 'padding'!\n")
    if ( length(object@padding) != 4 )
      stop("argument 'padding' must be of length 4!\n")
    if ( any(object@padding < 1) )
      stop("'padding' must be at least 1/% to each margin!\n")
    return(TRUE)
  }
)
### Inheritance
setClass(
  Class="sparkline",
  representation=representation(
      allColors="ANY", # 1=min,2=max,3=last,4=fillbackground,5=line,6=iqr
      pointWidth="numeric",
      lineWidth="numeric",
      showIQR="logical"
  ),
  prototype=prototype(
      allColors=c('red', 'green', 'blue', 'white', 'black', '#bbbbbb'),
      pointWidth=4, # point Width
      lineWidth=1,
      showIQR=FALSE
  ),
  validity=function(object) {
    if ( !all(is.na(object@values)) ) {
      if ( object@showIQR==TRUE & diff(range(object@values, na.rm=TRUE)) == 0 ) {
        stop("no variation in input data! IQR-box can not be drawn!\n")
      }
    }
    if ( !is.null(object@allColors) && length(object@allColors) != 6 )
      stop("argument 'allColors' must be a vector (including NA's) of length 6!\n")
    if ( is.na(object@allColors[5]) )
      stop("a color for the plot-line must be specified!\n")
    if ( length(object@lineWidth) != 1 )
      stop("argument 'lineWidth' must be a numeric vector of length 1\n")
    if ( length(object@pointWidth) != 1 )
      stop("argument 'pointWidth' must be a numeric vector of length 1\n")
    if ( length(object@showIQR) != 1 )
      stop("argument 'showIQR' must be a logical vector of length 1\n")
    cols <- checkColors(object@allColors)
    if ( is.null(cols) )
      stop("please provide valid colors!\n")
    return(TRUE)
  },
  contains="spark"
)
setClass(
  Class="sparkbar",
  representation=representation(
      barCol="ANY",
      barWidth="numeric",
      barSpacingPerc="numeric",
      bgCol="ANY"
  ),
  prototype=prototype(
      barCol=c("#0000ff", "#ff0000", "#000000"), # negativ, positiv, lines
      barWidth=NULL,
      barSpacingPerc=2,
      bgCol="white"
  ),
  validity=function(object) {
    if ( !is.null(object@barCol) && length(object@barCol) != 3 )
      stop("Argument 'barCol' must be of length 3!\n")
    if ( object@barSpacingPerc >= 100 )
      stop("Argument 'barSpacingPerc' must be less than 100!\n")
    if ( object@barSpacingPerc <= 0 )
      stop("Argument 'barSpacingPerc' must be > than 0!\n")
    cols <- checkColors(object@barCol)
    if ( is.null(cols) )
      stop("please provide valid colors in 'barCol'!\n")
    return(TRUE)
  },
  contains="spark"
)
setClass(
    Class="sparkhist",contains="sparkbar")

setClass(
  Class="sparkbox",
  representation=representation(
      outCol="ANY", # outlierColor
      boxCol="ANY",   # 1=lineColor,2=fillColor
      boxLineWidth="numeric",
      bgCol="ANY"
  ),
  prototype=prototype(
      outCol=c('orange'),
      boxCol=c('#000000', 'orange'),
      boxLineWidth=1,
      bgCol="white"
  ),
  validity=function(object) {
    if ( sd(object@values, na.rm=T) == 0 )
      stop("no variation in values, sparkbox can not be drawn!\n")
    if ( !is.null(object@outCol) && length(object@outCol) != 1 )
      stop("'outCol' must be of length 1!\n")
    if ( !is.null(object@boxCol) && length(object@boxCol) != 2 )
      stop("'boxCol' must be of length 2!\n")
    if ( object@boxLineWidth < 1 )
      stop("'boxLineWidth' must be >= 1!\n")
    cols <- checkColors(object@outCol)
    if ( is.null(cols) )
      stop("please provide valid colors in 'outCol'!\n")
    cols <- checkColors(object@boxCol)
    if ( is.null(cols) )
      stop("please provide valid colors in 'boxCol'!\n")
  },
  contains="spark"
)

setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("dfOrNULL", c("data.frame", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClass(
  Class="sparkTable",
  representation=representation(
      dataObj="dfOrNULL",
      varType="characterOrNULL",
      tableContent="listOrNULL"
  ),
  prototype=prototype(
      dataObj=NULL,
      varType=NULL,
      tableContent=NULL
  ),
  validity=function(object) {
    if ( !is.null(object@dataObj) && length(attributes(object@dataObj)$reshapeLong) == 0)
      stop("'dataObj' must be in 'long' format -> use reshapeExt()?\n")
    if ( !is.null(object@tableContent) ) {
      con <- object@tableContent
      for(i in 1:length(con)){
        if(!class(con[[i]])%in%c("sparkline","sparkbox","sparkbar","sparkhist","function"))
          stop("'tableContent' must be a list with elements of class 'sparkline','sparkbox','sparkbar','sparkhist' or 'function'\n")

      }
    }
    if ( !is.null(object@varType) && length(setdiff(unique(object@varType), colnames(object@dataObj))) != 0 )
      stop("check argument 'varType' (only valid variable names must be used!\n")
    if ( !is.null(object@varType) && length(object@varType) != length(object@tableContent) ) {
      print(object@varType)
      print(object@tableContent)
      stop("'varType' and 'tableContent' must have the same length!\n")
    }
  }
)
setClass(
  Class="geoTable",
  representation=representation(
      dataObj="listOrNULL",
      varType="characterOrNULL",
      tableContent="listOrNULL",
      geographicVar="characterOrNULL",
      geographicInfo="dfOrNULL",
      geographicOrder="dfOrNULL"
  ),
  prototype=prototype(
      dataObj=NULL,
      varType=NULL,
      tableContent=NULL,
      geographicVar=NULL,
      geographicInfo=NULL,
      geographicOrder=NULL
  ),
  validity=function(object) {
#      if ( !is.null(object@dataObj) && length(attributes(object@dataObj)$reshapeLong) == 0)
#        stop("'dataObj' must be in 'long' format -> use reshapeExt()?\n")
    if ( !is.null(object@tableContent) ) {
      con <- object@tableContent
      for(i in 1:length(con)){
        if(!class(con[[i]])%in%c("sparkline","sparkbox","sparkbar","sparkhist","function"))
          stop("'tableContent' must be a list with elements of class 'sparkline','sparkbox','sparkbar','sparkhist' or 'function'\n")

      }
    }
#      if ( !is.null(object@varType) && length(setdiff(unique(object@varType), colnames(object@dataObj))) != 0 )
#        stop("check argument 'varType' (only valid variable names must be used!\n")
#      if ( !is.null(object@varType) && length(object@varType) != length(object@tableContent) ) {
#        print(object@varType)
#        print(object@tableContent)
#        stop("'varType' and 'tableContent' must have the same length!\n")
#      }

  }
)
