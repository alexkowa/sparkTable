######################################
### Definition of generic methods ####
######################################
setGeneric("scaleSpark", function(.Object, vMin=NULL, vMax=NULL) { standardGeneric("scaleSpark")} )

### These generic functions are inherited by objects
### of classes 'sparkline', 'sparkbox' and 'sparkbar'
setGeneric('width<-', function(object,value) {standardGeneric('width<-')})
setGeneric('width', function(object,value) {standardGeneric('width')})
setGeneric('height<-', function(object,value) {standardGeneric('height<-')})
setGeneric('height', function(object,value) {standardGeneric('height')})
setGeneric('values<-', function(object,value) {standardGeneric('values<-')})
setGeneric('values', function(object,value) {standardGeneric('values')})
setGeneric('padding<-', function(object,value) {standardGeneric('padding<-')})
setGeneric('padding', function(object,value) {standardGeneric('padding')})

### These generic functions are used for objects of class 'sparkline'
setGeneric('allColors<-', function(object,value) {standardGeneric('allColors<-')})
setGeneric('allColors', function(object,value) {standardGeneric('allColors')})
setGeneric('lineWidth<-', function(object,value) {standardGeneric('lineWidth<-')})
setGeneric('lineWidth', function(object,value) {standardGeneric('lineWidth')})
setGeneric('pointWidth<-', function(object,value) {standardGeneric('pointWidth<-')})
setGeneric('pointWidth', function(object,value) {standardGeneric('pointWidth')})
setGeneric('showIQR<-', function(object,value) {standardGeneric('showIQR<-')})
setGeneric('showIQR', function(object,value) {standardGeneric('showIQR')})

### These generic functions are used for objects of class 'sparkbar'
setGeneric('barCol<-', function(object,value) {standardGeneric('barCol<-')})
setGeneric('barCol', function(object,value) {standardGeneric('barCol')})
setGeneric('barWidth<-', function(object,value) {standardGeneric('barWidth<-')})
setGeneric('barWidth', function(object,value) {standardGeneric('barWidth')})
setGeneric('barSpacingPerc<-', function(object,value) {standardGeneric('barSpacingPerc<-')})
setGeneric('barSpacingPerc', function(object,value) {standardGeneric('barSpacingPerc')})
setGeneric('bgCol<-', function(object,value) {standardGeneric('bgCol<-')})
setGeneric('bgCol', function(object,value) {standardGeneric('bgCol')})

### These generic functions are used for objects of class 'sparkbox'
setGeneric('outCol<-', function(object,value) {standardGeneric('outCol<-')})
setGeneric('outCol', function(object,value) {standardGeneric('outCol')})
setGeneric('boxCol<-', function(object,value) {standardGeneric('boxCol<-')})
setGeneric('boxCol', function(object,value) {standardGeneric('boxCol')})
setGeneric('boxOutCol<-', function(object,value) {standardGeneric('boxOutCol<-')})
setGeneric('boxOutCol', function(object,value) {standardGeneric('boxOutCol')})
setGeneric('boxMedCol<-', function(object,value) {standardGeneric('boxMedCol<-')})
setGeneric('boxMedCol', function(object,value) {standardGeneric('boxMedCol')})
setGeneric('boxLineWidth<-', function(object,value) {standardGeneric('boxLineWidth<-')})
setGeneric('boxLineWidth', function(object,value) {standardGeneric('boxLineWidth')})
setGeneric('boxShowOut<-', function(object,value) {standardGeneric('boxShowOut<-')})
setGeneric('boxShowOut', function(object,value) {standardGeneric('boxShowOut')})
setGeneric('bgCol<-', function(object,value) {standardGeneric('bgCol<-')})
setGeneric('bgCol', function(object,value) {standardGeneric('bgCol')})

### These generic functions are used for objects of class 'sparkTable'
setGeneric('dataObj<-', function(object,value) {standardGeneric('dataObj<-')})
setGeneric('dataObj', function(object,value) {standardGeneric('dataObj')})
setGeneric('tableContent<-', function(object,value) {standardGeneric('tableContent<-')})
setGeneric('tableContent', function(object,value) {standardGeneric('tableContent')})
setGeneric('varType<-', function(object,value) {standardGeneric('varType<-')})
setGeneric('varType', function(object,value) {standardGeneric('varType')})


### These generic functions are used for objects of class 'geoTable'
setGeneric('geographicVar<-', function(object,value) {standardGeneric('geographicVar<-')})
setGeneric('geographicVar', function(object,value) {standardGeneric('geographicVar')})
setGeneric('geographicInfo<-', function(object,value) {standardGeneric('geographicInfo<-')})
setGeneric('geographicInfo', function(object,value) {standardGeneric('geographicInfo')})
setGeneric('geographicOrder<-', function(object,value) {standardGeneric('geographicOrder<-')})
setGeneric('geographicOrder', function(object,value) {standardGeneric('geographicOrder')})


###########################
### Initialize methods ####
###########################
setMethod (
    f='initialize',
    signature='sparkline',
    definition=function(.Object, vMin=NULL, vMax=NULL) {
      .Object <- scaleSpark(.Object, vMin=vMin, vMax=vMin)
      validObject(.Object) # call of inspector
      return(.Object)
    }
)

setMethod (
    f='initialize',
    signature='sparkbox',
    definition=function(.Object, vMin=NULL, vMax=NULL) {
      .Object <- scaleSpark(.Object, vMin=vMin, vMax=vMin)
      validObject(.Object) # call of inspector
      return(.Object)
    }
)

setMethod (
    f='initialize',
    signature='sparkbar',
    definition=function(.Object, vMin=NULL, vMax=NULL) {
      .Object <- scaleSpark(.Object, vMin=vMin, vMax=vMin)
      validObject(.Object) # call of inspector
      return(.Object)
    }
)
setMethod (
    f='initialize',
    signature='sparkhist',
    definition=function(.Object, vMin=NULL, vMax=NULL) {
      .Object <- scaleSpark(.Object, vMin=vMin, vMax=vMin)
      validObject(.Object) # call of inspector
      return(.Object)
    }
)
setMethod (
    f='initialize',
    signature='sparkTable',
    definition=function(.Object) {
      validObject(.Object) # call of inspector
      return(.Object)
    }
)

setMethod (
    f='initialize',
    signature='geoTable',
    definition=function(.Object) {
      validObject(.Object) # call of inspector
      return(.Object)
    }
)


######################
### Scale methods ####
######################
setMethod(
    f="scaleSpark",
    signature="sparkline",
    def=function(.Object, vMin, vMax) {
      if ( all(is.na(.Object@values)) ) {
        return(.Object)
      }

      if ( is.null(vMin))
        vMin <- min(.Object@values, na.rm=TRUE)
      if ( is.null(vMax))
        vMax <- max(.Object@values, na.rm=TRUE)

      len <- length(.Object@values)
      .Object@availableWidth <- .Object@width - .Object@width*((.Object@padding[3])/100) - .Object@width*((.Object@padding[4])/100)
      .Object@availableHeight <- .Object@height - .Object@height*((.Object@padding[1])/100) - .Object@height*((.Object@padding[2])/100)
      .Object@stepWidth <- (.Object@availableWidth*1.0) / (len-1)
      .Object@coordsX <- (0:(len-1))*.Object@stepWidth + .Object@width*(.Object@padding[3]/100)

      v <- .Object@values
      if(vMin!=vMax){
        if ( vMin >= 0 ) {
          v <- v - vMin
          steps <- .Object@availableHeight / (vMax-vMin)
          .Object@coordsY <- v * steps
        }
        if ( vMin < 0 ) {
          v <- v - vMax
          steps <- .Object@availableHeight / (vMax-vMin)
          .Object@coordsY <- .Object@availableHeight - abs(v * -steps)
        }
        .Object@coordsY <- .Object@coordsY + (.Object@height-.Object@availableHeight)/2
      }else{
        .Object@coordsY <- (.Object@height-.Object@availableHeight)/2
      }
      return(.Object)
    }
)

setMethod(
    f="scaleSpark",
    signature="sparkbox",
    def=function(.Object, vMin, vMax) {
      if ( all(is.na(.Object@values)) ) {
        return(.Object)
      }
      if ( is.null(vMin))
        vMin <- min(.Object@values, na.rm=TRUE)
      if ( is.null(vMax))
        vMax <- max(.Object@values, na.rm=TRUE)

      len <- length(.Object@values)
      .Object@availableWidth <- .Object@width - (.Object@width*((.Object@padding[3])/100) + .Object@width*((.Object@padding[4])/100))
      .Object@availableHeight <- .Object@height - (.Object@height*((.Object@padding[1])/100) + .Object@height*((.Object@padding[2])/100))
      .Object@stepWidth <- (.Object@availableHeight*1.0) / (len-1)
      .Object@coordsY <- (0:(len-1))*.Object@stepWidth + .Object@height*(.Object@padding[1]/100)

      v <- .Object@values
      if ( vMin >= 0 ) {
        v <- v - vMin
        steps <- .Object@availableWidth / (vMax-vMin)
        .Object@coordsX <- v * steps
      }
      if ( vMin < 0 ) {
        v <- v - vMax
        steps <- .Object@availableWidth / (vMax-vMin)
        .Object@coordsX <- .Object@availableWidth - abs(v * -steps)
      }
      .Object@coordsX <- .Object@coordsX + (.Object@width-.Object@availableWidth)/2
      return(.Object)
    }
)

setMethod(
    f="scaleSpark",
    signature="sparkhist",
    def=function(.Object, vMin, vMax) {
      if ( all(is.na(.Object@values)) ) {
        return(.Object)
      }
      hh <- hist(.Object@values,plot=FALSE)
      vals <- hh$counts
      mids <- hh$mids
      if ( is.null(vMin))
        vMin <- min(mids, na.rm=TRUE)
      if ( is.null(vMax))
        vMax <- max(mids, na.rm=TRUE)

      len <- length(vals)
      .Object@availableWidth <- .Object@width - .Object@width*((.Object@padding[3])/100) - .Object@width*((.Object@padding[4])/100)
      .Object@availableHeight <- .Object@height - .Object@height*((.Object@padding[1])/100) - .Object@height*((.Object@padding[2])/100)
      lowerCutOff <-((min(mids)-vMin)/(vMax-vMin))*.Object@availableWidth
      upperCutOff <- ((vMax-max(mids))/(vMax-vMin))*.Object@availableWidth
      .Object@availableWidth <- .Object@availableWidth - lowerCutOff - upperCutOff
      .Object@stepWidth <- (.Object@availableWidth * (.Object@barSpacingPerc/100))/(len-1)
      .Object@barWidth <- (.Object@availableWidth - .Object@stepWidth*(len-1))/len
      .Object@coordsX <- lowerCutOff+(0:(len-1))*.Object@barWidth + .Object@stepWidth* (0:(len-1)) + .Object@width*(.Object@padding[3]/100)

      v <- vals
      mid <- floor(.Object@height / 2)

      # all positive?
      if ( all(v >= 0, na.rm=TRUE) ) {
        steps <- .Object@availableHeight / max(vals)
        .Object@coordsY <- v * steps + .Object@height*(.Object@padding[2]/100)
        .Object@coordsY <- .Object@coordsY - ((.Object@height-.Object@availableHeight)/2)
      }
      # all negative
      else if ( all(v <= 0, na.rm=TRUE) ) {
        steps <- abs(.Object@availableHeight / min(vals))
        .Object@coordsY <- v * steps - .Object@height*(.Object@padding[1]/100)
        .Object@coordsY <- .Object@coordsY + ((.Object@height-.Object@availableHeight)/2)
      }
      # negative and positive values
      else {
        absMax <- max(abs(c(min(vals),max(vals))), na.rm=TRUE)
        steps <- (.Object@availableHeight)/ 2 / absMax
        .Object@coordsY <- v * steps
      }
      return(.Object)
    }
)

setMethod(
    f="scaleSpark",
    signature="sparkbar",
    def=function(.Object, vMin, vMax) {
      if ( all(is.na(.Object@values)) ) {
        return(.Object)
      }
      if ( is.null(vMin))
        vMin <- min(.Object@values, na.rm=TRUE)
      if ( is.null(vMax))
        vMax <- max(.Object@values, na.rm=TRUE)

      len <- length(.Object@values)
      .Object@availableWidth <- .Object@width - .Object@width*((.Object@padding[3])/100) - .Object@width*((.Object@padding[4])/100)
      .Object@availableHeight <- .Object@height - .Object@height*((.Object@padding[1])/100) - .Object@height*((.Object@padding[2])/100)
      .Object@stepWidth <- (.Object@availableWidth * (.Object@barSpacingPerc/100))/(len-1)
      .Object@barWidth <- (.Object@availableWidth - .Object@stepWidth*(len-1))/len
      .Object@coordsX <- (0:(len-1))*.Object@barWidth + .Object@stepWidth* (0:(len-1)) + .Object@width*(.Object@padding[3]/100)

      v <- .Object@values
      mid <- floor(.Object@height / 2)

      # all positive?
      if ( all(v >= 0, na.rm=TRUE) ) {
        steps <- .Object@availableHeight / vMax
        .Object@coordsY <- v * steps + .Object@height*(.Object@padding[2]/100)
        .Object@coordsY <- .Object@coordsY - ((.Object@height-.Object@availableHeight)/2)
      }
      # all negative
      else if ( all(v <= 0, na.rm=TRUE) ) {
        steps <- abs(.Object@availableHeight / vMin)
        .Object@coordsY <- v * steps - .Object@height*(.Object@padding[1]/100)
        .Object@coordsY <- .Object@coordsY + ((.Object@height-.Object@availableHeight)/2)
      }
      # negative and positive values
      else {
        absMax <- max(abs(c(vMin,vMax)), na.rm=TRUE)
        steps <- (.Object@availableHeight)/ 2 / absMax
        .Object@coordsY <- v * steps
      }
      return(.Object)
    }
)


######################
### Print methods ####
######################
setMethod(
    f="print",
    signature="spark",
    definition=function(x,...) {
      cat("---class 'spark' (method=print) --- \n")
      cat("nrValues:", length(x@values),"\n")
      cat("min(values):", min(x@values),"\n")
      cat("max(values):", max(x@values),"\n")
      cat("width:", x@width,"\n")
      cat("height:", x@height,"\n")#
      cat("---class 'spark' (method=print) --- \n")
    }
)


############################
### Set/Replace methods ####
############################

### set/replace slots 'width', 'height', 'values' and 'padding'
### of objects of class 'spark'
setReplaceMethod(
    f='width',
    signature='spark',
    definition=function(object,value) {
      object@width <- value
      object <- scaleSpark(object)
      validObject(object)
      return(object)
    }
)
setMethod(
    f='width',
    signature='spark',
    definition=function(object) { return(object@width) }
)

setReplaceMethod(
    f='height',
    signature='spark',
    definition=function(object,value) {
      object@height <- value
      object <- scaleSpark(object)
      validObject(object)
      return(object)
    }
)
setMethod(
    f='height',
    signature='spark',
    definition=function(object) { return(object@height) }
)

setReplaceMethod(
    f='values',
    signature='spark',
    definition=function(object,value) {
      object@values <- value
      object <- scaleSpark(object)
      validObject(object)
      return(object)
    }
)
setMethod(
    f='values',
    signature='spark',
    definition=function(object) { return(object@values) }
)

### set and get padding for sparkobject ###
setReplaceMethod(
    f='padding',
    signature='spark',
    definition=function(object,value) {
      object@padding <- value
      object <- scaleSpark(object)
      validObject(object)
      return(object)
    }
)
setMethod(
    f='padding',
    signature='spark',
    definition=function(object) { return(object@padding) }
)


### set/replace slots 'allColors', 'lineWidth', 'pointWidth' and 'showIQR'
### of objects of class 'sparkline'
setReplaceMethod(
    f='allColors',
    signature='sparkline',
    definition=function(object,value) {
      object@allColors <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='allColors',
    signature='sparkline',
    definition=function(object) { return(object@allColors) }
)

setReplaceMethod(
    f='lineWidth',
    signature='sparkline',
    definition=function(object,value) {
      object@lineWidth <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='lineWidth',
    signature='sparkline',
    definition=function(object) { return(object@lineWidth) }
)

setReplaceMethod(
    f='pointWidth',
    signature='sparkline',
    definition=function(object,value) {
      object@pointWidth <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='pointWidth',
    signature='sparkline',
    definition=function(object) { return(object@pointWidth) }
)

setReplaceMethod(
    f='showIQR',
    signature='sparkline',
    definition=function(object,value) {
      object@showIQR <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='showIQR',
    signature='sparkline',
    definition=function(object) { return(object@showIQR) }
)


### set/replace slots 'barCol', 'barWidth' and 'barSpacingPerc', 'bgCol'
### of objects of class 'sparkbar'
setReplaceMethod(
    f='barWidth',
    signature='sparkbar',
    definition=function(object,value) {
      object@barWidth <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='barWidth',
    signature='sparkbar',
    definition=function(object) { return(object@barWidth) }
)
setReplaceMethod(
    f='barCol',
    signature='sparkbar',
    definition=function(object,value) {
      object@barCol <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='barCol',
    signature='sparkbar',
    definition=function(object) { return(object@barCol) }
)

setReplaceMethod(
    f='barSpacingPerc',
    signature='sparkbar',
    definition=function(object,value) {
      object@barSpacingPerc <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='barSpacingPerc',
    signature='sparkbar',
    definition=function(object) { return(object@barSpacingPerc) }
)
setReplaceMethod(
    f='bgCol',
    signature='sparkbar',
    definition=function(object,value) {
      object@bgCol <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='bgCol',
    signature='sparkbar',
    definition=function(object) { return(object@bgCol) }
)
### set/replace slots 'barCol', 'barWidth' and 'barSpacingPerc'
### of objects of class 'sparkhist'
#setReplaceMethod(
#    f='barWidth',
#    signature='sparkhist',
#    definition=function(object,value) {
#      object@barWidth <- value
#      validObject(object)
#      return(object)
#    }
#)
#setMethod(
#    f='barWidth',
#    signature='sparkhist',
#    definition=function(object) { return(object@barWidth) }
#)
#setReplaceMethod(
#    f='barCol',
#    signature='sparkhist',
#    definition=function(object,value) {
#      object@barCol <- value
#      validObject(object)
#      return(object)
#    }
#)
#setMethod(
#    f='barCol',
#    signature='sparkhist',
#    definition=function(object) { return(object@barCol) }
#)
#
#setReplaceMethod(
#    f='barSpacingPerc',
#    signature='sparkhist',
#    definition=function(object,value) {
#      object@barSpacingPerc <- value
#      validObject(object)
#      return(object)
#    }
#)
#setMethod(
#    f='barSpacingPerc',
#    signature='sparkhist',
#    definition=function(object) { return(object@barSpacingPerc) }
#)
### set/replace slots 'outCol', 'boxCol', 'boxOutCol', 'boxMedCol', 'boxLineWidth',
### and 'boxShowOut' of objects of class 'sparkbox'
setReplaceMethod(
    f='outCol',
    signature='sparkbox',
    definition=function(object, value) {
      object@outCol <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='outCol',
    signature='sparkbox',
    definition=function(object) { return(object@outCol) }
)

setReplaceMethod(
    f='boxCol',
    signature='sparkbox',
    definition=function(object, value) {
      object@boxCol <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='boxCol',
    signature='sparkbox',
    definition=function(object) { return(object@boxCol) }
)

setMethod(
    f='boxMedCol',
    signature='sparkbox',
    definition=function(object) { return(object@boxMedCol) }
)
setReplaceMethod(
    f='boxMedCol',
    signature='sparkbox',
    definition=function(object, value) {
      object@boxMedCol <- value
      validObject(object)
      return(object)
    }
)

setMethod(
    f='boxOutCol',
    signature='sparkbox',
    definition=function(object) { return(object@boxOutCol) }
)
setReplaceMethod(
    f='boxOutCol',
    signature='sparkbox',
    definition=function(object, value) {
      object@boxOutCol <- value
      validObject(object)
      return(object)
    }
)

setReplaceMethod(
    f='boxLineWidth',
    signature='sparkbox',
    definition=function(object, value) {
      object@boxLineWidth <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='boxLineWidth',
    signature='sparkbox',
    definition=function(object) { return(object@boxLineWidth) }
)

setReplaceMethod(
    f='boxShowOut',
    signature='sparkbox',
    definition=function(object, value) {
      object@boxShowOut <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='boxShowOut',
    signature='sparkbox',
    definition=function(object) { return(object@boxShowOut) }
)

setReplaceMethod(
    f='bgCol',
    signature='sparkbox',
    definition=function(object, value) {
      object@bgCol <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='bgCol',
    signature='sparkbox',
    definition=function(object) { return(object@bgCol) }
)

### set/replace slots 'dataObj', 'tableContent' and 'varType'
### of objects of class 'sparkTable'
setReplaceMethod(
    f='dataObj',
    signature='sparkTable',
    definition=function(object, value) {
      object@dataObj <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='dataObj',
    signature='sparkTable',
    definition=function(object) { return(object@dataObj) }
)

setReplaceMethod(
    f='tableContent',
    signature='sparkTable',
    definition=function(object, value) {
      object@tableContent <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='tableContent',
    signature='sparkTable',
    definition=function(object) { return(object@tableContent) }
)

setReplaceMethod(
    f='varType',
    signature='sparkTable',
    definition=function(object, value) {
      object@varType <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='varType',
    signature='sparkTable',
    definition=function(object) { return(object@varType) }
)


### set/replace slots 'dataObj', 'tableContent', 'varType', 'geographicVar',
### 'geographicInfo' and 'geographicOrder' of objects of class 'geoTable'
setReplaceMethod(
    f='dataObj',
    signature='geoTable',
    definition=function(object, value) {
      object@dataObj <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='dataObj',
    signature='geoTable',
    definition=function(object) { return(object@dataObj) }
)

setReplaceMethod(
    f='tableContent',
    signature='geoTable',
    definition=function(object, value) {
      object@tableContent <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='tableContent',
    signature='geoTable',
    definition=function(object) { return(object@tableContent) }
)

setReplaceMethod(
    f='varType',
    signature='geoTable',
    definition=function(object, value) {
      object@varType <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='varType',
    signature='geoTable',
    definition=function(object) { return(object@geographicVar) }
)
setReplaceMethod(
    f='geographicVar',
    signature='geoTable',
    definition=function(object, value) {
      object@geographicVar <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='geographicVar',
    signature='geoTable',
    definition=function(object) { return(object@geographicVar) }
)
setReplaceMethod(
    f='geographicInfo',
    signature='geoTable',
    definition=function(object, value) {
      object@geographicInfo <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='geographicInfo',
    signature='geoTable',
    definition=function(object) { return(object@geographicInfo) }
)

setReplaceMethod(
    f='geographicOrder',
    signature='geoTable',
    definition=function(object, value) {
      object@geographicOrder <- value
      validObject(object)
      return(object)
    }
)
setMethod(
    f='geographicOrder',
    signature='geoTable',
    definition=function(object) { return(object@geographicOrder) }
)


#####################
### Plot methods ####
#####################

plotEmpty <- function(df) {
  x <- y <- NULL
  p <- ggplot(data=df)
  p <- p + geom_point(aes(x=x, y=y), color="white")
  p <- p + theme(
    line = element_blank(),
    text = element_blank(),
    title = element_blank(),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    legend.background=element_rect(fill="white", colour=NA),
    plot.background=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    panel.grid = element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.background=element_rect(fill="white")
  )
  p <- p + labs(x=NULL, y=NULL)
  return(p)
}

setMethod(f='plot', signature='sparkline', definition=function(x, y,...) {
  y <- 0
  if ( all(is.na(x@values)) ) {
    df <- data.frame(x=1:length(x@values), y=0)
    p <- plotEmpty(df)
    return(p)
  }
  df <- data.frame(x=x@coordsX, y=x@coordsY)
  p <- ggplot(data=df)
  p <- p + theme(
    legend.position = "none",
    line = element_blank(),
    text = element_blank(),
    title = element_blank(),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    legend.background=element_rect(fill="white", colour=NA),
    plot.background=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    panel.margin = unit(0,"null"),
    plot.margin = rep(unit(0,"null"),4),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0,"null"),
    #axis.ticks.margin = unit(0,"null"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.background=element_rect(fill=allColors(x)[4])
  )
#  p <- p + scale_x_continuous(expand=c(0,0.02)) + scale_y_continuous(expand=c(0,0.02))
  p <- p + labs(x=NULL, y=NULL)

  # IQR
  if ( showIQR(x) ) {
    n <- length(df$x)
    x1 <- df$x[1]
    x2 <- df$x[n]
    y1 <- as.numeric(quantile(df$y, 0.25))
    y2 <- as.numeric(quantile(df$y, 0.75))
    p <- p + geom_rect(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=allColors(x)[6], color=allColors(x)[6])
  }
  lw <- lineWidth(x)
  #lw <- min(round(lineWidth(x)), 10)
  #lw <- max(1, lw)
  #lw <- seq(0.5, 2, length=10)[lw]

  p <- p + geom_line(aes(x=x, y=y), size=lw, color=allColors(x)[5])
  p <- p + geom_point(aes(x=x, y=y), size=lw/20, color=allColors(x)[5])

  # points
  size_p <- pointWidth(x)
  #size_p <- min(round(pointWidth(x)), 10)
  #size_p <- max(1, size_p)
  #size_p <- round(seq(2.5, 3.1, length=10),2)[size_p]
  p <- p + scale_x_continuous(expand=c(0,size_p/50)) + scale_y_continuous(expand=c(0,size_p/50))
  # minimum
  if ( !is.na(allColors(x)[1]) ) {
    minIndex <- max(which(df$y==min(na.omit(df$y))))
    p <- p + geom_point(x=df$x[minIndex], y=df$y[minIndex], color=allColors(x)[1], size=size_p)
  }
  #maximum
  if ( !is.na(allColors(x)[2]) ) {
    maxIndex <- max(which(df$y==max(na.omit(df$y))))
    p <- p + geom_point(x=df$x[maxIndex], y=df$y[maxIndex], color=allColors(x)[2], size=size_p)
  }
  #last point
  if ( !is.na(allColors(x)[3]) ) {
    lastIndex <- length(df$y)
    p <- p + geom_point(x=df$x[lastIndex], y=df$y[lastIndex], color=allColors(x)[3], size=size_p)
  }

  params <- list(...)
  if ( !is.null(params$padding) ) {
    pad <- params$padding
    if ( !is.numeric(pad) | length(pad) != 4 ) {
      warning("padding must be a numeric vector of length 4 --> not used!\n")
    }
    rg <- seq(0.001, 0.2, length=20)
    pad <- sapply(pad, function(x) { min(x, 20) })
    pad <- sapply(pad, function(x) { max(1, x) })
    pad <- rg[ceiling(pad)]
    p <- p + theme(plot.margin = unit(pad, "npc"))
  }
  return(p)
})

setMethod(f='plot', signature='sparkbar', definition=function(x, y, ...) {
  xmin <- xmax <- ymin <- ymax <- NULL
  if ( all(is.na(x@values)) ) {
    df <- data.frame(x=1:length(x@values), y=0)
    p <- plotEmpty(df)
    return(p)
  }
  x@coordsY[is.na(x@coordsY)] <- 0
  df <- data.frame(xmin=x@coordsX-x@barWidth/2, xmax=x@coordsX+x@barWidth/2, ymin=0, ymax=x@coordsY,barCol="A",stringsAsFactors = FALSE)
  df$xmin[-1] <- df$xmax[-nrow(df)]
  df[x@values<0,"barCol"] <- "B"
  p <- ggplot(df)
  p <- p + theme(
    legend.position = "none",
    line = element_blank(),
    text = element_blank(),
    title = element_blank(),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    legend.background=element_rect(fill="white", colour=NA),
    plot.background=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    panel.margin = unit(0,"null"),
    plot.margin = rep(unit(0,"null"),4),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0,"null"),
    #axis.ticks.margin = unit(0,"null"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.background=element_rect(fill=bgCol(x))
  )
  p <- p + scale_x_continuous(expand=c(0,0.02)) + scale_y_continuous(expand=c(0,0.02))
  p <- p + labs(x=NULL, y=NULL)

  p <- p + geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,fill=barCol), colour=x@barCol[3], size=0)
  if(all(df$barCol=="A")){
    p <- p + scale_fill_manual(values=x@barCol[1],guide=FALSE)
  }else if(all(df$barCol=="B")){
    p <- p + scale_fill_manual(values=x@barCol[2],guide=FALSE)
  }else{
    p <- p + scale_fill_manual(values=x@barCol[1:2],guide=FALSE)
  }
  

  params <- list(...)
  if ( !is.null(params$padding) ) {
    pad <- params$padding
    if ( !is.numeric(pad) | length(pad) != 4 ) {
      warning("padding must be a numeric vector of length 4 --> not used!\n")
    }
    rg <- seq(0.001, 0.2, length=20)
    pad <- sapply(pad, function(x) { min(x, 20) })
    pad <- sapply(pad, function(x) { max(1, x) })
    pad <- rg[ceiling(pad)]
    p <- p + theme(plot.margin = unit(pad, "npc"))
  }
  p <- p + labs(x=NULL, y=NULL)
  return(p)
})

setMethod(f='plot', signature='sparkhist', definition=function(x, y, ...) {
  if ( all(is.na(x@values)) ) {
    df <- data.frame(x=1:length(x@values), y=0)
    p <- plotEmpty(df)
    return(p)
  }
  df <- data.frame(x=x@coordsX, y=x@coordsY)
  x@coordsY[is.na(x@coordsY)] <- 0
  p <- ggplot(df)
  p <- p + theme(
    legend.position = "none",
    line = element_blank(),
    text = element_blank(),
    title = element_blank(),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    legend.background=element_rect(fill="white", colour=NA),
    plot.background=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    panel.margin = unit(0,"null"),
    plot.margin = rep(unit(0,"null"),4),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0,"null"),
    #xis.ticks.margin = unit(0,"null"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.background=element_rect(fill=bgCol(x))
  )
  p <- p + scale_x_continuous(expand=c(0,0.02)) + scale_y_continuous(expand=c(0,0.02))
  p <- p + labs(x=NULL, y=NULL)

  p <- p + geom_histogram(aes(x=x, y=y), stat="identity", fill=x@barCol[2], col=x@barCol[3])

  params <- list(...)
  if ( !is.null(params$padding) ) {
    pad <- params$padding
    if ( !is.numeric(pad) | length(pad) != 4 ) {
      warning("padding must be a numeric vector of length 4 --> not used!\n")
    }
    rg <- seq(0.001, 0.2, length=20)
    pad <- sapply(pad, function(x) { min(x, 20) })
    pad <- sapply(pad, function(x) { max(1, x) })
    pad <- rg[ceiling(pad)]
    p <- p + theme(plot.margin = unit(pad, "npc"))
  }
  p <- p + labs(x=NULL, y=NULL)
  return(p)
})

setMethod(f='plot', signature='sparkbox', definition=function(x, y, ...) {
  if ( all(is.na(x@values)) ) {
    df <- data.frame(x=1:length(x@values), y=0)
    p <- plotEmpty(df)
    return(p)
  }
  df <- data.frame(x=x@coordsY, y=x@coordsX)
  p <- ggplot(data=df)
  p <- p + theme(
    legend.position = "none",
    line = element_blank(),
    text = element_blank(),
    title = element_blank(),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    legend.background=element_rect(fill="white", colour=NA),
    plot.background=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    panel.margin = unit(0,"null"),
    plot.margin = rep(unit(0,"null"),4),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0,"null"),
    #axis.ticks.margin = unit(0,"null"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.background=element_rect(fill=bgCol(x))
  )
  p <- p + scale_x_continuous(expand=c(0,0.02)) + scale_y_continuous(expand=c(0,0.02))
  p <- p + labs(x=NULL, y=NULL)

  p <- p + geom_boxplot(aes(x=x, y=y),
    stat="boxplot", position="dodge",
    outlier.colour=x@outCol,
    outlier.shape=16, outlier.size=3,
    notch=FALSE, notchwidth=0.5, color=x@boxCol[1], fill=x@boxCol[2])
    p <- p + coord_flip()

  params <- list(...)
  if ( !is.null(params$padding) ) {
    pad <- params$padding
    if ( !is.numeric(pad) | length(pad) != 4 ) {
      warning("padding must be a numeric vector of length 4 --> not used!\n")
    }
    rg <- seq(0.001, 0.2, length=20)
    pad <- sapply(pad, function(x) { min(x, 20) })
    pad <- sapply(pad, function(x) { max(1, x) })
    pad <- rg[ceiling(pad)]
    p <- p + theme(plot.margin = unit(pad, "npc"))
  }
  p <- p + labs(x=NULL, y=NULL)
  return(p)
})

setGeneric("export", function(object, ...) {
  standardGeneric("export")
})

setMethod(f='export', signature='sparkline',
  definition=function(object, outputType="pdf", filename="sparkLine", ...) {
    .Object <- object
    pp <- plot(.Object, ...) #+ theme(plot.margin=unit(c(-0.0,-0.0,-0.4,-0.4),c("line","line","line","line")))
    if ( !all(outputType %in% c("pdf","eps","png","svg")) ) {
      stop("please provide valid output types!\n")
    }
    #suppressWarnings(print(pp))
    for ( t in unique(outputType)) {
      ggsave(filename=paste0(filename, ".", t), plot=pp, units="in", width=.Object@width, height=.Object@height,bg="transparent")
    }
  }
)

setMethod(f='export', signature='sparkbar',
  definition=function(object, outputType="pdf", filename="sparkBar", ...) {
    .Object <- object
    pp <- plot(.Object) # theme(plot.margin=unit(c(-0.0,-0.0,-0.4,-0.4),c("line","line","line","line")))
    if ( !all(outputType %in% c("pdf","eps","png","svg")) ) {
      stop("please provide valid output types!\n")
    }
    #suppressWarnings(print(pp))
    for ( t in unique(outputType)) {
      ggsave(filename=paste0(filename, ".", t), plot=pp, units="in", width=.Object@width, height=.Object@height,bg="transparent")
    }
  }
)

setMethod(f='export', signature='sparkhist',
  definition=function(object, outputType="pdf", filename="sparkHist", ...) {
    .Object <- object
    pp <- plot(.Object) # theme(plot.margin=unit(c(-0.0,-0.0,-0.4,-0.4),c("line","line","line","line")))
    if ( !all(outputType %in% c("pdf","eps","png","svg")) ) {
      stop("please provide valid output types!\n")
    }
    #suppressWarnings(print(pp))
    for ( t in unique(outputType)) {
      ggsave(filename=paste0(filename, ".", t), plot=pp, units="in", width=.Object@width, height=.Object@height, bg="transparent")
    }
  }
)

setMethod(f='export', signature='sparkbox',
  definition=function(object, outputType="pdf", filename="sparkBox", ...) {
    .Object <- object
    pp <- plot(.Object) # theme(plot.margin=unit(c(-0.0,-0.0,-0.4,-0.4),c("line","line","line","line")))
    if ( !all(outputType %in% c("pdf","eps","png","svg")) ) {
      stop("please provide valid output types!\n")
    }
    #suppressWarnings(print(pp))
    for ( t in unique(outputType)) {
      ggsave(filename=paste0(filename, ".", t), plot=pp, units="in", width=.Object@width, height=.Object@height,bg="transparent")
    }
  }
)

setMethod(f='export', signature='sparkTable',
    definition=function(object, outputType="html", filename=NULL, graphNames="out",infonote=TRUE, scaleByCol=FALSE,...) {
    .Object <- object
    if ( !outputType %in% c("tex", "html", "htmlsvg") )
      stop("please provide a valid output type!\n")

    if(!is.null(filename)){
      if (outputType %in% c("html","tex"))
        filename <- paste0(filename, ".", outputType)
      if (outputType=="htmlsvg")
        filename <- paste0(filename,".html")
    }

    TH <- names(.Object@tableContent)
    # to data.frame
    if ( is.matrix(.Object@dataObj) ) {
      .Object@dataObj <- as.data.frame(.Object@dataObj)
      .Object@dataObj[1] <- as.character(.Object@dataObj[,1])
    }

    .Object@dataObj[,3:ncol(.Object@dataObj)] <- apply(.Object@dataObj[,3:ncol(.Object@dataObj),drop=FALSE], 2, function(x) { as.numeric(as.character(x))})

    nrRows <- length(unique(.Object@dataObj[,1]))
    nrCols <- length(.Object@tableContent)
    plotObj <- list()
    m <- matrix(NA, nrow=nrRows, ncol=nrCols)
    rownames(m) <- unique(.Object@dataObj[,1])

    vMin <- apply(.Object@dataObj[,3:ncol(.Object@dataObj)],2, min, na.rm=T)
    vMax <- apply(.Object@dataObj[,3:ncol(.Object@dataObj)],2, max, na.rm=T)
    allGroups <- unique(.Object@dataObj[,1])
    if(length(scaleByCol)==1)
      scaleByCol <- rep(scaleByCol,nrCols)
    for ( i in 1:nrCols) {
      plotObj[[i]] <- list()
      colIndex <- match(.Object@varType[i], colnames(.Object@dataObj))
      for ( j in 1:nrRows ) {
        fn <- paste(graphNames,i,"-",j, sep="")
        values <- (as.numeric(.Object@dataObj[.Object@dataObj[,1]==allGroups[j], colIndex]))
        if ( class(.Object@tableContent[[i]]) == "sparkline" )  {
          tmpObj <- newSparkLine(values=values)#, vMin=vMin[colIndex-2], vMax=vMax[colIndex-2])
          allColors(tmpObj) <- allColors(.Object@tableContent[[i]])
          pointWidth(tmpObj) <- pointWidth(.Object@tableContent[[i]])
          lineWidth(tmpObj) <- lineWidth(.Object@tableContent[[i]])
          showIQR(tmpObj) <- showIQR(.Object@tableContent[[i]])
          width(tmpObj) <- width(.Object@tableContent[[i]])
          height(tmpObj) <- height(.Object@tableContent[[i]])
          padding(tmpObj) <- padding(.Object@tableContent[[i]])
          if(scaleByCol[i]){
            tmpObj <- scaleSpark(tmpObj,vMin=vMin[colIndex-2],vMax=vMax[colIndex-2])
          }
          plotObj[[i]][[j]] <- tmpObj
          if(outputType=="tex"){
            export(plotObj[[i]][[j]], outputType='pdf', filename=fn)
            m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
          }else if(outputType=="html"){
            export(plotObj[[i]][[j]], outputType='png', filename=fn)
            m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in;" src="', fn, '.png">',sep="")
          }else if (outputType=="htmlsvg") {
            export(plotObj[[i]][[j]], outputType='svg', filename=fn)
            m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in;" src="', fn, '.svg">',sep="")
          } else stop("WTF happened now?")
        }else if ( class(.Object@tableContent[[i]]) == "sparkbar" )  {
          tmpObj <- newSparkBar(values=values)
          barCol(tmpObj) <- barCol(.Object@tableContent[[i]])
          bgCol(tmpObj) <- bgCol(.Object@tableContent[[i]])
          barSpacingPerc(tmpObj) <- barSpacingPerc(.Object@tableContent[[i]])
          width(tmpObj) <- width(.Object@tableContent[[i]])
          height(tmpObj) <- height(.Object@tableContent[[i]])
          padding(tmpObj) <- padding(.Object@tableContent[[i]])
          if(scaleByCol[i]){
            tmpObj <- scaleSpark(tmpObj,vMin=vMin[colIndex-2],vMax=vMax[colIndex-2])
          }
          plotObj[[i]][[j]] <- tmpObj
          if(outputType=="tex"){
            export(plotObj[[i]][[j]], outputType='pdf', filename=fn)
            m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
          }else if(outputType=="html"){
            export(plotObj[[i]][[j]], outputType='png', filename=fn)
            m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in;" src="', fn, '.png">',sep="")
          }else if(outputType=="htmlsvg"){
            export(plotObj[[i]][[j]], outputType='svg', filename=fn)
            m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in;" src="', fn, '.svg">',sep="")
          }else stop("WTF happened now?")
        }else if ( class(.Object@tableContent[[i]]) == "sparkbox" )  {
          tmpObj <- newSparkBox(values=values)
          boxCol(tmpObj) <- boxCol(.Object@tableContent[[i]])
          bgCol(tmpObj) <- bgCol(.Object@tableContent[[i]])
          boxLineWidth(tmpObj) <- boxLineWidth(.Object@tableContent[[i]])
          outCol(tmpObj) <- outCol(.Object@tableContent[[i]])
          width(tmpObj) <- width(.Object@tableContent[[i]])
          height(tmpObj) <- height(.Object@tableContent[[i]])
          padding(tmpObj) <- padding(.Object@tableContent[[i]])
          if(scaleByCol[i]){
            tmpObj <- scaleSpark(tmpObj,vMin=vMin[colIndex-2],vMax=vMax[colIndex-2])
          }
          plotObj[[i]][[j]] <- tmpObj
          if(outputType=="tex"){
            export(plotObj[[i]][[j]], outputType='pdf', filename=fn)
            m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
#              m[j,i] <- paste("\\includegraphics[height=1.4em]{", fn, "}",sep="")
          }else if(outputType=="html"){
            export(plotObj[[i]][[j]], outputType='png', filename=fn)
            m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in;" src="', fn, '.png">',sep="")
          }else if(outputType=="htmlsvg"){
            export(plotObj[[i]][[j]], outputType='svg', filename=fn)
            m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in;" src="', fn, '.svg">',sep="")
          }else stop("WTF happened now?")
        }else if ( class(.Object@tableContent[[i]]) == "function" )  {# user-defined function
          plotObj[[i]][[j]] <- .Object@tableContent[[i]](values)
          if(outputType=="tex")
            m[j,i] <- paste("$",plotObj[[i]][[j]],"$",sep= "")
          else
            m[j,i] <- paste("",plotObj[[i]][[j]],"",sep= "")
        }else if ( class(.Object@tableContent[[i]]) == "sparkhist" )  {
          tmpObj <- newSparkHist(values=values)
          barCol(tmpObj) <- barCol(.Object@tableContent[[i]])
          bgCol(tmpObj) <- bgCol(.Object@tableContent[[i]])
          barSpacingPerc(tmpObj) <- barSpacingPerc(.Object@tableContent[[i]])
          width(tmpObj) <- width(.Object@tableContent[[i]])
          height(tmpObj) <- height(.Object@tableContent[[i]])
          padding(tmpObj) <- padding(.Object@tableContent[[i]])
          if(scaleByCol[i]){
            tmpObj <- scaleSpark(tmpObj,vMin=vMin[colIndex-2],vMax=vMax[colIndex-2])
          }
          plotObj[[i]][[j]] <- tmpObj
          if(outputType=="tex"){
            export(plotObj[[i]][[j]], outputType='pdf', filename=fn)
            m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
          }else if(outputType=="html"){
            export(plotObj[[i]][[j]], outputType='png', filename=fn)
            m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in;" src="', fn, '.png">',sep="")
          }else if(outputType=="htmlsvg"){
            export(plotObj[[i]][[j]], outputType='svg', filename=fn)
            m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in;" src="', fn, '.svg">',sep="")
          }else stop("WTF happened now?")
        }
        else stop("Something is wrong in the content object!?!?!\n")
      }
    }
    colnames(m) <- TH
    if(outputType=="tex"){
      outputMat <- m
      print(xT <- xtable(m), sanitize.text.function = function(x){x},comment=infonote)
      if(infonote){
        cat("\n\nInformation: please do not forget to add the following command before \\begin{document} in your tex-file:\n\n")
        cat('\\newcommand{\\graph}[3]{ \\raisebox{-#1mm}{\\includegraphics[height=#2em]{#3}}}\n\n')
      }
    }else if(outputType%in%c("html","htmlsvg")){
      outputMat <- m
      print(xT <- xtable(m), sanitize.text.function = function(x){x},type="html",comment=infonote)
    }else stop("WTF happened now?")
    if(!is.null(filename)){
      if(outputType%in%c("html","htmlsvg")&infonote)
        cat('<!--',filename,'was created.-->\n')
      else if(outputType=="tex"&infonote)
        cat('%',filename,'was created.\n')
      sink(filename)
      if(outputType%in%c("html","htmlsvg")){
        cat('<html><body>')
        print(xT <- xtable(m), sanitize.text.function = function(x){x},type="html",comment=infonote)
        cat('</body></html>')
      }else if(outputType=="tex"){
        cat('\\documentclass[12pt,landscape]{article}\n')
        cat('\\usepackage{graphicx} \n')
        cat('\\usepackage{lmodern} \n')
        cat('\\newcommand{\\graph}[3]{\n')
        cat('\\raisebox{-#1mm}{\\includegraphics[height=#2em]{#3}}\n')
        cat('}\n')
        cat('\\begin{document}\n')
        print(xT <- xtable(m), sanitize.text.function = function(x){x})
        cat('\\end{document}')
      }
      sink()
    }
    invisible(outputMat)
  }
)

setMethod(f='export', signature='geoTable',
  definition=function(object, outputType="html", filename=NULL, graphNames="out", transpose=FALSE, include.rownames=FALSE,
      include.colnames=FALSE,rownames=NULL,colnames=NULL,...) {
    print.names <- FALSE
    .Object <- object
    if ( !outputType %in% c("tex", "html","htmlsvg"))
      stop("please provide a valid output type!\n")
    if(!is.null(filename)){
      if (outputType %in% c("html","tex"))
        filename <- paste0(filename, ".", outputType)
      if (outputType=="htmlsvg")
        filename <- paste0(filename,".html")
    }
    TH <- names(.Object@tableContent)
    # to data.frame
    if(!is.list(.Object@dataObj))
      stop("Wrong input!\n")
    for(i in 1:length(.Object@dataObj)){
      if ( is.matrix(.Object@dataObj[[i]]) ) {
        .Object@dataObj[[i]] <- as.data.frame(.Object@dataObj[[i]])
        .Object@dataObj[[i]][,1] <- as.character(.Object@dataObj[[i]][,1])
      }
      if(attr(.Object@dataObj[[i]],"reshapeLong")$idvar!=.Object@geographicVar)
        .Object@dataObj[[i]]<-.Object@dataObj[[i]][,-which(names(.Object@dataObj[[i]])==.Object@geographicVar)]
      .Object@dataObj[[i]][,3:ncol(.Object@dataObj[[i]])] <- apply(.Object@dataObj[[i]][,3:ncol(.Object@dataObj[[i]]),drop=FALSE], 2, function(x) { as.numeric(as.character(x))})
    }

    nrRows <- length(unique(.Object@dataObj[[1]][,1]))#Rows for each state
    nrCols <- length(.Object@tableContent)#Cols for each state
    mGes <- list()

    for(st in 1:length(.Object@dataObj)){
      plotObj <- list()
      m <- matrix(NA, nrow=nrRows, ncol=nrCols)
      rownames(m) <- unique(.Object@dataObj[[st]][,1])

      tmp.fn <- function(x, min=TRUE) {
        if ( all(is.na(x)) ) {
          return(0)
        } else {
          if ( min ) {
            return(min(x, na.rm=TRUE))
          } else {
            return(max(x, na.rm=TRUE))
          }
        }
      }
      vMin <- apply(.Object@dataObj[[st]][,3:ncol(.Object@dataObj[[st]])],2, function(x) { tmp.fn(x, min=TRUE) })
      vMax <- apply(.Object@dataObj[[st]][,3:ncol(.Object@dataObj[[st]])],2, function(x) { tmp.fn(x, min=FALSE) })
      allGroups <- unique(.Object@dataObj[[st]][,1])

      for ( i in 1:nrCols) {
        plotObj[[i]] <- list()
        colIndex <- match(.Object@varType[i], colnames(.Object@dataObj[[st]]))
        for ( j in 1:nrRows ) {
          fn <- paste(graphNames,i,"-",j,"-",st, sep="")
          values <- (as.numeric(.Object@dataObj[[st]][.Object@dataObj[[st]][,1]==allGroups[j], colIndex]))
          if ( class(.Object@tableContent[[i]]) == "sparkline" )  {
            tmpObj <- newSparkLine(values=values, vMin=vMin[colIndex-2], vMax=vMax[colIndex-2])
            allColors(tmpObj) <- allColors(.Object@tableContent[[i]])
            pointWidth(tmpObj) <- pointWidth(.Object@tableContent[[i]])
            lineWidth(tmpObj) <- lineWidth(.Object@tableContent[[i]])
            showIQR(tmpObj) <- showIQR(.Object@tableContent[[i]])
            width(tmpObj) <- width(.Object@tableContent[[i]])
            height(tmpObj) <- height(.Object@tableContent[[i]])
            padding(tmpObj) <- padding(.Object@tableContent[[i]])
            plotObj[[i]][[j]] <- tmpObj
            if(outputType=="tex"){
              export(plotObj[[i]][[j]], outputType='pdf', filename=fn)
              m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
            }else if(outputType=="html"){
              export(plotObj[[i]][[j]], outputType='png', filename=fn)
              m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in" src="', fn, '.png">',sep="")
            }else if(outputType=="htmlsvg"){
              export(plotObj[[i]][[j]], outputType='svg', filename=fn)
              m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in" src="', fn, '.svg">',sep="")
            }else stop("WTF happened now?")
          }else if ( class(.Object@tableContent[[i]]) == "sparkbar" )  {
            tmpObj <- newSparkBar(values=values, vMin=vMin[colIndex-2], vMax=vMax[colIndex-2])
            barCol(tmpObj) <- barCol(.Object@tableContent[[i]])
            barSpacingPerc(tmpObj) <- barSpacingPerc(.Object@tableContent[[i]])
            width(tmpObj) <- width(.Object@tableContent[[i]])
            height(tmpObj) <- height(.Object@tableContent[[i]])
            padding(tmpObj) <- padding(.Object@tableContent[[i]])
            plotObj[[i]][[j]] <- tmpObj
            if(outputType=="tex"){
              export(plotObj[[i]][[j]], outputType='pdf', filename=fn)
              m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
            }else if(outputType=="html"){
              export(plotObj[[i]][[j]], outputType='png', filename=fn)
              m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in" src="', fn, '.png">',sep="")
            }else if(outputType=="htmlsvg"){
              export(plotObj[[i]][[j]], outputType='svg', filename=fn)
              m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in" src="', fn, '.svg">',sep="")
            }else stop("WTF happened now?")
          }else if ( class(.Object@tableContent[[i]]) == "sparkbox" )  {
            tmpObj <- newSparkBox(values=values, vMin=vMin[colIndex-2], vMax=vMax[colIndex-2])
            boxCol(tmpObj) <- boxCol(.Object@tableContent[[i]])
            boxLineWidth(tmpObj) <- boxLineWidth(.Object@tableContent[[i]])
            outCol(tmpObj) <- outCol(.Object@tableContent[[i]])
            width(tmpObj) <- width(.Object@tableContent[[i]])
            height(tmpObj) <- height(.Object@tableContent[[i]])
            padding(tmpObj) <- padding(.Object@tableContent[[i]])
            plotObj[[i]][[j]] <- tmpObj
            if(outputType=="tex"){
              export(plotObj[[i]][[j]], outputType='pdf', filename=fn)
              m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
            }else if(outputType=="html"){
              export(plotObj[[i]][[j]], outputType='png', filename=fn)
              m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in" src="', fn, '.png">',sep="")
            }else if(outputType=="htmlsvg"){
              export(plotObj[[i]][[j]], outputType='svg', filename=fn)
              m[j,i] <- paste('<img style="height:',plotObj[[i]][[j]]@height,'in" src="', fn, '.svg">',sep="")
            }else stop("WTF happened now?")
          }else  if ( class(.Object@tableContent[[i]]) == "function" )  {# user-defined function
            plotObj[[i]][[j]] <- .Object@tableContent[[i]](values)
            if(outputType=="tex")
              m[j,i] <- paste("$",plotObj[[i]][[j]],"$",sep= "")
            else
              m[j,i] <- paste("",plotObj[[i]][[j]],"",sep= "")
          }
          else stop("Something is wrong in the content object!?!?!\n")
        }
      }
      colnames(m) <- TH
      if(transpose)
        mGes[[names(.Object@dataObj)[st]]]<- t(m)
      else
        mGes[[names(.Object@dataObj)[st]]] <- m
    }
    GO <- .Object@geographicOrder
    GO$y <- max(GO$y) - GO$y +1
    a <- GO$y
    GO$y <- GO$x
    GO$x <- a
    outputIndex <- function(rowcol,rowContent=nrow(mGes[[1]]),colContent=ncol(mGes[[1]])){
      row <- rowcol[,1]
      col <- rowcol[,2]
      list(row=(1:rowContent)+(row-1)*rowContent,
          col=1:colContent+(col-1)*colContent)
    }
    M <- matrix("",nrow=max(GO[,2])*nrow(mGes[[1]]),ncol=max(GO[,1])*ncol(mGes[[1]]))
    for(g in 1:nrow(GO)){
      oI <- outputIndex(GO[g,1:2])
      M[oI$row,oI$col] <- mGes[[as.character(GO[g,3])]]
    }
    mm <- matrix("",ncol=ncol(M),nrow=nrow(M)+max(GO[,1]))
    IndexRowState <- 1+(0:(max(GO[,1])-1))*(nrow(mGes[[1]])+1)
    mm[c(-IndexRowState),] <- M
    skipIT <- vector()
    for(i in 1:length(IndexRowState)){
      skipIT <- rbind(skipIT,c(IndexRowState[i],1))
      for(j in 1:max(GO[,2])){
        skipIT <- rbind(skipIT,c(IndexRowState[i],2+(j-1)*ncol(mGes[[1]])))
        if(outputType=="tex")
          word <- paste("\\multicolumn{",ncol(mGes[[1]]),"}{|c|}{\\bf{",as.character(GO[GO[,1]==i&GO[,2]==j,3]),"}}",sep="")
        else
          word <- paste("<h2>",as.character(GO[GO[,1]==i&GO[,2]==j,3]),"</h2>",sep="")
        mm[IndexRowState[i],1+(j-1)*ncol(mGes[[1]])] <- word
      }
    }
    if(!transpose){
      changeIT <- as.matrix(expand.grid(IndexRowState,(1:ncol(mm))[1:ncol(mm)%in%unique(skipIT[,2])]))
      changeIT <- changeIT[changeIT[,2]!=1,]
      changeIT[,2] <- changeIT[,2]-1
      if(ncol(mGes[[1]])>1){
        moveRight <- changeIT
        moveRight[,1] <- 0
        moveRight[,2] <- 1
        skipIT <- vector()
        for(i in 1:(ncol(mGes[[1]])-1)){
          skipIT <- rbind(skipIT,round(changeIT+moveRight*i))
        }
      }
    }else{

      changeIT <- as.matrix(expand.grid(IndexRowState,seq(from=1,to=ncol(mm),by=ncol(mGes[[1]]))))
      skipIT <- NULL
      if(ncol(mGes[[1]])>1){
        moveRight <- changeIT
        moveRight[,1] <- 0
        moveRight[,2] <- 1
        skipIT <- vector()
        for(i in 1:(ncol(mGes[[1]])-1)){
          skipIT <- rbind(skipIT,round(changeIT+moveRight*i))
        }
      }
    }
#      if(nrow(skipIT)==0)
#        skipIT <- NULL
    if(nrow(changeIT)==0)
      changeIT <- NULL
    m <- mm
    hline <- unique(c(0,(1:max(GO[,1])*nrow(mGes[[1]]))+1:max(GO[,1])))
    outputMat <- m
    xT <- xtable(m)
    align(xT)[] <- "c"
    if(ncol(mGes[[1]])>1){
      align(xT)[seq(from=2,to=length(align(xT)),by=ncol(mGes[[1]]))] <- "|c"
      align(xT)[length(align(xT))] <- "c|"
    }else{
      align(xT)[] <- "|c"
      align(xT)[length(align(xT))] <- "|c|"
    }
    column.width <- ncol(mGes[[1]])
    if(is.null(rownames)){
      rownames <- rownames(mGes[[1]])
    }
    if(is.null(colnames)){
      colnames <- colnames(mGes[[1]])
    }

    if(outputType=="tex"){
      print.xtable2(xT, sanitize.text.function = function(x){x},hline.after=hline,include.rownames=include.rownames,column.width=column.width,
          include.colnames=include.colnames,skip.columns=skipIT,transpose=transpose,rownames=rownames,colnames=colnames,comment=)

      cat("\n\nInformation: please do not forget to add the following command before \\begin{document} in your tex-file:\n\n")
      cat('\\newcommand{\\graph}[3]{ \\raisebox{-#1mm}{\\includegraphics[height=#2em]{#3}}}\n\n')

    }else if(outputType%in%c("html","htmlsvg")){
      print.xtable2(xT, sanitize.text.function = function(x){x},type="html",include.rownames=include.rownames,
          include.colnames=include.colnames,skip.columns=skipIT,wider.columns=changeIT,column.width=column.width,
          hline.after=hline,transpose=transpose,rownames=rownames,colnames=colnames)
    }else stop("WTF happened now?")
    if(!is.null(filename)){
      if(outputType=="html")
        cat('<!--',filename,'was created.-->\n')
      else if(outputType=="tex")
        cat('%',filename,'was created.\n')
      sink(filename)
      if(outputType%in%c("html","htmlsvg")){
        cat('<html><head>
                <style type="text/css">
                table{border-color: #000;border-width: 0px 0px 0px 0px;border-style: solid; border-collapse: collapse;}
                .top {border-top:1px solid #000000;}
                .bottom{border-bottom:1px solid #000000;}
                .right{border-right:1px solid #000000;}
                .left{border-left:1px solid #000000;}
                </style></head><body>')
        print.xtable2(xT, sanitize.text.function = function(x){x},type="html",include.rownames=include.rownames,
            include.colnames=include.colnames,skip.columns=skipIT,wider.columns=changeIT,column.width=column.width,
            hline.after=hline,transpose=transpose,rownames=rownames,colnames=colnames)
        cat('</body></html>')
      }else if(outputType=="tex"){
        cat('\\documentclass[12pt,landscape]{article}\n')
        cat('\\usepackage{graphicx} \n')
        cat('\\usepackage{lmodern} \n')
        cat('\\newcommand{\\graph}[3]{\n')
        cat('\\raisebox{-#1mm}{\\includegraphics[height=#2em]{#3}}\n')
        cat('}\n')
        cat('\\begin{document}\n')
        print.xtable2(xT, sanitize.text.function = function(x){x},hline.after=hline,include.rownames=include.rownames,
            include.colnames=include.colnames,skip.columns=skipIT,rownames=rownames,colnames=colnames,column.width=column.width)
        cat('\\end{document}')
      }
      sink()
    }
    invisible(outputMat)
  }
)
