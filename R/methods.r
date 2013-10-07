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
      if ( is.null(vMin))
        vMin <- min(.Object@values, na.rm=TRUE)
      if ( is.null(vMax))
        vMax <- max(.Object@values, na.rm=TRUE)			
      
      #if ( vMin > min(.Object@values, na.rm=TRUE) )
      #  stop()
      #if ( vMax < max(.Object@values, na.rm=TRUE) )
      #  stop()			
      
      len <- length(.Object@values)
      .Object@availableWidth <- .Object@width - .Object@width*((.Object@padding[3])/100) - .Object@width*((.Object@padding[4])/100)
      .Object@availableHeight <- .Object@height - .Object@height*((.Object@padding[1])/100) - .Object@height*((.Object@padding[2])/100)
      .Object@stepWidth <- (.Object@availableWidth*1.0) / (len-1)	
      .Object@coordsX <- (0:(len-1))*.Object@stepWidth + .Object@width*(.Object@padding[3]/100) 	
      
      v <- .Object@values
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
      return(.Object)
    }
)

setMethod(
    f="scaleSpark",
    signature="sparkbox",
    def=function(.Object, vMin, vMax) {
      if ( is.null(vMin))
        vMin <- min(.Object@values, na.rm=TRUE)
      if ( is.null(vMax))
        vMax <- max(.Object@values, na.rm=TRUE)			
      
      if ( vMin > min(.Object@values, na.rm=TRUE) )
        stop()
      if ( vMax < max(.Object@values, na.rm=TRUE) )
        stop()	
      
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
      if ( is.null(vMin))
        vMin <- min(.Object@values, na.rm=TRUE)
      if ( is.null(vMax))
        vMax <- max(.Object@values, na.rm=TRUE)			
      
      if ( vMin > min(.Object@values, na.rm=TRUE) )
        stop()
      if ( vMax < max(.Object@values, na.rm=TRUE) )
        stop()	
      
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


### set/replace slots 'barCol', 'barWidth' and 'barSpacingPerc' 
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
setGeneric("plotSparks", function(object, outputType="pdf", filename="testSpark", ...) { standardGeneric("plotSparks")} )
setMethod(
    f='plotSparks',
    signature='sparkline',
    definition=function(object, outputType="pdf", filename="testSpark", ...) {
      .Object <- object
      if ( !outputType %in% c("pdf","eps","png") )
        stop("please provide a valid output type!\n")
      filename <- paste(filename, ".", outputType, sep="")
      if ( outputType == "pdf" )
        pdf(filename, width=.Object@width, height=.Object@height)
      else if ( outputType == "eps")
        postscript(filename, width=.Object@width, height=.Object@height, paper='special')
      else if ( outputType == "png" ){
        #bitmap(filename, width=.Object@width, height=.Object@height,units="in",res=86,type="png16m",taa=1,gaa=1)
        if(Sys.info()[1]!="Windows")
          CairoPNG(filename, width=.Object@width, height=.Object@height,units="in",dpi=100)
        else
          png(filename ,width=.Object@width, height=.Object@height,units="in",res=100)
      }
      grid.rect(width=unit(.Object@width, "inches"), height=unit(.Object@width, "inches"), name="frame", draw=FALSE)
      
      # IQR
      if ( .Object@showIQR==TRUE ) {
        grid.rect(
            x=unit(0, "inches"),
            y=unit(quantile(.Object@coordsY, 0.25), "inches"),
            width=unit(.Object@width, "inches"), 
            height=unit(quantile(.Object@coordsY, 0.75,na.rm=TRUE)-quantile(.Object@coordsY, 0.25,na.rm=TRUE), "inches"), just=c("left","bottom"), gp=gpar(fill=.Object@allColors[6]))
      }				
      
      # plot lines
      grid.lines(unit(.Object@coordsX,"inches"), unit(.Object@coordsY,"inches"), gp=gpar(lwd=.Object@lineWidth, col=.Object@allColors[5]))
      
      # minimum
      if ( !is.na(.Object@allColors[1]) ) {
        minIndex <- max(which(.Object@coordsY==min(na.omit(.Object@coordsY))))
        grid.points(unit(.Object@coordsX[minIndex],"inches"), unit(.Object@coordsY[minIndex],"inches"), size=unit((.Object@pointWidth/100)*.Object@availableWidth, "inches"), gp=gpar(col=.Object@allColors[1], fill=.Object@allColors[1]), pch=19)
      }
      
      # maximum
      if ( !is.na(.Object@allColors[2]) ) {
        maxIndex <- max(which(.Object@coordsY==max(na.omit(.Object@coordsY))))
        grid.points(unit(.Object@coordsX[maxIndex],"inches"), unit(.Object@coordsY[maxIndex],"inches"), size=unit((.Object@pointWidth/100)*.Object@availableWidth, "inches"), gp=gpar(col=.Object@allColors[2]), pch=19)
      }
      
      # last
      if ( !is.na(.Object@allColors[3]) ) {
        lastIndex <- length(.Object@coordsY)
        grid.points(unit(.Object@coordsX[lastIndex],"inches"), unit(.Object@coordsY[lastIndex],"inches"), size=unit((.Object@pointWidth/100)*.Object@availableWidth, "inches"), gp=gpar(col=.Object@allColors[3]), pch=19)
      }
      dev.off()
    }
)

setMethod(
    f='plotSparks',
    signature='sparkbar',
    definition=function(object, outputType="pdf", filename="testSpark", ...) {
      .Object <- object
      if ( !outputType %in% c("pdf","eps","png") )
        stop("please provide a valid output type!\n")
      filename <- paste(filename, ".", outputType, sep="")
      if ( outputType == "pdf" )
        pdf(filename, width=.Object@width, height=.Object@height)
      else if ( outputType == "eps")
        postscript(filename, width=.Object@width, height=.Object@height, paper='special')
      else if ( outputType == "png" ){
        #bitmap(filename, width=.Object@width, height=.Object@height,units="in",res=86,type="png16m",taa=1,gaa=1)
        if(Sys.info()[1]!="Windows")
          CairoPNG(filename, width=.Object@width, height=.Object@height,units="in",dpi=100)
        else
          png(filename ,width=.Object@width, height=.Object@height,units="in",res=100)
      }
      
      grid.rect(width=unit(.Object@width, "inches"), height=unit(.Object@width, "inches"), name="frame", draw=FALSE)
      
      # default: positive and negative values
      yStart <- .Object@height/2	
      # case 1: all values >= 0
      if ( all(.Object@values <= 0,na.rm=TRUE) )
        yStart <- .Object@height - ((.Object@height - .Object@availableHeight) / 2)
      # case 2: all values <= 0
      if ( all(.Object@values >= 0,na.rm=TRUE) )
        yStart <- ((.Object@height - .Object@availableHeight) / 2)
      
      for ( i in 1:length(.Object@coordsX)) {
        if(is.na(.Object@coordsY[i]))
          .Object@coordsY[i] <- 0
        if ( .Object@coordsY[i] < 0 ) {
          grid.rect(
              x=unit(.Object@coordsX[i], "inches"), 
              y=unit(yStart, "inches"), 
              width=unit(.Object@barWidth, "inches"),
              height=unit(.Object@coordsY[i], "inches"),		
              gp=gpar(col=.Object@barCol[3],fill=.Object@barCol[1]),just=c("left","bottom"))			
        }
        else {
          grid.rect(
              x=unit(.Object@coordsX[i], "inches"), 
              y=unit(yStart, "inches"), 
              width=unit(.Object@barWidth, "inches"),
              height=unit(.Object@coordsY[i], "inches"),		
              gp=gpar(col=.Object@barCol[3],fill=.Object@barCol[2]), just=c("left","bottom"))			
        }
        #grid.lines(
        #	x=unit(c(.Object@coordsX[1], .Object@coordsX[length(.Object@coordsX)]+.Object@barWidth), "inches"), 
        #	y=unit(yStart, "inches"),
        #	gp=gpar(lwd=1, col=.Object@barCol[3], lineend="square"))	
      }
      dev.off()
    }
)

setMethod(
    f='plotSparks',
    signature='sparkhist',
    definition=function(object, outputType="pdf", filename="testSpark", ...) {
      .Object <- object
      vals <- hist(.Object@values,plot=FALSE)$counts
      if ( !outputType %in% c("pdf","eps","png") )
        stop("please provide a valid output type!\n")
      filename <- paste(filename, ".", outputType, sep="")
      if ( outputType == "pdf" )
        pdf(filename, width=.Object@width, height=.Object@height)
      else if ( outputType == "eps")
        postscript(filename, width=.Object@width, height=.Object@height, paper='special')
      else if ( outputType == "png" ){
        #bitmap(filename, width=.Object@width, height=.Object@height,units="in",res=86,type="png16m",taa=1,gaa=1)
        if(Sys.info()[1]!="Windows")
          CairoPNG(filename, width=.Object@width, height=.Object@height,units="in",dpi=100)
        else
          png(filename ,width=.Object@width, height=.Object@height,units="in",res=100)
      }
      
      grid.rect(width=unit(.Object@width, "inches"), height=unit(.Object@width, "inches"), name="frame", draw=FALSE)
      
      # default: positive and negative values
      yStart <- .Object@height/2	
      # case 1: all values >= 0
      if ( all(vals <= 0,na.rm=TRUE) )
        yStart <- .Object@height - ((.Object@height - .Object@availableHeight) / 2)
      # case 2: all values <= 0
      if ( all(vals >= 0,na.rm=TRUE) )
        yStart <- ((.Object@height - .Object@availableHeight) / 2)
      
      for ( i in 1:length(.Object@coordsX)) {
        if(is.na(.Object@coordsY[i]))
          .Object@coordsY[i] <- 0
        if ( .Object@coordsY[i] < 0 ) {
          grid.rect(
              x=unit(.Object@coordsX[i], "inches"), 
              y=unit(yStart, "inches"), 
              width=unit(.Object@barWidth, "inches"),
              height=unit(.Object@coordsY[i], "inches"),		
              gp=gpar(col=.Object@barCol[3],fill=.Object@barCol[1]),just=c("left","bottom"))			
        }
        else {
          grid.rect(
              x=unit(.Object@coordsX[i], "inches"), 
              y=unit(yStart, "inches"), 
              width=unit(.Object@barWidth, "inches"),
              height=unit(.Object@coordsY[i], "inches"),		
              gp=gpar(col=.Object@barCol[3],fill=.Object@barCol[2]), just=c("left","bottom"))			
        }
        #grid.lines(
        #	x=unit(c(.Object@coordsX[1], .Object@coordsX[length(.Object@coordsX)]+.Object@barWidth), "inches"), 
        #	y=unit(yStart, "inches"),
        #	gp=gpar(lwd=1, col=.Object@barCol[3], lineend="square"))	
      }
      dev.off()
    }
)

setMethod(
    f='plotSparks',
    signature='sparkbox',
    definition=function(object, outputType="pdf", filename="testSpark", ...) {
      .Object <- object
      if ( !outputType %in% c("pdf","eps","png") )
        stop("please provide a valid output type!\n")
      filename <- paste(filename, ".", outputType, sep="")
      if ( outputType == "pdf" ){
        pdf(filename, width=.Object@width, height=.Object@height)
      }else if ( outputType == "eps"){
        postscript(filename, width=.Object@width, height=.Object@height, paper='special')
      }else if ( outputType == "png" ){
        #bitmap(filename, width=.Object@width, height=.Object@height,units="in",res=86,type="png16m",taa=1,gaa=1)
        if(Sys.info()[1]!="Windows")
          CairoPNG(filename, width=.Object@width, height=.Object@height,units="in",dpi=100)
        else
          png(filename ,width=.Object@width, height=.Object@height,units="in",res=100)
      }
      
      # parameter: 
      # percRemoveUp/Down
      down <- .1
      up <- 1-down
      
      paraEndLine1 <- 0.5 - ((up-down)/8)
      paraEndLine2 <- 0.5 + ((up-down)/8)		
      
      boxStats <- boxplot.stats(.Object@coordsX)	
      grid.rect(width=unit(.Object@width, "inches"), height=unit(.Object@height, "inches"), name="frame" , draw=FALSE)
      
      # main box
#      grid.rect(
#          x=unit(boxStats$stats[2], "inches"), 
#          y=unit(quantile(0:.Object@height, down), "inches"), 
#          width=unit(boxStats$stats[4]-boxStats$stats[2], "inches"),
#          height=unit(quantile(0:.Object@height, up)-quantile(0:.Object@height, down), "inches"),		
#          gp=gpar(lwd=.Object@boxLineWidth, fill=.Object@boxCol[2]), just=c("left", "bottom")
#      )		
      grid.rect(
          x=unit(boxStats$stats[2], "inches"), 
          y=unit(.Object@height* down, "inches"),
          width=unit(boxStats$stats[4]-boxStats$stats[2], "inches"),
          height=unit(.Object@height*up-.Object@height*down, "inches"),		
          gp=gpar(col=.Object@boxCol[1],lwd=.Object@boxLineWidth, fill=.Object@boxCol[2]), just=c("left", "bottom")
      )
      
      # whiskers
#      grid.lines(
#          x=unit(c(boxStats$stats[1], boxStats$stats[2]), "inches"), 
#          y=unit(quantile(0:.Object@height,0.5), "inches"),
#          gp=gpar(lwd=.Object@boxLineWidth, lty=1, col=.Object@boxCol[1], lineend="square")
#      )
      grid.lines(
          x=unit(c(boxStats$stats[1], boxStats$stats[2]), "inches"), 
          y=unit(.Object@height*0.5, "inches"),
          gp=gpar(lwd=.Object@boxLineWidth, lty=1, col=.Object@boxCol[1], lineend="square")
      )
      
#      grid.lines(
#          x=unit(c(boxStats$stats[4],boxStats$stats[5]), "inches"), 
#          y=unit(quantile(0:.Object@height,0.5), "inches"),
#          gp=gpar(lwd=.Object@boxLineWidth, lty=1, col=.Object@boxCol[1], lineend="square")
#      )
      grid.lines(
          x=unit(c(boxStats$stats[4],boxStats$stats[5]), "inches"), 
          y=unit(.Object@height*0.5, "inches"),
          gp=gpar(lwd=.Object@boxLineWidth, lty=1, col=.Object@boxCol[1], lineend="square")
      )
      
      grid.lines(
          x=unit(boxStats$stats[1], "inches"), 
          y=unit(c(.Object@height* paraEndLine1, .Object@height* paraEndLine2), "inches"),
          gp=gpar(lwd=.Object@boxLineWidth, lty=1, col=.Object@boxCol[1], lineend="square")
      )
      grid.lines(
          x=unit(boxStats$stats[5], "inches"), 
          y=unit(c(.Object@height* paraEndLine1, .Object@height*paraEndLine2), "inches"),
          gp=gpar(lwd=.Object@boxLineWidth, lty=1, col=.Object@boxCol[1], lineend="square")
      )
      
      # median
      # FIXME: absolute values - linejoin!!
      grid.lines(
          x=unit(boxStats$stats[3], "inches"), 
          y=unit(c(.Object@height*(down+0.002),.Object@height*( up-0.002)), "inches"),
          gp=gpar(lwd=.Object@boxLineWidth+4, lineend="butt", col=.Object@boxCol[1])
      )		
      
      # outliers
      if( length(boxStats$out) > 0 & !is.null(.Object@outCol) ) {			
        for( j in 1:length(boxStats$out) ) {
          indX <- match(boxStats$out[j], .Object@coordsX)
          grid.points(unit(.Object@coordsX[indX],"inches"), unit(.Object@height/2,"inches"),
              size=unit(.Object@height/10,"inches"), gp=gpar(col=.Object@outCol), pch=19)
        }	
      }		
      dev.off()
    }	
)
setGeneric("plotSparkTable", function(object, outputType="html", filename=NULL, graphNames="out",infonote=TRUE,scaleByCol=FALSE, ...) { standardGeneric("plotSparkTable")} )
setMethod(
    f='plotSparkTable',
    signature='sparkTable',
    definition=function(object, outputType="html", filename=NULL, graphNames="out",infonote=TRUE, scaleByCol=FALSE,...) {
      .Object <- object
      if ( !outputType %in% c("tex", "html") )
        stop("please provide a valid output type!\n")
      filename <- paste(filename, ".", outputType, sep="")
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
              plotSparks(plotObj[[i]][[j]], outputType='pdf', filename=fn)
              m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
            }else  if(outputType=="html"){
              plotSparks(plotObj[[i]][[j]], outputType='png', filename=fn)
              m[j,i] <- paste('<img src="', fn, '.png">',sep="")  
            }else stop("WTF happened now?")
          }else if ( class(.Object@tableContent[[i]]) == "sparkbar" )  {
            tmpObj <- newSparkBar(values=values)
            barCol(tmpObj) <- barCol(.Object@tableContent[[i]])
            barSpacingPerc(tmpObj) <- barSpacingPerc(.Object@tableContent[[i]])
            width(tmpObj) <- width(.Object@tableContent[[i]])
            height(tmpObj) <- height(.Object@tableContent[[i]])
            padding(tmpObj) <- padding(.Object@tableContent[[i]])
            if(scaleByCol[i]){
              tmpObj <- scaleSpark(tmpObj,vMin=vMin[colIndex-2],vMax=vMax[colIndex-2])
            }
            plotObj[[i]][[j]] <- tmpObj					
            if(outputType=="tex"){
              plotSparks(plotObj[[i]][[j]], outputType='pdf', filename=fn)
              m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
            }else if(outputType=="html"){
              plotSparks(plotObj[[i]][[j]], outputType='png', filename=fn)
              m[j,i] <- paste('<img src="', fn, '.png">',sep="")
            }else stop("WTF happened now?")
          }else if ( class(.Object@tableContent[[i]]) == "sparkbox" )  {
            tmpObj <- newSparkBox(values=values)
            boxCol(tmpObj) <- boxCol(.Object@tableContent[[i]])
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
              plotSparks(plotObj[[i]][[j]], outputType='pdf', filename=fn)
              m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
#              m[j,i] <- paste("\\includegraphics[height=1.4em]{", fn, "}",sep="")
            }else if(outputType=="html"){
              plotSparks(plotObj[[i]][[j]], outputType='png', filename=fn)
              m[j,i] <- paste('<img src="', fn, '.png">',sep="")
            }else stop("WTF happened now?")
          }else  if ( class(.Object@tableContent[[i]]) == "function" )  {# user-defined function
            plotObj[[i]][[j]] <- .Object@tableContent[[i]](values)
            if(outputType=="tex")
              m[j,i] <- paste("$",plotObj[[i]][[j]],"$",sep= "")
            else
              m[j,i] <- paste("",plotObj[[i]][[j]],"",sep= "")
          }else if ( class(.Object@tableContent[[i]]) == "sparkhist" )  {
            tmpObj <- newSparkHist(values=values)
            barCol(tmpObj) <- barCol(.Object@tableContent[[i]])
            barSpacingPerc(tmpObj) <- barSpacingPerc(.Object@tableContent[[i]])
            width(tmpObj) <- width(.Object@tableContent[[i]])
            height(tmpObj) <- height(.Object@tableContent[[i]])
            padding(tmpObj) <- padding(.Object@tableContent[[i]])
            if(scaleByCol[i]){
              tmpObj <- scaleSpark(tmpObj,vMin=vMin[colIndex-2],vMax=vMax[colIndex-2])
            }
            plotObj[[i]][[j]] <- tmpObj					
            if(outputType=="tex"){
              plotSparks(plotObj[[i]][[j]], outputType='pdf', filename=fn)
              m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
            }else if(outputType=="html"){
              plotSparks(plotObj[[i]][[j]], outputType='png', filename=fn)
              m[j,i] <- paste('<img src="', fn, '.png">',sep="")
            }else stop("WTF happened now?")
          }
          else stop("Something is wrong in the content object!?!?!\n")
        }
      }			
      colnames(m) <- TH
      if(outputType=="tex"){
        print(xT <- xtable(m), sanitize.text.function = function(x){x})
    if(infonote){  
		  cat("\n\nInformation: please do not forget to add the following command before \\begin{document} in your tex-file:\n\n")
		  cat('\\newcommand{\\graph}[3]{ \\raisebox{-#1mm}{\\includegraphics[height=#2em]{#3}}}\n\n')	
    }
	  }else if(outputType=="html"){
        print(xT <- xtable(m), sanitize.text.function = function(x){x},type="html")
      }else stop("WTF happened now?")
      if(!is.null(filename)){
        if(outputType=="html")
          cat('<!--',filename,'was created.-->\n')
        else if(outputType=="tex")
          cat('%',filename,'was created.\n')
        sink(filename)
        if(outputType=="html"){
          cat('<html><body>')
          print(xT <- xtable(m), sanitize.text.function = function(x){x},type="html")
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
    }	
)

setGeneric("plotGeoTable", function(object, outputType="html", filename=NULL, graphNames="out", transpose=FALSE, include.rownames=FALSE,include.colnames=FALSE,rownames=NULL,colnames=NULL,...) { standardGeneric("plotGeoTable")} )
setMethod(
    f='plotGeoTable',
    signature='geoTable',
    definition=function(object, outputType="html", filename=NULL, graphNames="out", transpose=FALSE, include.rownames=FALSE,include.colnames=FALSE,rownames=NULL,colnames=NULL,...) {
      print.names <- FALSE
      .Object <- object
      if ( !outputType %in% c("tex", "html") )
        stop("please provide a valid output type!\n")
      if(!is.null(filename))
        filename <- paste(filename, ".", outputType, sep="")
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
        
        vMin <- apply(.Object@dataObj[[st]][,3:ncol(.Object@dataObj[[st]])],2, min, na.rm=T)
        vMax <- apply(.Object@dataObj[[st]][,3:ncol(.Object@dataObj[[st]])],2, max, na.rm=T)
        
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
                plotSparks(plotObj[[i]][[j]], outputType='pdf', filename=fn)
                m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
              }else  if(outputType=="html"){
                plotSparks(plotObj[[i]][[j]], outputType='png', filename=fn)
                m[j,i] <- paste('<img src="', fn, '.png">',sep="") 
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
                plotSparks(plotObj[[i]][[j]], outputType='pdf', filename=fn)
                m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
              }else if(outputType=="html"){
                plotSparks(plotObj[[i]][[j]], outputType='png', filename=fn)
                m[j,i] <- paste('<img src="', fn, '.png">',sep="")
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
                plotSparks(plotObj[[i]][[j]], outputType='pdf', filename=fn)
                m[j,i] <- paste("\\graph{1}{1}{", fn, "}",sep="")
              }else if(outputType=="html"){
                plotSparks(plotObj[[i]][[j]], outputType='png', filename=fn)
                m[j,i] <- paste('<img src="', fn, '.png">',sep="")
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
            include.colnames=include.colnames,skip.columns=skipIT,transpose=transpose,rownames=rownames,colnames=colnames)
	
		cat("\n\nInformation: please do not forget to add the following command before \\begin{document} in your tex-file:\n\n")
		cat('\\newcommand{\\graph}[3]{ \\raisebox{-#1mm}{\\includegraphics[height=#2em]{#3}}}\n\n')	

	  }else if(outputType=="html"){
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
        if(outputType=="html"){
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
    }	
)
