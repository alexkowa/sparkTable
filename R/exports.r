### sparkTable summary
setGeneric("summaryST", function(.Object, outputType="html", filename=NULL, graphNames="out",
        hist=TRUE,boxplot=TRUE,min=TRUE,quantile=TRUE,median=TRUE,
        mean=TRUE,max=TRUE,changeOrder=NULL,addFun=NULL,digits=2,scaleHistByCol=FALSE,scaleBoxByCol=FALSE
    ) { standardGeneric("summaryST")} )
setMethod(
    f="summaryST",
    signature="data.frame",
    def=function(.Object, outputType="html", filename=NULL, graphNames="out",
        hist=TRUE,boxplot=TRUE,min=TRUE,quantile=TRUE,median=TRUE,
        mean=TRUE,max=TRUE,changeOrder=NULL,addFun=NULL,digits=2,scaleHistByCol=FALSE,scaleBoxByCol=FALSE) {
      classdf <- unlist(lapply(as.list(.Object),class))
      if(!all(classdf%in%c("integer","numeric"))){
        warning("At the moment only the numerical variables of a data frame are used.")
        .Object <- .Object[,classdf=="numeric"]
      }
      content <- list()
      if(min){
        content[[length(content)+1]] <- eval(parse(text=paste("function(x)round(min(x,na.rm=TRUE),digits=",digits,")")))
        names(content)[length(content)] <- "min"
      }
      if(quantile){
        content[[length(content)+1]] <- eval(parse(text=paste("function(x)round(quantile(x,.25,na.rm=TRUE),digits=",digits,")")))
        names(content)[length(content)] <- "1st_q"
      }
      if(median){
        content[[length(content)+1]] <- eval(parse(text=paste("function(x)round(median(x,na.rm=TRUE),digits=",digits,")")))
        names(content)[length(content)] <- "median"
      }
      if(mean){
        content[[length(content)+1]] <- eval(parse(text=paste("function(x)round(mean(x,na.rm=TRUE),digits=",digits,")")))
        names(content)[length(content)] <- "mean"
      }
      if(quantile){
        content[[length(content)+1]] <- eval(parse(text=paste("function(x)round(quantile(x,.75,na.rm=TRUE),digits=",digits,")")))
        names(content)[length(content)] <- "3rd_q"
      }
      if(max){
        content[[length(content)+1]] <- eval(parse(text=paste("function(x)round(max(x,na.rm=TRUE),digits=",digits,")")))
        names(content)[length(content)] <- "max"
      }
      if(hist){
        content[[length(content)+1]] <- newSparkHist()
        names(content)[length(content)] <- "hist"
      }
      if(boxplot){
        content[[length(content)+1]] <-  newSparkBox()
        names(content)[length(content)] <- "boxplot"
      }
      if(!is.null(addFun)){
        for(i in 1:length(addFun)){
          content[[length(content)+1]] <-  addFun[[i]]
          names(content)[length(content)] <- names(addFun)[i]
        }
      }
      if(!is.null(changeOrder)){
        co <- list()
        for(i in changeOrder)
          co[[length(co)+1]] <- content[[i]]
        nam <- names(content)
        content <- co
        names(content) <- nam[changeOrder]

      }
      df <- data.frame()
      for(i in 1:ncol(.Object)){
        df <- rbind(df,cbind(rep(colnames(.Object)[i],nrow(.Object)),.Object[,i]))
      }
      colnames(df) <- c("CAT","value")
      df <- reshapeExt(df, varying=list(2))
      varType <- rep("value",length(content))
      sparkTab <- newSparkTable(df, content, varType)
      spr <- rep(FALSE,length(content))
      if(scaleHistByCol)
        spr[names(content)=="hist"] <- TRUE
      if(scaleBoxByCol)
        spr[names(content)=="boxplot"] <- TRUE
      export(sparkTab, outputType=outputType, filename=filename, graphNames=graphNames,scaleByCol=spr)
      invisible(sparkTab)
    })

# allowes the user to specify a new sparkLine object
### default values for outputType tex here, html in classdefinitions
newSparkLine <- function(width=NULL, height=NULL, values=NULL, padding=NULL, allColors=NULL,
    pointWidth=NULL, lineWidth=NULL, showIQR=NULL, vMin=NULL, vMax=NULL,outputType="html") {
  x <- new('sparkline')
  if ( !is.null(width) ){
    width(x) <- width
  }else if(outputType=="tex"){
    width(x) <- 6/3
  }
  if ( !is.null(height) ){
    height(x) <- height
  }else if(outputType=="tex"){
    height(x) <- 2/3
  }
  if ( !is.null(values) )
    values(x) <- values
  if ( !is.null(padding) ){
    padding(x) <- padding
  }else if(outputType=="tex"){
    padding(x) <- c(2,2,2,2)
  }
  if ( !is.null(allColors) )
    allColors(x) <- allColors
  if ( !is.null(pointWidth) ){
    pointWidth(x) <- pointWidth
  }else if(outputType=="tex"){
    pointWidth(x) <- 4
  }
  if ( !is.null(lineWidth) ){
    lineWidth(x) <- lineWidth
  }else if(outputType=="tex"){
    lineWidth(x) <- 3
  }
  if ( !is.null(showIQR) )
    showIQR(x) <- showIQR
  x <- scaleSpark(x, vMin=vMin, vMax=vMax)
  x
}

newSparkBar <- function(width=NULL, height=NULL, values=NULL, padding=NULL, barCol=NULL, barWidth=NULL, barSpacingPerc=NULL,
    vMin=NULL, vMax=NULL,bgCol=NULL,outputType="html") {
  x <- new('sparkbar')
  if ( !is.null(width) ){
    width(x) <- width
  }else if(outputType=="tex"){
    width(x) <- 6
  }
  if ( !is.null(height) ){
    height(x) <- height
  }else if(outputType=="tex"){
    height(x) <- 2
  }
  if ( !is.null(values) )
    values(x) <- values
  if ( !is.null(padding) ){
    padding(x) <- padding
  }else if(outputType=="tex"){
    padding(x) <- c(2,2,2,2)
  }
  if ( !is.null(barCol) )
    barCol(x) <- barCol
  if ( !is.null(barWidth) ){
    barWidth(x) <- barWidth
  }else if(outputType=="tex"){
    barWidth(x) <- NULL
  }
  if ( !is.null(barSpacingPerc) ){
    barSpacingPerc(x) <- barSpacingPerc
  }else if(outputType=="tex"){
    barSpacingPerc(x) <- 15
  }
  if ( !is.null(bgCol) ){
    bgCol(x) <- bgCol
  }
  x <- scaleSpark(x, vMin=vMin, vMax=vMax)
  x
}

newSparkHist <- function(width=NULL, height=NULL, values=NULL, padding=NULL, barCol=NULL, barWidth=NULL, barSpacingPerc=NULL,
    vMin=NULL, vMax=NULL,bgCol=NULL,outputType="html") {
  x <- new('sparkhist')
  if ( !is.null(width) ){
    width(x) <- width
  }else if(outputType=="tex"){
    width(x) <- 6
  }
  if ( !is.null(height) ){
    height(x) <- height
  }else if(outputType=="tex"){
    height(x) <- 2
  }
  if ( !is.null(values) )
    values(x) <- values
  if ( !is.null(padding) ){
    padding(x) <- padding
  }else if(outputType=="tex"){
    padding(x) <- c(2,2,2,2)
  }
  if ( !is.null(barCol) )
    barCol(x) <- barCol
  if ( !is.null(barWidth) ){
    barWidth(x) <- barWidth
  }else if(outputType=="tex"){
    barWidth(x) <- NULL
  }
  if ( !is.null(barSpacingPerc) ){
    barSpacingPerc(x) <- barSpacingPerc
  }else if(outputType=="tex"){
    barSpacingPerc(x) <- 15
  }
  if ( !is.null(bgCol) ){
    bgCol(x) <- bgCol
  }
  x <- scaleSpark(x, vMin=vMin, vMax=vMax)
  x
}
newSparkBox <- function(width=NULL, height=NULL, values=NULL, padding=NULL, boxOutCol=NULL,
    boxMedCol=NULL, boxShowOut=NULL, boxCol=NULL, boxLineWidth=NULL,
    vMin=NULL, vMax=NULL,bgCol=NULL,outputType="html") {
  x <- new('sparkbox')
  if ( !is.null(width) ){
    width(x) <- width
  }else if(outputType=="tex"){
    width(x) <- 6
  }
  if ( !is.null(height) ){
    height(x) <- height
  }else if(outputType=="tex"){
    height(x) <- 2
  }
  if ( !is.null(values) )
    values(x) <- values
  if ( !is.null(padding) ){
    padding(x) <- padding
  }else if(outputType=="tex"){
    padding(x) <- c(2,2,2,2)
  }
  if ( !is.null(boxLineWidth) ){
    boxLineWidth(x) <- boxLineWidth
  }else if(outputType=="tex"){
    boxLineWidth(x) <- 3
  }
  if ( !is.null(boxOutCol) )
    boxOutCol(x) <- boxOutCol
  if ( !is.null(boxMedCol) )
    boxMedCol(x) <- boxMedCol
  if ( !is.null(boxShowOut) )
    boxShowOut(x) <- boxShowOut
  if ( !is.null(boxCol) )
    boxCol(x) <- boxCol
  if ( !is.null(bgCol) ){
    bgCol(x) <- bgCol
  }
  x <- scaleSpark(x, vMin=vMin, vMax=vMax)
  x
}

# use reshapeExt to transform data that are already in 'long'
# format and required attributes
# based on reshape from package 'stats'
reshapeExt <- function(
    data,
    timeValues = NULL,
    geographicVar=NULL,
    varying = NULL, v.names = NULL, timevar = "time",
    idvar = "id", ids = 1:NROW(data),
    drop = NULL, new.row.names = NULL,
    sep = ".",
    split = if (sep == "") {
          list(regexp = "[A-Za-z][0-9]", include = TRUE)
        } else {
          list(regexp = sep, include = FALSE, fixed = TRUE)}
  ){
    x <- data
  if(is.null(geographicVar)){
    dat <- reshape(x,direction="long",varying=varying,v.names=v.names,timevar=timevar,
        idvar=idvar,ids=ids, drop=drop,new.row.names=new.row.names,
        sep=sep,split=split)
    n1 <- (nrow(dat)/length(unique(dat[,1])))
    if(is.null(timeValues))
      timeValues <- 1:n1
    if(is.null(attr(dat,"reshapeLong"))){
      attr(dat,"reshapeLong") <- list(
          timevar=names(dat)[2],
          idvar=names(dat)[1]
      )
    }
    #dat[,attr(dat,"reshapeLong")[["timevar"]]] <- rep(timeValues,nrow(dat)/n1)
    valTime <- 1
    for(i in 1:NROW(dat)){
      if(i>1){
        if(dat[i,1]!=dat[i-1,1]){
          valTime <- 1
        }
      }
      dat[i,attr(dat,"reshapeLong")[["timevar"]]] <- valTime
      valTime <- valTime + 1
    }
  }else{
    dat <- list()
    for(co in unique(x[,geographicVar])){
      dat[[co]] <- reshape(x[x[,geographicVar]==co,],direction="long",varying=varying,v.names=v.names,timevar=timevar,
          idvar=idvar,ids=ids, drop=drop,new.row.names=new.row.names,
          sep=sep,split=split)
      n1 <- (nrow(dat[[co]])/length(unique(dat[[co]][,1])))
      if(is.null(timeValues))
        timeValues <- 1:n1
      if(is.null(attr(dat[[co]],"reshapeLong"))){
        attr(dat[[co]],"reshapeLong") <- list(
            timevar=names(dat[[co]])[2],
            idvar=names(dat[[co]])[1]
        )
      }
      dat[[co]][,attr(dat[[co]],"reshapeLong")[["timevar"]]] <- rep(timeValues,nrow(dat[[co]])/n1)
    }
  }
  dat
}

newSparkTable <- function(dataObj, tableContent, varType){#}, graphicPara) {
  x <- new('sparkTable')
  dataObj(x) <- dataObj
  tableContent(x) <- tableContent
  varType(x) <- varType
  #GraphicPara(x) <- graphicPara
  x
}

newGeoTable <- function(dataObj, tableContent, varType,geographicVar,geographicInfo=NULL){
  x <- new('geoTable')
  dataObj(x) <- dataObj
  tableContent(x) <- tableContent
  varType(x) <- varType
  geographicVar(x)  <- geographicVar
  if(!is.null(geographicInfo)){
    geographicInfo(x)  <- geographicInfo
    n <- ceiling(sqrt(nrow(geographicInfo)))
    ### start optimization
    addGrid <- 0 # >0 mehr Punkte als noetig
    dat2 <- geographicInfo[,2:3]
    gridpoints <- ceiling(sqrt(nrow(dat2)))+addGrid
    dat2[,1] <- (dat2[,1] - min(dat2[,1]))/(max(dat2[,1] - min(dat2[,1]))) * (gridpoints-1)
    dat2[,2] <- (dat2[,2] - min(dat2[,2]))/(max(dat2[,2] - min(dat2[,2]))) * (gridpoints-1)
    dat1 <- 1+as.matrix(expand.grid(0:(gridpoints-1),0:(gridpoints-1)))
    dat2 <- 1+dat2
    n1 <- nrow(dat1)
    n2 <- nrow(dat2)
    colnames(dat2) <- colnames(dat1)
    D <- as.matrix(dist(rbind(dat1,dat2)))[1:n1,(n1+1):((n1+n2))]

    f.obj <- as.vector(D) # Zeilenweise hintereinander haengen
    f.con <- matrix(0,nrow=n1+n2,ncol=n1*n2) # leere Nebenbedingungen
    for(i in 0:(n2-1)){
      f.con[i+1,i*n1+(1:n1)] <- 1 # Jede Beobachtung aus dat2 soll genau eine naechste aus dat1 bekommen
    }
    for(i in 0:(n1-1)){ #Spaltenbedingungen
      f.con[i+n2+1,seq(1,n1*n2-n1+1,by=n1)+i] <- 1 # Jede Beobachtung aus dat1 soll maximal einmal verwendet werden
    }
    f.dir <- c(rep("==",n2),rep("<=",n1))
    f.rhs <- c(rep(1, n1+n2))
    rglpk <- Rglpk_solve_LP(f.obj, f.con, f.dir, f.rhs, types = rep("B",n1*n2), max = FALSE,
        bounds = NULL, verbose = FALSE)
    sol <- matrix(rglpk$solution,byrow=TRUE,ncol=n1)
    sol1 <- sol
    ind <- apply(sol,1,which.max) # This row in dat2 corresponds to the row in dat1
    ##optimization like in a checkerplot
    dat1ord <- dat1[ind,]
    XY <- data.frame(dat1ord,as.character(geographicInfo[,1]))
    names(XY) <- c("x","y",geographicVar)
    geographicOrder(x)  <- XY[order(XY$x*10+XY$y),]
  }

  x
}

setParameter <- function(object, value, type) {
  if ( type == 'width' ) {
    width(object) <- value
  }
  if ( type == 'height' ) {
    height(object) <- value
  }
  if ( type == 'values' ) {
    values(object) <- value
  }
  if ( type == 'padding' ) {
    padding(object) <- value
  }
  if ( type == 'allColors' ) {
    allColors(object) <- value
  }
  if ( type == 'lineWidth' ) {
    lineWidth(object) <- value
  }
  if ( type == 'pointWidth' ) {
    pointWidth(object) <- value
  }
  if ( type == 'showIQR' ) {
    showIQR(object) <- value
  }
  # sparkbox-objects
  if ( type == 'boxCol' ) {
    boxCol(object) <- value
  }
  if ( type == 'outCol' ) {
    outCol(object) <- value
  }
  if ( type == 'boxLineWidth' ) {
    boxLineWidth(object) <- value
  }
  # sparkbar-objects
  if ( type == 'barCol' ) {
    barCol(object) <- value
  }
  if ( type == 'barSpacingPerc' ) {
    barSpacingPerc(object) <- value
  }
  # sparkbar,sparkbox, sparkhist  
  if ( type == 'bgCol' ) {
    bgCol(object) <- "value"
  }
  # sparkTable-objects
  if ( type == 'dataObj' ) {
    dataObj(object) <- value
  }
  if ( type == 'tableContent' ) {
    tableContent(object) <- value
  }
  if ( type == 'varType' ) {
    varType(object) <- value
  }
  # geoTable-objects
  if ( type == 'geographicVar' ) {
    geographicVar(object) <- value
  }
  if ( type == 'geographicInfo' ) {
    geographicInfo(object) <- value
  }
  if ( type == 'geographicOrder' ) {
    geographicOrder(object) <- value
  }
  object
}

getParameter <- function(object, type) {
  if ( type == 'width' ) {
    out <- width(object)
  }
  if ( type == 'height' ) {
    out <- height(object)
  }
  if ( type == 'values' ) {
    out <- values(object)
  }
  if ( type == 'padding' ) {
    out <- padding(object)
  }
  if ( type == 'allColors' ) {
    out <- allColors(object)
  }
  if ( type == 'lineWidth' ) {
    out <- lineWidth(object)
  }
  if ( type == 'pointWidth' ) {
    out <- pointWidth(object)
  }
  if ( type == 'showIQR' ) {
    out <- showIQR(object)
  }
  # sparkbox-objects
  if ( type == 'boxCol' ) {
    out <- boxCol(object)
  }
  if ( type == 'outCol' ) {
    out <- outCol(object)
  }
  if ( type == 'boxLineWidth' ) {
    out <- boxLineWidth(object)
  }
  # sparkbar-objects
  if ( type == 'barCol' ) {
    out <- barCol(object)
  }
  if ( type == 'barSpacingPerc' ) {
    out <- barSpacingPerc(object)
  }
  # sparkbar,sparkbox, sparkhist  
  if ( type == 'bgCol' ) {
    out <- bgCol(object)
  }
  
  # sparkTable-objects
  if ( type == 'dataObj' ) {
    out <- dataObj(object)
  }
  if ( type == 'tableContent' ) {
    out <- tableContent(object)
  }
  if ( type == 'varType' ) {
    out <- varType(object)
  }
  # geoTable-objects
  if ( type == 'geographicVar' ) {
    out <- geographicVar(object)
  }
  if ( type == 'geographicInfo' ) {
    out <- geographicInfo(object)
  }
  if ( type == 'geographicOrder' ) {
    out <- geographicOrder(object)
  }

  out
}

