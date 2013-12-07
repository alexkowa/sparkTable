setGeneric("shiny_sparkTable",
  function(object, outputDir=tempdir(), ...) {
    require(shiny)
    standardGeneric("shiny_sparkTable")
  }
)
setMethod(
  f='shiny_sparkTable',
  signature="sparkTable",
  definition=function(object, outputDir=tempdir(), ...) {
    oldDir <- getwd()
    setwd(outputDir)
    
		dat <- object
		save(dat, file="data.rdata")
		
    dd <- file.path(outputDir,"www")
    if ( !file.exists(dd) ) {
      dir.create(dd)
    }

    cat(paste(readLines(system.file("shinystuff", "shinypara_global.R", package="sparkTable")), collapse="\n"), file="global.R")        
    cat(paste(readLines(system.file("shinystuff", "shinypara_server.R", package="sparkTable")), collapse="\n"), file="server.R")        
    cat(paste(readLines(system.file("shinystuff", "shinypara_ui.R", package="sparkTable")), collapse="\n"), file="ui.R")                      
		
    runApp(outputDir)
		setwd(oldDir)
  }
)
