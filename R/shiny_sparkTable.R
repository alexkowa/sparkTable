setGeneric("shiny_sparkTable",
  function(object, outputDir=getwd(), ...) {
    require(shiny)
    standardGeneric("shiny_sparkTable")
  }
)
setMethod(
  f='shiny_sparkTable',
  signature="sparkTable",
  definition=function(object, outputDir=getwd(), ...) {
		if ( !file.exists(outputDir) ) {
			stop("outputDir does not exist!\n")
		}
		tempdir <- tempdir()
    setwd(tempdir)
    
		dat <- object
		save(dat, file="data.rdata")
		
		dirs <- c(outputDir, tempdir)
		
		save(dirs, file="dirs.rdata")
		
    dd <- file.path(tempdir,"www")
    if ( !file.exists(dd) ) {
      dir.create(dd)
    }

    cat(paste(readLines(system.file("shinystuff", "shinypara_global.R", package="sparkTable")), collapse="\n"), file="global.R")        
    cat(paste(readLines(system.file("shinystuff", "shinypara_server.R", package="sparkTable")), collapse="\n"), file="server.R")        
    cat(paste(readLines(system.file("shinystuff", "shinypara_ui.R", package="sparkTable")), collapse="\n"), file="ui.R")                      
		
    runApp(tempdir)
		setwd(outputDir)
  }
)
