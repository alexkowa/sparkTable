setGeneric("customizeSparkTable",
  function(object, outputDir=getwd(), ...) {
    standardGeneric("customizeSparkTable")
  }
)
setMethod(
  f='customizeSparkTable',
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

    js <- file.path(dd,"/js")
    if ( !file.exists(js) ) {
      dir.create(js)
    }

    cat(paste(readLines(system.file("shinystuff", "www/sort.css", package="sparkTable")), collapse="\n"), file="www/sort.css")
    cat(paste(readLines(system.file("shinystuff", "www/js/jquery-ui.min.js", package="sparkTable")), collapse="\n"), file="www/js/jquery-ui.min.js")
    cat(paste(readLines(system.file("shinystuff", "www/js/sort.js", package="sparkTable")), collapse="\n"), file="www/js/sort.js")

    runApp(tempdir)
    setwd(outputDir)
  }
)
