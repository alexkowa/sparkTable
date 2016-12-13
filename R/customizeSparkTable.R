customizeSparkTable <- function(object, outputDir=getwd()) {
  appDir <- system.file("shinystuff", "customize", package="sparkTable")
  #dat <- tempdir <-  outputDir <- current_dir <- NULL
  if (appDir == "") {
    stop("Could not find shiny-app. Try re-installing `sparkTable`.", call.=FALSE)
  }
  if (!class(object)=="sparkTable") {
    stop("wrong input!\n")
  }
  if ( !file.exists(outputDir) ) {
    stop("outputDir does not exist!\n")
  }
  tmp <- tempdir()
  options(sparkTable.tempdir=file.path(tmp,"customize"))
  options(sparkTable.outputDir=outputDir)
  options(sparkTable.current_dir=getwd())
  options(sparkTable.dat=object)

  # copy customize-dir recursive to tempdir
  file.copy(appDir,tmp,recursive=TRUE)

  if (!"ui.R" %in%list.files(file.path(tmp,"customize"))) {
    stop(paste0("Problem while copying the Shiny-App to ",tmp,"\n"))
  }
  
  
  runApp(paste0(file.path(tmp,"customize")), display.mode="normal", launch.browser=TRUE)
}
