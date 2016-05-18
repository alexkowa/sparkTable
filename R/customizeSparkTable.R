customizeSparkTable <- function(object, outputDir=getwd()) {
  appDir <- system.file("shinystuff", "customize", package="sparkTable")
  dat <- tempdir <-  outputDir <- current_dir <- NULL
  if (appDir == "") {
    stop("Could not find shiny-app. Try re-installing `sparkTable`.", call.=FALSE)
  }
  if (!class(object)=="sparkTable") {
    stop("wrong input!\n")
  }
  if ( !file.exists(outputDir) ) {
    stop("outputDir does not exist!\n")
  }
  tmp <- gsub("\\\\","/", tempdir())
  assign("tempdir", tmp, envir = .GlobalEnv)
  assign("outputDir", outputDir, envir = .GlobalEnv)
  assign("current_dir", getwd(), envir = .GlobalEnv)
  assign("dat", object, envir = .GlobalEnv)

  # copy customize-dir recursive to tempdir
  if (tolower(Sys.info()["sysname"])=="windows") {
    cmd <- paste("xcopy", from=gsub("/","\\\\",appDir), to=gsub("/","\\\\",tmp),"/e /i /h /y")
  } else {
    cmd <- paste("cp -r", from=appDir, to=tmp)
  }
  system(cmd, show.output.on.console=FALSE)

  if (!"ui.R" %in%list.files(tmp)) {
    stop(paste0("Problem while copying the Shiny-App to ",tmp,"\n"))
  }

  on.exit(rm(tempdir, envir=.GlobalEnv))
  on.exit(rm(outputDir, envir=.GlobalEnv))
  on.exit(rm(dat, envir=.GlobalEnv))
  on.exit(setwd())

  runApp(paste0(tmp), display.mode="normal", launch.browser=TRUE)
}
