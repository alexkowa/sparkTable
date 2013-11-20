setGeneric("runShinyApp",
    function(object, outputDir=tempdir(),outputType="html", filename=NULL,
        graphNames="out", ...) {
      standardGeneric("runShinyApp")
    } )
setMethod(
    f='runShinyApp',
    signature='sparkTable',
    definition=function(object,outputDir=tempdir(), outputType="html", filename=NULL,
        graphNames="out",...) {
      oldDir <- getwd()
      setwd(outputDir)
      dir.create(file.path(outputDir,"www"))
      setwd("www")
      m <- plotSparkTable(object, outputType=outputType,filename=filename,graphNames=graphNames,
          ...)
      setwd("..")
      save(m,file="data.RData")
      cat('
              library(shiny)
              shinyServer(function(input, output) {
              load("data.RData")
              output$mytable1 = renderDataTable({
              cbind(rownames(m),m)
              })
              })
              ',file="server.R")    
      cat('
              library(shiny)
              
              shinyUI(bootstrapPage(
              headerPanel("Powered by \'shiny\' and \'sparkTable\'"),
              mainPanel(
              tabsetPanel(
              tabPanel("sparkTable",
              dataTableOutput("mytable1"))
              )
              )
              ))
              ',file="ui.R")
      setwd(oldDir)
      runApp(outputDir)
      
    })
setMethod(
    f='runShinyApp',
    signature='data.frame',
    definition=function(object,outputDir=tempdir(), outputType="html", filename=NULL,
        graphNames="out",...) {
      oldDir <- getwd()
      setwd(outputDir)
      dir.create(file.path(outputDir,"www"))
      setwd("www")
      object <- summaryST(object,outputType=outputType,
          filename=filename,graphNames=graphNames,...)
      setwd("..")
      setwd(oldDir)
      runShinyApp(object)
    })
