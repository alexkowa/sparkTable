setGeneric("showSparkTable",
    function(object, outputDir=tempdir(),outputType="html", filename=NULL,
        graphNames="out", ...) {
      standardGeneric("showSparkTable")
    } )
setMethod(
    f='showSparkTable',
    signature='sparkTable',
    definition=function(object,outputDir=tempdir(), outputType="html", filename=NULL,
        graphNames="out",...) {
      oldDir <- getwd()
      setwd(outputDir)
      dir.create(file.path(outputDir,"www"))
      setwd("www")
      m <- export(object, outputType=outputType,filename=filename,graphNames=graphNames,
          ...)
      setwd("..")
      save(m,file="data.RData")
      cat('
              library(shiny)
              shinyServer(function(input, output) {
                load("data.RData")
                output$mytable1 = renderDataTable({
                cbind(rownames(m),m)
                }, escape=FALSE)
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
    f='showSparkTable',
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
      showSparkTable(object)
    })

#setMethod(
#    f='showSparkTable',
#    signature='geoTable',
#    definition=function(object,outputDir=tempdir(), outputType="html", filename=NULL,
#        graphNames="out",...) {
#      oldDir <- getwd()
#      setwd(outputDir)
#      dir.create(file.path(outputDir,"www"))
#      setwd("www")
#      m <- export(object, outputType=outputType,filename=filename,graphNames=graphNames,
#          ...)
#      setwd("..")
#      print(m)
#      save(m,file="data.RData")
#      cat('
#              library(shiny)
#              shinyServer(function(input, output) {
#              load("data.RData")
#              output$mytable1 = renderDataTable({
#              cbind(rownames(m),m)
#              })
#              })
#              ',file="server.R")
#      cat('
#              library(shiny)
#
#              shinyUI(bootstrapPage(
#              headerPanel("Powered by \'shiny\' and \'sparkTable\'"),
#              mainPanel(
#              tabsetPanel(
#              tabPanel("sparkTable",
#              dataTableOutput("mytable1"))
#              )
#              )
#              ))
#              ',file="ui.R")
#      setwd(oldDir)
#      runApp(outputDir)
#    })
####Example geotable - EU population and debt
#data(popEU,package="sparkTable")
#data(debtEU,package="sparkTable")
#data(coordsEU,package="sparkTable")
#popEU <- popEU[popEU$country%in%coordsEU$country,]
#debtEU <- debtEU[debtEU$country%in%coordsEU$country,]
#EU <- cbind(popEU,debtEU[,-1])
#EUlong <- reshapeExt(EU,idvar="country",v.names=c("pop","debt"),
#    varying=list(2:13,14:25),geographicVar="country",timeValues=1999:2010)
#l <- newSparkLine()
#l <- setParameter(l, 'lineWidth', 2.5)
#content <- list(function(x){"Population:"},l,function(x){"Debt:"},l)
#varType <- c(rep("pop",2),rep("debt",2))
#xGeoEU <- newGeoTable(EUlong, content, varType,geographicVar="country",
#    geographicInfo=coordsEU)
#showSparkTable(xGeoEU)
