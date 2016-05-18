output$sparkplot <- renderDataTable({
  setwd("www")
  inputdata$tmpfile <- paste0(tempfile(),".rdata")
  tmpdat <- data()$sparkO

  save(tmpdat, file=inputdata$tmpfile)
  inputdata$fn <- paste("tab-",format(Sys.time(),"%Y%m%d-%H%M%S"), sep="")
  m <- export(data()$sparkO,outputType="html",filename=inputdata$fn ,graphNames="out", infonote=FALSE)
  setwd("../")
  cbind(rownames(m),m)
}, escape=FALSE)

output$plotcode <- renderUI({
  if (is.null(inputdata$tmpfile)) {
    return(NULL)
  }
  cmd <- paste0("load('",paste(unlist(strsplit(inputdata$tmpfile, "\\\\")), collapse="\\\\"),"')\n")
  cmd <- paste0(cmd, "export(tmpdat, outputType='html', filename='",inputdata$fn,"', graphNames='spark') # html\n")
  cmd <- paste0(cmd, "export(tmpdat, outputType='tex', filename='",inputdata$fn,"', graphNames='spark') # tex\n")
  cmd <- paste0(cmd, "###################\n")

  list(
    htmlTemplate("tpl_one_col.html", inp=h4("R-Code to reproduce the current graphical table")),
    htmlTemplate("tpl_one_col_pre.html", inp=cmd)
  )
})

output$ui_plot_export <- renderUI({
  btn1 <- actionButton("exporthtml", "Export to html", style="btn-primary")
  btn2 <- actionButton("exportlatex", "Export to latex", style="btn-success")

  list(
    dataTableOutput("sparkplot"),
    uiOutput("plotcode"),
    htmlTemplate("tpl_two_col.html", inp1=btn1, inp2=btn2))
})
