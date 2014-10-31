shinyUI(
  bootstrapPage(
    tags$head(
      tags$script(src = "js/jquery-ui.min.js"),
      tags$script(src='js/sort.js'),
      tags$link(rel='stylesheet', type='text/css', href='sort.css'),
      tags$link(rel='stylesheet', type='text/css', href='shared/slider/css/jquery.slider.min.css'),
      tags$script(src='shared/slider/js/jquery.slider.min.js')
    ),
    headerPanel("Interactively create a graphical table"),
    mainPanel(
      tabsetPanel(
        tabPanel("Import data",
          sidebarPanel(
             uiOutput("cgroups")
          ),
          dataTableOutput("origdata")
        ),
        tabPanel("Add/Delete rows/columns", uiOutput("modify.table")),
        tabPanel("Customize",
          uiOutput("opts.global")
        ),
        tabPanel("Sort/Re-order",
          uiOutput('sort_cols'),
          uiOutput('sort_rows'),
          verbatimTextOutput('showorder')
        ),
        tabPanel("sparkTable-Plot",
          dataTableOutput("sparkplot"),
          actionButton("exporthtml", "Export to html", style="btn-primary"),
          actionButton("exportlatex", "Export to latex", style="btn-success")
        )
      )
    )
  )
)
