shinyUI(
  bootstrapPage(
    headerPanel("plotSparkTable using shiny..."),
    mainPanel(
      tabsetPanel(
        tabPanel("importData",
					sidebarPanel(
  					 uiOutput("cgroups")
					),		
					dataTableOutput("origdata")
				),
				tabPanel("modify table", uiOutput("modify.table")),				
				tabPanel("global options",
					uiOutput("opts.global")						
				),
				tabPanel("sparkTable-Plot", 
					dataTableOutput("sparkplot"),
					actionButton("exporthtml", "Export to html", style="btn-primary"),
					actionButton("exportlatex", "Export to latex", style="btn-success")
				),
				tabPanel("sortable",
					tags$head(tags$script(src = "js/jquery-ui.min.js")),
      		wellPanel(
        		uiOutput('sortable_rui')
      		),
    			mainPanel(
	  				tableOutput('showData'),
	  				verbatimTextOutput('showorder')
    			)					
				)
      )
    )
  )
)
