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
				tabPanel("sparkTable-Plot", dataTableOutput("sparkplot"))#,
        #tabPanel("gridster",  
        #gridster(width = 10, height = 10, marginx=10, marginy=10,
				#	 gridsterItem(col = 1, row = 1, sizex = 1, sizey = 1,
				#	 	textOutput("A")			
				#	 ),
				#	 gridsterItem(col = 2, row = 1, sizex = 1, sizey = 1,
				#	  textOutput("B")			
				#	 ),
				#	 gridsterItem(col = 1, row = 2, sizex = 1, sizey = 1,
				#	  textOutput("C")				
				#	 ),
 				#	 gridsterItem(col = 2, row = 2, sizex = 1, sizey = 1,
 				#	 	textOutput("D")					 						 
				#	 )
        #	)
        #)
      )
    )
  )
)
