

tabPanel("MFI"
	fluidPage(
		titlePanel("Most Frequent Item"),
		sidebarLayout(
			sidebarPanel(
				siderInput(inputId = "top_level",label="top items", min=1,
								max=40, value=10), width =3),
				mainPanel(
					tabsetPanel(type="tabs",
								tabPanel("Relation Tablets", renderDataTable("ItemsetData")),
								tabPanel("MFI Graph", plotOutput("ItemsetGraph"))
								
								)
					)
			)
			)
	)

)