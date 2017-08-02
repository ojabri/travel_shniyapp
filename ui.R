dashboardPage(
 skin = "green",
 #Header
 dashboardHeader(title = "Travel Expenses"),
 #Siderbar
 dashboardSidebar(
 imageOutput("flag", height = "128px"),
 selectInput("Country", label = h3("Select Country"), 
  choices = c("Overall", countries), selected = "Overall"),
 dateRangeInput(inputId = "DateRange", "Date Range"),
 sidebarMenu(
  id = "tabs",
  menuItem("Summary", tabName = "Summary", icon = icon("money")),
  menuItem("Cities", tabName = "Cities", icon = icon("building")),
  menuItem("Travel", tabName = "Travel", icon = icon("bus")),
  #menuItem("Steps", icon = icon("paw"), tabName = "Steps"),
  menuItem(actionButton("updateExpenses", "Update Expenses", icon = icon("refresh")))
 ),
 radioButtons(inputId = "Currency", "Currency", c("Dollars" = "Dollars", "Pounds"="Pounds")),
 radioButtons(inputId = "Distance", "Distance", c("Miles" = "miles", "KM"="distance"))
 ),
 #Body
 dashboardBody(
  tags$head( 
   tags$link(rel = "stylesheet", type = "text/css", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"),
   #tags$link(rel = "stylesheet", type = "text/css", href = "flag-icon.min"),
   includeScript("drill.js")
  ),
  tabItems(
   tabItem(tabName = "Summary",
    fluidRow(
     column(width = 8,
      box(width = NULL, status = "success", leafletOutput("mymap", height="600px")), #MAP
      #box(width = NULL, status = "success", plotOutput('spendTS')) #GRAPH
      box(width = NULL, status = "success", DT::dataTableOutput('cityBreakdown'))
     ),
     valueBoxOutput("totalSpend"),
     valueBoxOutput("daysAway"),
     valueBoxOutput("distTraveled1"),
     infoBoxOutput("activityCost",width = 2),
     infoBoxOutput("alcoholCost",width = 2),
     infoBoxOutput("foodCost",width = 2),
     infoBoxOutput("hostelCost",width = 2),
     infoBoxOutput("miscCost",width = 2),
     infoBoxOutput("travelCost",width = 2)
    )
   ),
   tabItem(tabName = "Cities",
    fluidRow(
     column(width = 8,
      box(width = NULL, status = "success", 
       uiOutput("cityControls")#, 
       #radioButtons("dailySelector", "By Day", c("Yes" = F, "No" = T), selected = T, inline = T),
       #radioButtons("cityFunctionAggregator", "Sum Or Average", c("Sum" = "sum", "Avg" = "avg"), selected = "sum", inline = T)
       
      ),
      box(width = NULL, status = "success",  withTags(h2("Cities")), DT::dataTableOutput('cityOutput')),
      #bsModal(id = "cityModal", title = "Data Table", trigger = "CitiesModalTrigger", size = "large", DT::dataTableOutput('cityOutput')),
      #box(width = NULL, status = "success", dygraphOutput('citySpendTS'))
      box(width = NULL, uiOutput("cityImages"))
     ),
     column(width = 4,
      box(width = NULL, box(width = NULL, status = "success", leafletOutput("cityMap", height="400px"))),
      box(width = NULL, status = "success",  withTags(h2("Notes")), DT::dataTableOutput('notes'))
     )
    )
   )
  )
 )
)


