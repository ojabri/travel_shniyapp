 ###########
 #REACTIVES#
 ###########
 expenses <- eventReactive(input$updateExpenses , {
   expenses = import(expensesURL, format="csv")
   exchange = import(exchangeURL, format="csv")

   expenses = prepData(expenses, "%d/%m/%Y")
   names(exchange) = make.names(names(exchange))
   exchange$Date = as.Date(as.character(exchange$Date), format =  "%d/%m/%Y")
  
   expenses = spreadMultiDayExpenses(expenses)
   expenses = convertCurrency(expenses, exchange)
   
   expenses$Exclude[is.na(expenses$Exclude)] = F
   expenses$Who[expenses$Who==""] = "Both"
   
  expenses
 }, ignoreNULL = FALSE)

 inputCountryBreakdown <- reactive({
 
  if(input$Country!="Overall"){
   x = subset(expenses(), Country==input$Country)
  } else {
   x = expenses() #subset(expenses(), Check.In >= input$DateRange[1] & Check.In <= input$DateRange[2] )
  }
  if(input$DateRange[1] == input$DateRange[2]){
   updateDateRangeInput(session, "DateRange", start = min(x$Check.In), end = max(x$Check.In))
  }
  x = subset(x, Check.In >= input$DateRange[1] & Check.In <= input$DateRange[2] )
  #Remove International Flights
  x[!x$Exclude,]
  
 })
 
 inputCountryDist <- reactive({
  if(input$Country!="Overall"){
   x = subset(dest, Country==input$Country)
  } else {
   x = dest
  }
 })
 
 inputCountry <- reactive({
  input$Country
 })

 inputCity <- reactive({
  input$cities
 })
 
 expenseBreakdown <- reactive({
  x=inputCountryBreakdown()
  ddply(x, .(What.was.it.), function(x){ 
   data.frame(
    Amount  = sum(x[,inputCurrency()$currency])
   )}
  )
 })
  
 inputCurrency <- reactive({
  if(input$Currency == "Pounds") {currencySymbol = "£";currencyIcon = "gbp"}
  if(input$Currency == "Dollars") {currencySymbol = "$";currencyIcon = "usd"}
  
  list(currency = input$Currency, symbol = currencySymbol, icon = currencyIcon)
 })
 
 dailySelector <- reactive({
	 input$dailySelector
 })
 
 cityFunctionAggregator <- reactive({
	 input$cityFunctionAggregator
 })
 
 inputDistanceUnits <- reactive({
	 input$Distance
 })
    
 points <- eventReactive(input$Country, {
  if(input$Country!="Overall"){
   cities = subset(uniqueCitiesLocations, Country==input$Country)
   travel = subset(actualTravelData, Country==input$Country & Exclude==F)
  } else {
   cities = uniqueCitiesLocations
   travel = actualTravelData
  }
  list(cities = cities, travel = travel)
 }, ignoreNULL = FALSE)
 
 observe({
  if (is.null(input$goto)) return()

  isolate({
   geographyName <- input$goto$geographyName
   geography <- input$goto$geography
   
   if(geography == "Country" & geographyName!="Overall"){
    updateSelectInput(session, "Country", selected = geographyName)
   } else if(geography == "City" & geographyName=="Overall"){
    updateSelectInput(session, "Country", selected = geographyName)
   } else if(geography == "City" & geographyName!="Overall"){
    #toggleModal(session, "cityModal", toggle = "toggle")
    updateCheckboxGroupInput(session, "cities", selected  = geographyName)
    updateTabItems(session, "tabs", selected = "Cities")
   }
  })
 })
  