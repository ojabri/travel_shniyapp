shinyServer(function(input, output, session) {
 source("reactives.R", local = T,  encoding = 'UTF-8')
    
#############
###OUTPUTS###
#############
  
 #############
 #SUMMARY TAB#
 #############
 
 output$cityBreakdown <- renderDataTable({
  x=inputCountryBreakdown()
  geography = "City"
  if(length(unique(x$Country))>1) geography = "Country"
  currency = inputCurrency()$currency
  citySpend = ddply(x, geography, function(x){
   if(geography == "City"){ dayMetric = "hostel" } else { dayMetric = c("hostel", "travel") }  
   Days = dayCount(x, dayMetric )
   data.frame(
    Amount  = sum(x[,currency]),
    Days = Days,
    AvgDay = sum(x[,currency]) / Days,
    AvgFoodDay = sum(subset(x, What.was.it.=="food & drink")[,currency]) / Days,
    AvgHostel = sum(subset(x, What.was.it.=="hostel")[,currency]) / Days
   )
  })
  citySpend = citySpend[citySpend[,geography]!="",]
  citySpend[order(citySpend$Days, decreasing=T),]
  
  Days = dayCount(x)
  overall.row = data.frame(
   Country = "Overall",
   Amount  = sum(x[,currency]),
   Days = Days,
   AvgDay = sum(x[,currency]) / Days,
   AvgFoodDay = sum(subset(x, What.was.it.=="food & drink")[,currency]) / Days,
   AvgHostel = sum(subset(x, What.was.it.=="hostel")[,currency]) / Days
  )
  names(overall.row)[1] = geography
  
  citySpend = rbind(overall.row, citySpend)
  
  #add links to Country/City
  citySpend[, geography] = sapply(citySpend[, geography], function(x) as.character(actionLink("CitiesModalTrigger", label = x, class = "drillButton", "data-geography" = geography, "data-geographyName" = x)) )
 
  moneyCols = c("Amount", "AvgDay", "AvgFoodDay", "AvgHostel")
  citySpend[,moneyCols] = round(citySpend[,moneyCols])
  
  DT::datatable(citySpend, options = list(dom = 'tp', pageLength = 30), rownames = FALSE, escape = FALSE, selection = "none")  %>% 
         #formatRound(moneyCols, digits = 0) %>% 
         formatCurrency(moneyCols, currency = inputCurrency()$symbol)


 }, server = FALSE)
  
 output$mymap <- renderLeaflet({
  t = points()$travel
  pal = c("red", "green")#brewer.pal(max(length(persons),3),"Set3")
  
  map = leaflet() %>%
   addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
   addCircleMarkers(~lon, ~lat, radius = 5, color = "red", stroke = FALSE, fillOpacity = 0.25, popup=~City, data = points()$cities)
   #addPolylines(~lon, ~lat, data=subset(t, Flight==F), group = "Overland", color="green") %>%
   #addPolylines(~lon, ~lat, data=subset(t, Flight==T), group = "Flights", color="green")
   
  for(p in persons){ map = map %>% addPolylines(~lon, ~lat, data=subset(t, Who.route==p), group = p, color = pal[p==persons]) }
  map = map %>%   
  addLayersControl(
    #overlayGroups = c("Overland", "Flights", persons) ,
    overlayGroups = c(persons) ,
    options = layersControlOptions(collapsed = FALSE)
  )
  map
 })
   
 output$totalSpend <- renderValueBox({
  if(inputCountry() == "Overall"){ 
   x = sum(expenses()[,inputCurrency()$currency]) 
  } else { 
   x =  sum(inputCountryBreakdown()[,inputCurrency()$currency]) 
  }
  valueBox("Total Spent", pn(x, inputCurrency()$currency), icon = icon(inputCurrency()$icon), color="green")
 })
 
 #####################
 #Summary Value Boxes#
 #####################
 output$daysAway <- renderValueBox({
  valueBox("Days Away", length(unique(inputCountryBreakdown()$Check.In)), icon = icon("calendar"), color="green")
 })
 
 output$distTraveled1 <- renderValueBox({
  if(inputDistanceUnits() == "miles") {units = "Miles"} else {units = "KM"}
  distance = paste(pn(sum(inputCountryDist()[,inputDistanceUnits()])), units)
  valueBox("Distance Travelled", distance , icon = icon("road"), color="green")
 }) 
 
 output$distTraveled2 <- renderValueBox({
  if(inputDistanceUnits() == "miles") {units = "Miles"} else {units = "KM"}
  distance = paste(pn(sum(inputCountryDist()[,inputDistanceUnits()])), units)
  valueBox("Distance Travelled", distance , icon = icon("road"), color="green")
 })

 ####################
 #Summary Info Boxes#
 ####################
 {
  output$activityCost <- renderInfoBox({
   x=expenseBreakdown()
   x=subset(x, What.was.it.=="activity")[,"Amount"]
   infoBox("Activity", pn(x, inputCurrency()$currency), icon = icon("ticket"), color="green")
  })
  
  output$alcoholCost <- renderInfoBox({
   x=expenseBreakdown()
   x=subset(x, What.was.it.=="alcohol")[,"Amount"]
   infoBox("alcohol", pn(x, inputCurrency()$currency), icon = icon("glass"), color="green")
  })  
  
  output$foodCost <- renderInfoBox({
   x=expenseBreakdown()
   x=subset(x, What.was.it.=="food & drink")[,"Amount"]
   infoBox("Food & Drink", pn(x, inputCurrency()$currency), icon = icon("cutlery"), color="green")
  })
  
  output$hostelCost <- renderInfoBox({
   x=expenseBreakdown()
   x=subset(x, What.was.it.=="hostel")[,"Amount"]
   infoBox("Hostel", pn(x, inputCurrency()), icon = icon("bed"), color="green")
  })
  
  output$miscCost <- renderInfoBox({
   x=expenseBreakdown()
   x=subset(x, What.was.it.=="misc")[,"Amount"]
   infoBox("Misc", pn(x, inputCurrency()$currency), icon = icon("question-circle"), color="green")
  })
  
  output$travelCost <- renderInfoBox({
   x=expenseBreakdown()
   x=subset(x, What.was.it.=="travel")[,"Amount"]
   infoBox("Travel", pn(x, inputCurrency()$currency), icon = icon("bus"), color="green")
  })
 }
 
 #END SUMMARY#
 
 #######
 #TRAVL#
 #######
 
 output$distSummary <- renderDataTable({
  x=inputCountryDist()
  
  geography = "City"
  if(length(unique(x$Country))>1) geography = "Country"
  
  if(inputDistanceUnits() == "miles") {units = "Miles"} else {units = "KM"}
  distance = paste(pn(sum(inputCountryDist()[,inputDistanceUnits()])), units)
  
  o1 = list(dom = 'tp', paging = FALSE, searching = FALSE)
  if(geography=="Country"){
   busSpend = ddply(dest, .(Country, Type), function(x){ 
    data.frame(
     #Country = unique(x$Country),
     Total.Distance  = sum(x[,inputDistanceUnits()]),
     Total.Spend  = sum(x[,inputCurrency()$currency]),
     AvgPer100= sum(x[,inputCurrency()$currency]) / sum(x[,inputDistanceUnits()]) * 100
    )
   })
   busSpend[order(busSpend$AvgPer100, decreasing=T),]
   busSpend$Type = as.character(sapply(busSpend$Type, function(x){ if(x=="flight") as.character(icon("plane")) else if(x=="ferry") as.character(icon("ship")) else as.character(icon(x))  }))
   busSpend[,c("Total.Distance", "Total.Spend", "AvgPer100")] = round( busSpend[,c("Total.Distance", "Total.Spend", "AvgPer100")])
   t = datatable(busSpend, options = o1,  rownames = FALSE, escape = -2) %>% formatCurrency(c("Total.Spend", "AvgPer100"), inputCurrency()$symbol) %>% formatRound(c("Total.Distance", "Total.Spend", "AvgPer100"), 0)
  } else {
   busSpend = subset(dest, Country == unique(x$Country) )[,c("from", "to", "Type", inputCurrency()$currency, inputDistanceUnits())]
   busSpend[,c(c(inputCurrency()$currency, inputDistanceUnits()))] = round( busSpend[,c(c(inputCurrency()$currency, inputDistanceUnits()))])
   busSpend$Type = as.character(sapply(busSpend$Type, function(x){ if(x=="flight") as.character(icon("plane")) else if(x=="ferry") as.character(icon("ship")) else as.character(icon(x))  }))
   t = datatable(busSpend, options = o1,  rownames = FALSE, escape = -3) %>% formatCurrency(inputCurrency()$currency, inputCurrency()$symbol) %>% formatRound(c(inputCurrency()$currency, inputDistanceUnits()), 1)
  }
  return(t)
 })

 #END TRAVEL#
 
 ##########
 #CITY TAB#
 ##########
 
 #### CITY CHECKBOXES ####
 output$cityControls <- renderUI({
  cities <- unique(inputCountryBreakdown()$City)
  cities = cities[cities!=""]
  checkboxGroupInput("cities", "Choose Cities", cities, inline=T)
 })  
 
 #### TUMBLR IMAGES ####
 output$cityImages <- renderUI({
  photoURLs = cityImagesURLs()
  cityImageOutputList = lapply(1:length(photoURLs), function(x) {
   src = photoURLs[x]
   tags$div(tags$a(tags$img(src=src, class = "img-responsive"), class = "thumbnail", inline=T), class = "image-wrapper col-xs-12 col-sm-6 col-md-4 col-lg-3")
  })
  #print(str(cityImageOutputList))
  do.call(tagList, cityImageOutputList)
  return(cityImageOutputList)
 })
 
 output$cityOutput <- renderDataTable({
  x = inputCountryBreakdown()
  if(!is.null(inputCity())) x = subset(x, City %in% inputCity())
  
  #cols = c("City", "Check.In", "What.was.it.")
  #if(dailySelector()) cols = c("City", "What.was.it.")
  cols = c("City", "Check.In", "What.was.it.")
  aggFun = sum
  
  citySpendAmount = ddply(x, cols, function(x){
   #data.frame(Amount  = aggFun(aggregate(x[,inputCurrency()],list(x[,"Check.In"]),sum))) 
   data.frame(Amount  = aggFun(x[,inputCurrency()$currency])) 
  })
  citySpendAmountTotal = ddply(x, c("City", "Check.In"), function(x){
   data.frame(Total  = aggFun(x[,inputCurrency()$currency])) 
  })

  citySpendNotes = ddply(x, cols[cols!="What.was.it."], function(x){ data.frame("Biggest Expense" = paste(tail(x[order(x[,inputCurrency()$currency]), "Note"], 2), collapse = "; "))})
  #print(head(citySpendNotes))
   
  citySpendAmount = citySpendAmount[citySpendAmount[,"City"]!="",]
  decastFormula = paste(paste(cols[cols!="What.was.it."], collapse="+"), "~", "What.was.it.")
  citySpendAmount = cast(citySpendAmount, decastFormula  , value = "Amount", fill=0)
  
  citySpend = merge(citySpendAmount, citySpendNotes)
  citySpend = merge(citySpend, citySpendAmountTotal)
  citySpend[,unique(x$What.was.it.)] = round(citySpend[,unique(x$What.was.it.)], 2)
  
  DT::datatable(citySpend, options = list(dom = 'tp', pageLength = 15), rownames = FALSE, escape = FALSE, selection = "none")  %>% 
  #formatRound(unique(x$What.was.it.), digits = 0)  
  formatCurrency(c(unique(x$What.was.it.), "Total"), currency = inputCurrency()$symbol)
   
 }, server = FALSE)
 
 output$notes <- renderDataTable({
  x = inputCountryBreakdown()
  if(!is.null(inputCity())) x = subset(x, City %in% inputCity())
 
  DT::datatable(x[,c("City", "Check.In", "Note", inputCurrency())], options = list(dom = 'tp', pageLength = 15), rownames = FALSE, escape = FALSE, selection = "none")   
 }, server = FALSE)
 
 output$cityMap <- renderLeaflet({
  t = points()$travel
  leaflet() %>%
   addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
   addCircleMarkers(~lon, ~lat, radius = 5, color = "red", stroke = FALSE, fillOpacity = 0.25, popup=~City, data = points()$cities)
 }) 
  
 cityImagesURLs <- reactive({
  tags = inputCountry()
  if(!is.null(inputCity())) tags = inputCity()
  tumblrURLs = paste0("http://empanadalady.tumblr.com/api/read?", paste0("tagged=", tags))
  #print(str(tumblrURLs))
  
  all.URLs = c()
  for(tumblrURL in tumblrURLs){
   a =  xmlToList(xmlParse(tumblrURL))
   nPosts = length(a$posts) -1
  
   photoURLs = unlist(lapply(a$posts[1:nPosts], function(x){
    nPhotos = length(x$photoset)
    lapply(x$photoset[1:nPhotos], function(y) {y[[4]]$text})
   }), use.names = F)
   all.URLs = c(all.URLs, photoURLs)
  }
  all.URLs
 })
 
 #END CITY#
 
 #############
 #UI ELEMENTS#
 #############
 
 #### FLAG ####
 output$flag <- renderImage({
 #print(str(countryMeta))
 countryIndex = match(inputCountry(), countryMeta$Common.Name)
 p = ifelse (inputCountry() == "Overall", 
  normalizePath('flags/flag-icon-css-master/flags/4x3/us.svg'), 
  normalizePath(paste0('flags/flag-icon-css-master/flags/4x3/',tolower(countryMeta[countryIndex,"ISO.3166.1.2.Letter.Code"]),'.svg')))
 
 return(list(
   src = p,
   contentType = "image/svg+xml",
   style = "padding:15px;width:230px",
   alt = "GLOBE"
  ))
 
  }, deleteFile=F) 
})