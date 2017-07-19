library(plyr)
library(rio)
library(XML)
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(DT)
library(dygraphs)
library(plyr)
library(reshape)
library(shinyBS)

#http://api.fixer.io/2000-01-03?base=USD

###FUCNTIONS####
source("functions.R",  encoding = 'UTF-8', local = T)
source("config.R",  encoding = 'UTF-8', local = T)

#if(!(file.exists("expenses.csv") | file.exists("exchange.csv"))){
 expenses = import(expensesURL, format="csv")
 exchange = import(exchangeURL, format="csv")

 expenses = prepData(expenses, "%d/%m/%Y")
 names(exchange) = make.names(names(exchange))
 exchange$Date = as.Date(as.character(exchange$Date), format =  "%d/%m/%Y")
 
 expenses = spreadMultiDayExpenses(expenses)
 expenses = convertCurrency(expenses, exchange)
 
 expenses$Exclude[is.na(expenses$Exclude)] = F
 expenses$Who[expenses$Who==""] = "OH"

 write.csv(expenses, "expenses.csv",  row.names = F)
 write.csv(exchange, "exchange.csv",  row.names = F)
#} else {
# expenses = read.csv("expenses.csv")
# exchange = read.csv("exchange.csv")
 
# expenses = prepData(expenses, "%Y-%m-%d")
#}
steps = read.csv("steps.csv")

countryMeta = read.csv(countryMeta.csv)
#uniqueCitiesLocations = read.csv("uniqueCitiesLocations.csv")
cityLocations = read.csv(cityLocations.csv)
#t1 = as.Date(uniqueCitiesLocations$Check.In[1], format="%d/%m/%Y")
#if(is.na(t1)) { uniqueCitiesLocations$Check.In = as.Date(uniqueCitiesLocations$Check.In) } else {uniqueCitiesLocations$Check.In = as.Date(uniqueCitiesLocations$Check.In, format="%d/%m/%Y")}

countries = unique(expenses$Country)


#Geolocate Cities

#Find new cities in expenses
allUniqueCities = unique(expenses[,c("City", "Country")])
#remove rows with blacnk city field
uniqueCities = allUniqueCities[allUniqueCities$City!="",]
#remove cities with lat/lon data 
citiesWithLocations = with(uniqueCities, paste(City,Country)) %in% with(cityLocations, paste(City,Country))
uniqueCities = uniqueCities[!citiesWithLocations,]
#Only run when new cities in expenses
if(nrow(uniqueCities)>0){
 cities = list()
 cityLocations.new = data.frame()
 for(i in  1:nrow(uniqueCities)){
  
  cityCountryString = paste(uniqueCities$City[i], uniqueCities$Country[i], sep=",+")
  cityCountryString = gsub(" ", "%20", cityCountryString)
  queryString = paste("http://nominatim.openstreetmap.org/search?q=", 
   cityCountryString, 
   "&format=json&polygon=0&addressdetails=1",
   sep=""
  )
  city = jsonlite::fromJSON(queryString)
  cities[[i]] = city
  
  cityLocations.new = rbind(cityLocations.new,
   data.frame(
    City = uniqueCities$City[i],
    Country = uniqueCities$Country[i],
    city[1, c("lat", "lon")]
   )
  )
 }

 #uniqueCitiesLocations.new = merge(uniqueCities, cityLocations.new)
 #uniqueCitiesLocations.new = unique(merge(uniqueCitiesLocations.new, expenses[,c("City", "Country", "Check.In", "Who")]))
 cityLocations = rbind(cityLocations, cityLocations.new)
 cityLocations$lat = as.numeric(cityLocations$lat)
 cityLocations$lon = as.numeric(cityLocations$lon)
 write.csv(cityLocations, "city.locations.csv", row.names=F)
 
 uniqueCitiesLocations = unique(merge(cityLocations, expenses[,c("City", "Country", "Check.In", "Who")]))
 #uniqueCitiesLocations = uniqueCitiesLocations[order(uniqueCitiesLocations$Check.In),]
 
 #UCL = ddply(uniqueCitiesLocations, .(Who), align.cities )
 #'UCL = UCL[order(UCL$Check.In),]
 
 UCL.all = data.frame()
 persons = unique(unlist(sapply(unique(uniqueCitiesLocations$Who), strsplit, "")))
 for(p in persons){
  person.route = uniqueCitiesLocations[grep(p, uniqueCitiesLocations$Who),]
  person.route$Who.route = p
  person.route = person.route[!duplicated(person.route[,1:5]),]
  person.route = person.route[order(person.route$Check.In),]
  person.route = align.cities(person.route)
  
  UCL.all = rbind(UCL.all, person.route)
 }
 
 # remove Locals that can not be found 
 UCL = subset(UCL.all, !City %in% c("Rila", "Kol-Ukok"))

 index = c()
 for(i in 2:nrow(UCL)) {
  if(UCL[i,"City"] == UCL[i-1,"City"] & UCL[i,"Who.route"] == UCL[i-1,"Who.route"]) index = c(index, i-1)
 }
 UCL.route = UCL[-index,]


 #####Distances
 dest = setupDestTables( "[Bb]us:", type = "bus" )
 dest = rbind(dest, setupDestTables( "[Ff]light:", type = "flight" ))
 dest = rbind(dest, setupDestTables(  "[Ff]erry:", type = "ferry"))
 dest = rbind(dest, setupDestTables(  "[Tt]rain:", type = "train"))
 dest = rbind(dest, setupDestTables(  "[Tt]axi:", type = "taxi"))
 dest = rbind(dest, setupDestTables(  "[Hh]ike:", type = "hike"))

 uniqueCitiesLocations2 = UCL.route[c("City", "Country", "lat", "lon", "Who.route", "Who")]

 #travelRoutes = buildTravelRoutes(uniqueCitiesLocations2, dest, ghkey)
 travelRoutes = dlply(uniqueCitiesLocations2, .(Who.route), buildTravelRoutes, dest, ghkey )
 actualTravelData = ldply(travelRoutes, function(x){ x$actualTravelData} )
 dest = ldply(travelRoutes, function(x){ x$dest} )
 
 write.csv(actualTravelData, "travel.routes.csv", row.names=F)
 write.csv(dest, "distance.csv", row.names=F) 

} else {

 uniqueCitiesLocations = cityLocations
 
 actualTravelData = read.csv("travel.routes.csv")
 dest = read.csv("distance.csv")

}

# if(ForceRebuild){
 # travelRoutes = buildTravelRouts(uniqueCitiesLocations2, dest)
 # actualTravelData = travelRoutes$actualTravelData
 # dest = travelRoutes$dest
 
 # write.csv(actualTravelData, "travel.routes.csv", row.names=F)
 # write.csv(dest, "distance.csv", row.names=F) 
# }


#Local transport costs
#local.transport.query = "[Ll]ocal|[Mm]etro|[Tt]axi|[Tt]ube|[Tt]uk[ Tt]+uk"
#subset(expenses[grep(local.transport.query, expenses$Note),], What.was.it.=="travel")

#Steps
#steps$date = as.Date(steps$date, format="%Y-%m-%d")
steps$date = as.Date(steps$date, format="%d/%m/%Y")
#expenses = merge(expenses, steps, by.x="Check.In", by.y="date")
steps = steps[steps$steps!=0,]
row.names(steps) = steps$date
