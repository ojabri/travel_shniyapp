##print pretty numbers. add comas for thousands and currency symbol
pn = function(x, currency = NULL){

 x = formatC(x, format="d", big.mark=',')
 if(!is.null(currency)){
 #print(x)
 #str(currency)
  if(currency == "Dollars"){
   x = paste("$", x, collapse="", sep="")
  } else if(currency == "Pounds") {
    x = paste("£", x, collapse="", sep="")
  }
 }
 x
}

prepData = function(expenses, dateFormat){
 names(expenses) = make.names(names(expenses))
 expenses$Country = as.character(expenses$Country)
 expenses$Currency = as.character(expenses$Currency)
 expenses$City = as.character(expenses$City)
 #Convert to dates
 expenses$Check.In = as.Date(as.character(expenses$Check.In), format = dateFormat)
 expenses$Check.Out = as.Date(as.character(expenses$Check.Out), format = dateFormat)
 
 expenses
}

spreadMultiDayExpenses = function(expenses){
 #subset out hotel expenses
 hotelExpenses = subset(expenses, !is.na(Check.Out))
 hotelExpenses$Length = as.numeric(with(hotelExpenses, Check.Out-Check.In))
 #replicate hotel expenses by number of days while also dividing amount by total days
 hotelExpensesExpanded = hotelExpenses[rep(row.names(hotelExpenses), hotelExpenses$Length),]
 hotelExpensesExpanded$daysFromStart = unlist(sapply(hotelExpenses$Length, seq, from=1))
 hotelExpensesExpanded = mutate(hotelExpensesExpanded,  Check.In = Check.In + daysFromStart-1, Amount = Amount/Length)
 #subset expenses without hostels
 expensesWithoutHostels = subset(expenses,!( !is.na(Check.Out)))
 #add expanded hostels cots back to expenses
 expenses = rbind(expensesWithoutHostels, hotelExpensesExpanded[,c(names(expensesWithoutHostels))])
 
 expenses
}

convertCurrency = function(expenses, exchange){
 #Convert to Dollars and Pounds
 exchange = exchange[order(exchange$Country, exchange$Date),]
 expenses = expenses[order(expenses$Country, expenses$Check.In),]

 expenses = ddply(expenses, .(Country, Currency), function(x){
  country.exchange = exchange[exchange[,"Country"]==unique(ifelse(x$Currency=="local", x$Country, x$Currency)),]
  t = apply(outer(x$Check.In , country.exchange$Date, "-"), 1, function(x) which.min(abs(x)))
  
  x$Dollars = x$Amount * country.exchange[t,"Dollar.exchange"]
  x$Pounds = x$Amount * country.exchange[t,"Pound.exchange"]

  x
 })
 
 expenses
}

deg2rad <- function(deg) return(as.numeric(deg)*pi/180)
# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  long1 = deg2rad(long1)
  long2 = deg2rad(long2)
  lat1 = deg2rad(lat1)
  lat2 = deg2rad(lat2)
  
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  data.frame(distance = d, c = c) # Distance in km
}

greaterCirclePoints = function(lon1, lat1, lon2, lat2, d, f ){
 lon1 = deg2rad(lon1)
 lon2 = deg2rad(lon2)
 lat1 = deg2rad(lat1)
 lat2 = deg2rad(lat2)
 
 A=sin((1-f)*d)/sin(d)
 B=sin(f*d)/sin(d)
 x = A*cos(lat1)*cos(lon1) +  B*cos(lat2)*cos(lon2)
 y = A*cos(lat1)*sin(lon1) +  B*cos(lat2)*sin(lon2)
 z = A*sin(lat1)           +  B*sin(lat2)
 lat=atan2(z,sqrt(x^2+y^2))
 lon=atan2(y,x)
 
 data.frame(lat = lat, lon =lon)
}

align.cities = function(uniqueCitiesLocations){
 n = nrow(uniqueCitiesLocations)
 index = 1:n
 for(i in 2:(n-1)){
  UCL = uniqueCitiesLocations[index,] 
  city1 = UCL[i-1,]
  city2 = UCL[i,]
  city3 = UCL[i+1,]
  
  
  
  if(city1$Check.In == city2$Check.In & city2$Check.In == city3$Check.In) {
   #three cities in one day
   print("3 cities visted in one day - trying to align cities by looking at previous day's cities.")
   
   #print(i)
   city0 = UCL[i-2,]
   city4 = UCL[i+2,]
   
   print("current order of cities")
   print(paste(city1$City, city2$City, city3$City, sep=" to "))
   print(paste("previous city", city0$City))
   print(paste("next city", city4$City))
   
   to1 = which(city0$City == UCL[(i-1):(i+1),"City"])
   to3 = which(city4$City == UCL[(i-1):(i+1),"City"])
   to2 = (1:3)[!1:3 %in% c(to1, to3)]
   
   if(length(to2)==1){
    index[(i-1):(i+1)] = index[(i-1):(i+1)][c(to1, to2, to3)]
   } else {
    print("trying min distance method")
    d = as.matrix(dist(UCL[(i-2):(i+2),c("lat", "lon")]))
    diag(d)=Inf
    a = apply(d, 1, function(x) {which.min(x)} )

    to1=a[1]
    to3=a[5]
    to2=(2:4)[!2:4 %in% c(to1, to3)]
    
    index[(i-2):(i+2)] = index[(i-2):(i+2)][c(1,to1,to2,to3,5)]
   }   
  } else if(city1$City != city2$City & city1$Check.In == city2$Check.In & city1$City==city3$City) {
   #previous city and current city are not the same
   #previous city and current city have the same check in date
   #current city and next city are different
   
   if(length(unique(UCL[(i-2):(i+2),"City"])) < 3 & length(unique(UCL[(i-2):(i+2),"Check.In"])) < 4){
   #if the same 2 cities are visited twice within 5 days with a stay of 2 days each
    #print("Special case do nothing")
   }else if(as.character(city2$City) > as.character(city1$City) ){
    #print("condition 1")
    #print(i)
   
    index[i] = i-1
    index[i-1] = i
   }
  } else if(city3$City != city2$City & city3$Check.In == city2$Check.In & city1$City==city3$City) {
   #next city and current city are not the same
   #next city and current city have the same check in date
   #previous city and next city are the same
   if(as.character(city2$City) < as.character(city3$City) ){
    #print("condition 2")
    #print(i)
    index[i+1] = i
    index[i] = i + 1
   }
   # print("new order of cities")
   # print(paste(uniqueCitiesLocations[index,][i-1,]$City, uniqueCitiesLocations[index,][i,]$City, uniqueCitiesLocations[index,][i+1,]$City, sep=" to "))
   # print(paste("previous city", uniqueCitiesLocations[index,][i-2,]$City))
   # print(paste("next city", uniqueCitiesLocations[index,][i+1,]$City))
   # print("------------------------------")
  }
 }
 row.names(UCL) = 1:n
 UCL
}

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

spaceLandandAirTravel = function(a){
 a=actualTravelData  
 w = which(a$Flight)
 b = a[w[which(c(T,diff(w)>1))], "order"]
 1:length(b)
 for(i in 1:length(b)) {
  a[a$order>b[i],"order"] = a[a$order>b[i],"order"] + i
 }
 a = rbind(a, data.frame( lat= NA, lon=NA, Country= NA, Flight=F, city1.City = NA, city2.City=NA, order= a[b, "order"]+1))
 a =  a[order(a$order),]

}

dayCount = function(x, travelCat=c("hostel", "travel")){
 length(unique(x[x$What.was.it. %in% travelCat,]$Check.In))
}

setupDestTables = function(s, note = expenses$Note, e = expenses, type ){
 destinations = gsub(s, "", grep(s, note , value=T))
 destinations = sapply(destinations, simpleCap)
 origins = e[grep(s, note), c("City", "Dollars", "Pounds", "Exclude")]
 names(origins)[names(origins)=="City"] = "from"
 data.frame(origins, to = as.character(destinations), Type = type)
}


buildTravelRoutes = function(uniqueCitiesLocations2,  dest, ghkey){
 #url="http://www.yournavigation.org/api/1.0/gosmore.php?"
 url="https://graphhopper.com/api/1/route?"
 format = "json"
 distances = data.frame()
 actualTravelData = data.frame()
 
 for(i in 2:nrow(uniqueCitiesLocations2)){
  city1 = uniqueCitiesLocations2[i-1,]
  city2 = uniqueCitiesLocations2[i,]

  directionsFile = paste(city1$City, " to ", city2$City, ".json", sep="")
  directionsFile = paste("directions", directionsFile, sep="/")
  
  print(paste(city1$City, "to", city2$City))
  
  #flights
  flight = F
  if(any(city1$City==as.character(dest$from) & city2$City==as.character(dest$to) & dest$Type %in% c("flight", "ferry"))){
   print("flight")
   flight = TRUE
   d = gcd.hf(city1$lon, city1$lat, city2$lon, city2$lat)
   points = greaterCirclePoints(city1$lon, city1$lat, city2$lon, city2$lat, d$c, seq(0,1, 0.025)) * 180/pi
   distance = d$distance*1000
  }

  if(!file.exists(paste(directionsFile)) & !flight){
   #directionsURL = paste(url, "format=", format, "&flat=", city1$lat, "&flon=",city1$lon, "&tlon=", city2$lon, "&tlat=",city2$lat ,"&v=motorcar&fast=1&layer=mapnik", sep="")
   directionsURL = paste(url, "point=", city1$lat, "%2C",city1$lon, "&point=", city2$lat, "%2C", city2$lon ,"&vehicle=car&locale=de&debug=true&points_encoded=false&key=",ghkey, sep="")
   print(directionsURL)
   try( download.file(directionsURL, directionsFile))
   if(file.info(directionsFile)$size==0){
    directionsURL = paste(url, "point=", city1$lat, "%2C",city1$lon, "&point=", city2$lat, "%2C", city2$lon ,"&vehicle=foot&locale=de&debug=true&points_encoded=false&key=",ghkey, sep="")
    download.file(directionsURL, directionsFile)
    warning("Road directions failed. Using foot")
    }
  }

  if(!flight){
   #a = xmlToList(xmlParse(directionsFile))
   #distance = as.numeric(a$Document$distance)
   #time = a$Document$traveltime
   #points = data.frame(matrix(as.numeric(head(strsplit(a$Document$Folder$Placemark$LineString$coordinates, ",|\n")[[1]], -1)), ncol=2, byrow=T))
   #points = apply(points,2, function(x) as.numeric(as.character(x)))
   #names(points) = c("lon", "lat")
   
   a = jsonlite::fromJSON(directionsFile)
   distance = a$path$distance
   time = a$path$time
   points = as.data.frame(a$path$points$coordinates[[1]])
   names(points) = c("lon", "lat")
   
  }
  
  distances = rbind(distances, data.frame(
   from = city1$City,
   to = city2$City,
   Country = city1$Country,
   distance = distance/1000, #distance in KM
   Flight = flight
   #time = time
  )) 
  
  notByCar = any(with(dest, from==city1$City & as.character(to)==city2$City))
  exclude = subset(dest, from==city1$City & as.character(to)==city2$City )$Exclude
  
  actualTravelData = rbind(actualTravelData, data.frame(
   points$lat, points$lon, 
   Country = city1$Country, 
   Flight = flight, 
   city1$City, 
   city2$City,
   Exclude = ifelse(notByCar, exclude, F)))
   actualTravelData = rbind(actualTravelData, data.frame(points.lat=NA,points.lon=NA,Country=city1$Country,Flight = flight,city1.City=NA,city2.City=NA, Exclude=NA))
 }

 dest = merge(dest, distances)
 dest$miles = dest$distance * 0.6214

 #actualTravelData = unique(actualTravelData)
 names(actualTravelData)[1:2] = c("lat", "lon")
 actualTravelData[,1:2] = as.data.frame(apply(actualTravelData[,1:2], 2, function(x) as.numeric(as.character(x))))
 actualTravelData$order = 1:nrow(actualTravelData)
 #actualTravelData = spaceLandandAirTravel(actualTravelData)
 
 list(
  actualTravelData = actualTravelData, 
  dest = dest
 )
}