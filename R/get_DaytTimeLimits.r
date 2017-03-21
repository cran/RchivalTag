get_DayTimeLimits <- function(pos){
  if(any(!(c("Lon","Lat","date.long") %in% names(pos)))) stop("no geolocation data or date.long vector provided (Lon, Lat, date.long)! please revise.")
    
  pos$date.long <- as.POSIXct(pos$date.long,tz = "UTC")
  pos$sunrise <- sunriset(cbind(pos$Lon,pos$Lat), pos$date.long, direction="sunrise", POSIXct.out=TRUE)$time
  pos$sunset <- sunriset(cbind(pos$Lon,pos$Lat), pos$date.long, direction="sunset", POSIXct.out=TRUE)$time
  pos$dawn.naut <- crepuscule(cbind(pos$Lon,pos$Lat), pos$date.long, solarDep=12, direction="dawn", POSIXct.out=TRUE)$time
  pos$dawn.ast <- crepuscule(cbind(pos$Lon,pos$Lat), pos$date.long, solarDep=18, direction="dawn", POSIXct.out=TRUE)$time
  pos$dusk.naut <- crepuscule(cbind(pos$Lon,pos$Lat), pos$date.long, solarDep=12, direction="dusk", POSIXct.out=TRUE)$time
  pos$dusk.ast <- crepuscule(cbind(pos$Lon,pos$Lat), pos$date.long, solarDep=18, direction="dusk", POSIXct.out=TRUE)$time
  
  return(pos)
}


classify_DayTime <- function(pos, twilight.set="ast"){
  dawn <- paste0('dawn.',twilight.set)
  dusk <- paste0('dusk.',twilight.set)

  if(!all(c(dawn, dusk, 'sunrise','sunset') %in% names(pos))) pos <- get_DayTimeLimits(pos)
  pos$daytime <- 'Day'
  pos$daytime[which(pos$date.long < pos$sunrise | pos$date.long >= pos$sunset)] <- 'Night'
  pos$daytime.long <- pos$daytime
  pos$daytime.long[which(pos$date.long >= pos[[dawn]] & pos$date.long < pos$sunrise)] <- 'Dawn'
  pos$daytime.long[which(pos$date.long >= pos[[dusk]] & pos$date.long < pos$sunset)] <- 'Dusk'
  return(pos)
}