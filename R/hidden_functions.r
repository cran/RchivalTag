.switch_if <- function(x,a,b){
  if(x){
    y <- a
  }else{
    y <- b
  }
  return(y)
}

.fact2num <- function(x){  
  as.numeric(as.character(x))
}

.fact2Date <- function(x) as.Date(as.character(x))


.fact2Date.long <- function(x,tz="UTC") {
  out <- as.POSIXct(strptime(as.character(x),"%Y-%m-%d %H:%M:%S",tz=tz))
  i <- which(is.na(out))
  out[i] <- as.POSIXct(strptime(paste(x[i],"00:00:00"),"%Y-%m-%d %H:%M:%S",tz=tz))
  return(out)
}

.date.long2min.dc <- function(x){
  as.numeric(format(x,"%M"))+as.numeric(format(x,"%S"))/60
}

.date.long2hour <- function(x){
  as.numeric(format(x,"%H"))
}

.date.long2min <- function(x){
  as.numeric(format(x,"%M"))
}

.num2date.long <- function(x,tz="UTC",hours.offset=0){
  out <- .fact2Date.long("1970-01-01 00:00:00",tz=tz)+x+hours.offset*(60*60)
  return(out)
}

.num2date <- function(x){
  as.Date(x,origin="1970-01-01")
}


.num2month <- function(m,english=T,abbrev=F){
  if(english) lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  dat <- m
  if(class(m) != "Date") dat <- as.Date(paste0("2012-",m,"-1"))
  Dat <- format(as.Date(dat),"%B")
  if(abbrev) Dat <- format(as.Date(dat),"%b")
  return(Dat)
}

.date2date.long <- function(x,tz="",midday=T){
  sstart <- 12
  if(!midday) sstart <- 0
  strptime(paste(.fact2Date(x),paste0(sstart,":00:00")),"%Y-%m-%d %H:%M:%S",tz = tz)
}


.makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
