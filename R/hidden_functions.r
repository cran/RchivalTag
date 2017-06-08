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

.get_histos_stats <- function(df, bin_breaks){
  nbins <- length(bin_breaks)-1
  vbins <- paste0("Bin",1:nbins)
  mids <- bin_breaks[1:nbins]+diff(bin_breaks)/2
  
  df$SD <- df$avg <- NA
  for(i in 1:nrow(df)){
    s <- c()
    for(j in 1:length(vbins)){
      t <- df[[vbins[j]]][i]*86 # theoreticaly 8640 depth records per day if sampled every 10s
      s <- c(s,rep(mids[j],t))
    }
    df$avg[i] <- mean(s,na.rm=T)
    df$SD[i] <- sd(s,na.rm=T)
  }
  df.new <- df
  return(df.new)
}



