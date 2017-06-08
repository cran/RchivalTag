

ts2histos <- function(ts_df, tad_breaks=NULL, tat_breaks=NULL, split_by=NULL, aggregate_by="Ptt"){
  bin_prefix <- "Bin"
  output <- list()
  for(Type in c("TAD",'TAT')){
    bin_breaks <- NULL
    
    if(Type == "TAD"){
      field <- 'Depth'
      bin_breaks <- tad_breaks
    }else{
      field <- "Temperature"
      bin_breaks <- tat_breaks
    }
    
    if(field == 'Depth' & any(ts_df$Depth[!is.na(ts_df$Depth)] < 0)) {
      warning('Depth records < 0 found and treated as 0') 
      ts_df$Depth[which(ts_df$Depth < 0)] <- 0
    }
    
    if(!is.null(bin_breaks)){
      out <- c()
      aggregate_by <- c(aggregate_by, 'date')
      if(!('date' %in% names(ts_df)) & ('Day' %in% names(ts_df))) aggregate_by[which(aggregate_by == "date")] <- "Day"
      for(vv in c(field, aggregate_by)) if(!(vv %in% names(ts_df))) stop('Could not find vector:\n', vv, '\nin the data frame provided. Please revise!')
      
      if(length(split_by) > 0) aggregate_by <- c(aggregate_by, split_by)
      #       for(Serial in unique(ts_df$Serial)){
      #         for(d in unique(ts_df$date[ts_df$Serial == Serial])){
      
      ts_df.x <- ts_df#[which(ts_df$Serial == Serial & ts_df$date == d), which(names(ts_df) %in% c(aggregate_by, field))]
      #           tstep <- as.numeric(median(difftime(ts_df.x$date.long[2:nrow(ts_df.x)], ts_df.x$date.long[1:(nrow(ts_df.x)-1)], units="secs")))[1]
      #           print(range(ts_df.x[[field]],na.rm=T))
      sm.df <- plyr::ddply(ts_df.x[,c(aggregate_by,field)], aggregate_by, function(x){
        afield <- x[[field]]
        afield <- afield[!is.na(afield)]
        h <- hist(afield, breaks=bin_breaks, plot=F)
        sm <- h$counts*100/sum(h$counts)
        sm <- data.frame(matrix(c(sm, NA), nrow=1))
        names(sm) <- paste0(bin_prefix, 1:ncol(sm))
        #             sm$coverage_24h <- 100*sum(h$counts)/(24*60/(tstep/60))
        sm$nrec <- sum(h$counts)
        avg <- mean(x[[field]],na.rm=T)
        SD <- sd(x[[field]],na.rm=T)
        sm <- cbind(NumBins=length(bin_breaks),Sum=100,sm,avg=avg,sd=SD)
      })
      out <- rbind(out,  sm.df)
      #         }
      #       }
      output[[Type]][["merged"]] <- list(df=out,bin_breaks=bin_breaks)  
    }
  }
  return(output)
}


.ts2tad <- function(ts_df, tad_breaks=NULL, split_by=NULL, aggregate_by='Ptt'){
  out <- ts2histos(ts_df, tad_breaks=tad_breaks, tat_breaks=NULL, split_by=split_by, aggregate_by=aggregate_by)
  output <- out$TAD$merged
  return(output)
}

.ts2tat <- function(ts_df, tat_breaks=NULL, split_by=NULL, aggregate_by='Ptt'){
  out <- ts2histos(ts_df, tat_breaks=tat_breaks, tad_breaks=NULL, split_by=split_by, aggregate_by=aggregate_by)
  output <- out$TAT$merged
  return(output)
}



