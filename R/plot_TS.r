
empty.plot_TS <- function(xlim, ylim, xticks_interval, ylab="", xlab="Time (UTC)", las=1, xaxs="i", yaxs="i", cex=1, plot_box=TRUE, bty="l",...){
  
  if(class(xlim)[1] == 'Date' | nchar(as.character(xlim[1])) == 10){
    xlim <- as.POSIXct(paste(xlim, '00:00:00'), tz="UTC")
    if(length(xlim) == 1) xlim <- c(xlim, xlim)
    xlim[2] <- xlim[2]+24*60*60
  }
  xlim <- .fact2Date.long(xlim,tz = "UTC")
  if(length(xlim) == 1) xlim <- c(xlim, xlim[1]+24*60*60)
  if(length(xlim) > 2) xlim <- range(xlim)
  
  date.range.l <- .num2date.long(as.numeric(xlim, tz = "UTC", hours.offset = 0))
  date.range <- as.Date(date.range.l)
  n <- length(date.range.l)
  xticks <- seq(date.range.l[1], date.range.l[n], by=1)#21600)
  days <- unique(as.Date(xticks))
  if(missing(xticks_interval)){
    xticks_interval <- 6
    if((as.numeric(xlim[2])-as.numeric(xlim[1]))/(24*60*60) <= 1) xticks_interval <- 3
  }
  xticks <- xticks[which(.date.long2hour(xticks)%%xticks_interval == 0 & .date.long2min.dc(xticks) == 0)]
  
  #   print(.date.long2hour(xticks))
  xtick.labels <- format(xticks, "%H")
  date.range.ll <- date.range.l
  
  xti <- which(xticks >= date.range.ll[1] & xticks <=date.range.ll[2])
  
  date.ticks <- xticks[which(xtick.labels == "12" & xticks >= date.range.ll[1])]
  ###' not correct midday:
  #   if(show.sun){
  #     si <- which(.date.long2hour.dc(xticks[xti]) == 12)
  #     for(sii in si) text(xticks[xti][sii], y=max(ylim)-10, labels = "\U25d7", srt="90", cex=6, col=colors()[143], xpd=F)
  #   }
  par(las=las, yaxs=yaxs, xaxs=xaxs, ...)
  plot(0, 1, axes=FALSE, xlab=xlab, ylab=ylab, ylim=ylim, xlim=xlim, ...)
  axis(1, at=xticks[xti], labels=xtick.labels[xti], xpd=TRUE, pos=ylim[1], cex.axis=.9*cex, lwd=0, lwd.ticks = 1)
  axis(1, at=date.ticks, labels=format(date.ticks, "%Y-%m-%d"), lwd=0, line=1, cex.axis=1*cex)
  axis(2, lwd = 0, lwd.ticks=1)
  if(plot_box) box(bty=bty)
}



plot_DepthTS <- plot_TS <- function(df, y="Depth", xlim, ylim, xticks_interval,
                                    ylab=y, xlab="Time (UTC)", main, main.line=1, plot_info=TRUE, 
                                    ID, ID_label="Serial", #show.temp=F,
                                    plot_DayTimePeriods=TRUE, twilight.set="ast", 
                                    type="l", las=1, xaxs="i", yaxs="i", cex=1, plot_box=TRUE, bty="l", Return=FALSE, ...){
  if(missing(ID)) ID <- unique(df[[ID_label]])
  if(length(ID) > 1) {
    warning("multiple tags in data set: ", paste(ID, collapse=', '))
    main <- ''
  }
  
  if(!("date.long" %in% names(df))) stop('no "date.long" vector provided! please revise.')
  
  if(!missing(xlim)){
    if(class(xlim)[1] == 'Date'){# | nchar(as.character(xlim[1])) == 10){
      xlim <- as.POSIXct(paste(xlim, '00:00:00'), tz="UTC")
      if(length(xlim) == 1) xlim <- c(xlim, xlim)
      xlim[2] <- xlim[2]+24*60*60
    }
    xlim <- .fact2Date.long(xlim,tz = "UTC")
    if(length(xlim) == 1) xlim <- c(xlim, xlim[1]+24*60*60)
    if(length(xlim) > 2) xlim <- range(xlim)
    df <- df[which(df$date.long >= xlim[1] & df$date.long <= xlim[2]),]
  }else{
    xlim <- range(df$date.long)
    xlim <- .fact2Date.long(xlim,tz = "UTC")
  }
  
  if(!missing(ylim)){
  }else{
    ylim <- range(df[[y]],na.rm=TRUE)
  }
  if(y == "Depth" & max(abs(ylim) == max(ylim))) ylim <- rev(range(c(0,max(ylim))))
  
  
  ### fill potential data gaps
  df <- df[which(!is.na(df[[y]])),]
  tstep <- as.numeric(df$date.long[2])-as.numeric(df$date.long[1])
  if(tstep == 0) stop("time step between first valid 'y'-records is 0. please revise!")
  add0 <- data.frame(date.long=seq(df$date.long[1],tail(df$date.long, 1), by=tstep))
  df$date.long <- as.character(df$date.long); add0$date.long <- as.character(add0$date.long)
  df <- merge(df, add0, by="date.long",all=T)
  df$date.long <- strptime(df$date.long, format = "%Y-%m-%d %H:%M:%S")
  df$date <- as.Date(df$date.long)
  
  if(plot_DayTimePeriods){
    if('Lon' %in% names(df) & 'Lat' %in% names(df)){
      if(any(is.na(df$Lon))){
        pos <- unique(df[,c('date','Lon','Lat')])
        pos$Lon <- spline(1:nrow(pos), y = pos$Lon, xout = 1:nrow(pos))$y
        pos$Lat <- spline(1:nrow(pos), y = pos$Lat, xout = 1:nrow(pos))$y
        df$Lon <- df$Lat <- df$sunrise <- c()
        df <- merge(df, pos, by='date', all=TRUE)
      }
    }  
  }
  
  
  if(!("date" %in% names(df))) {
    warning('"date" vector missing and derived from provided "date.long" vector!')
    df$date <- as.Date(df$date.long)
  }
  
  ### plot emptyTS plot:
  par(las=las, yaxs=yaxs, xaxs=xaxs,...)  
  plot(df$date.long, df[[y]], axes=FALSE, lwd=0, cex=0, xlab="", ylab="", xlim=xlim, ylim=ylim, ...)
  
  
  xticks <- seq(xlim[1], xlim[2], by=1)#21600)
  days <- unique(as.Date(xticks))
  if(missing(xticks_interval)){
    xticks_interval <- 6
    if((as.numeric(xlim[2])-as.numeric(xlim[1]))/(24*60*60) <= 1) xticks_interval <- 3
  }
  xticks <- xticks[which(.date.long2hour(xticks)%%xticks_interval == 0 & .date.long2min.dc(xticks) == 0)]
  xtick.labels <- format(xticks, "%H")
  
  #### plot daytime periods:
  if(plot_DayTimePeriods){
      dawn.set <- paste0('dawn.', twilight.set)
      dusk.set <- paste0('dusk.', twilight.set)
      if(all(c(dawn.set, dusk.set, 'sunrise','sunset') %in% names(df)) | all(c('date.long','Lon','Lat') %in% names(df))){
      if(!all(c(dawn.set, dusk.set, 'sunrise','sunset') %in% names(df))){
        warning("not all required day period information found. calling function: 'get_DayTimeLimits'. 
            Consider to run this function before calling 'plot_TS' to increase performance speed.")
        df <- get_DayTimeLimits(df) # get sunrise, sunset and dusk/dawn information
      }
      df$dawn <- df[[dawn.set]]
      df$dusk <- df[[dusk.set]]
      
      rect(xlim[1], ylim[1], xlim[2], ylim[2], col="grey", lwd=0)
      sunrise <- sunset <- dawn <- dusk <- NA
      for(d in days){
        k <- df[which(df$date == d), ]
        if(nrow(k) > 0){
          dusk <- mean(k$dusk)
          dawn <- mean(k$dawn)
          sunrise <- mean(k$sunrise)
          sunset <- mean(k$sunset)
        }else{
          sunrise <- sunrise+24*60*60
          sunset <- sunset+24*60*60
          dawn <- dawn+24*60*60
          dusk <- dusk+24*60*60
        }      
        rect(sunrise, ylim[1], sunset, ylim[2], col="white", lwd=0)
        rect(dawn, ylim[1], sunrise, ylim[2], col="grey90", lwd=0)
        rect(sunset, ylim[1], dusk, ylim[2], col="grey90", lwd=0)
        dawn.dusk <- F
      }
    }else{
      warning('no geolocation data (Lon, Lat) or daytime period information provided! setting plot_DayTimePeriods to FALSE')
      plot_DayTimePeriods <- F
    }
  }
  df$dusk <- df$dawn <- c()
  
  xti <- which(xticks >= xlim[1] & xticks <=xlim[2])
  axis(1, at=xticks[xti], labels=xtick.labels[xti], xpd=TRUE, pos=par()$usr[3], cex.axis=.9*cex, lwd=0, lwd.ticks = 1)
  
  ### show_time_limits
  # if(.date.long2min(xlim[1]) != 0)  axis(1, at=xlim[1], labels=format(xlim[1], "%H:%M"), xpd=TRUE, pos=par()$usr[3], cex.axis=.9*cex, lwd=0, lwd.ticks = 1)
  # if(.date.long2min(xlim[2]) != 0)  axis(1, at=xlim[2], labels=format(xlim[2], "%H:%M"), xpd=TRUE, pos=par()$usr[3], cex.axis=.9*cex, lwd=0, lwd.ticks = 1)
  
  date.ticks <- xticks[which(xtick.labels == "12" & xticks >= xlim[1])]
  axis(1, at=date.ticks, labels=format(date.ticks, "%Y-%m-%d"), lwd=0, line=1, cex.axis=1*cex)
  
  ###' not correct midday:
  #   if(show.sun){
  #     si <- which(.date.long2hour.dc(xticks[xti]) == 12)
  #     for(sii in si) text(xticks[xti][sii], y=max(ylim)-10, labels = "\U25d7", srt="90", cex=6, col=colors()[143], xpd=F)
  #   }
  
  
  # if(show.temp){ ### interpolate Temperature data and add it to plot:
    # Depth_res <- 1
    # maxDepth <- max(ylim)
    # depths <- seq(0,maxDepth,by=Depth_res) # sequence of depth values defining the vertical grid resolution
    # df$date.longh <- .date2date.long(df$date,midday = F)+date.long2hour(df$date.long)*3600
    # df$date.longh_id <- as.numeric(1+difftime(df$date.longh,min(df$date.longh),units = "hours"))
    # date.longh_all <- seq(min(df$date.longh),max(df$date.longh),by=3600)
    # ndates <- length(date.longh_all)
    # Temperature_matrix <- matrix(ncol=ndates, nrow=length(depths), NA) # date-depth matrix
    # smooth_hours <- 0
    # smh <- smooth_hours
    # 
    # for(d in unique(df$date.longh_id)){
    #   # d <- 1
    #   df_sub <- df[which(df$date.longh_id %in% seq(d-smh,d+smh,by=1)),]
    #   if(length(unique(df_sub$Temp)) > 1){
    #     df_sub$Temp <- df_sub$Temp
    #     # k2 <- plyr::ddply(df_sub[,which(names(df_sub) %in% c('date.longh_id','Depth','Temp'))],c("date.longh_id","Depth"),function(x)c(Temp=mean(x$Temp)))
    #     # xx <- c(k2$date.longh_id,k2$date.longh_id+1)
    #     
    #     k2 <- plyr::ddply(df_sub[,which(names(df_sub) %in% c('date','Depth','Temp'))],c("date","Depth"),function(x)c(Temp=mean(x$Temp)))
    #     xx <- c(k2$date,k2$date+1)
    #     
    #     yy <- c(k2$Depth,k2$Depth)
    #     zz <- c(k2$Temp,k2$Temp)
    #     temp <- akima::interp(x=xx,y=yy,z=zz,linear=T,duplicate='mean',yo=depths) # interpolate data per day
    #     Temperature_matrix[,d] <- temp$z[1,]
    #   }
    # }
    # xi <- date.longh_all
    # yi <- depths
    # zi <- Temperature_matrix
    # 
    # print(xi)
  #   
  #   
  #   inst.pkg(raster)
  #   f <- raster(zi[nrow(zi):1, ])
  #   extent(f) <- extent(c(range(as.numeric(xi)), range(yi)))
  #   
  #   data(cmap)
  #   par(new=TRUE)
  #   image(f, xlim=as.numeric(xlim), ylim=ylim, col=cmap$light.jet, axes=F, ylab="", xlab="", add=T)#, zlim=zlim)
  # }
  
  
    
  ### plot vertical track:
  par(new=T)
  plot(df$date.long, df[[y]], axes=FALSE, xlab="", ylab=ylab, ylim=ylim, xlim=xlim, type=type,xpd=TRUE,...)
  axis(2, lwd = 0, cex.axis=.9*cex, lwd.ticks=1)
  if(plot_box) box(bty=bty)
  
  if(plot_info) mtext(side=1, xlab, line=3)
  if(missing(main)) main <- paste("Tag ID", ID, "-", paste0(xlim, collapse=" : "))
  if(plot_info) mtext(side=3, main, font=1.6, line=main.line, cex=cex*1.2)
  if(Return) return(df)
}

