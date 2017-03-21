
plot_geopos <- function(file, pos, xlim, ylim, prob_lim=.75, pal="jet", alpha=70, type="p", pch=19, add=FALSE, ...){
  
  cmap <- NULL
#   oceanmap::plotmap('lion') ### check for Error in get(dbname) : object 'worldHiresMapEnv' not found

  # ### altered script
  # file <- "/home/robert/Dropbox/my_R-packages/RchivalTag.build/RchivalTag/inst/example_files/15P1019-104659-1-GPE3.nc"
  # # file <- system.file("example_files/15P1019-104659-1-GPE3.nc",package="RchivalTag")
  # 
  # ### extract all 0.5-polygons from all tags:
  # inst.pkg(oceanmap)
  # inst.pkg(ncdf4)
  # library(maptools)
  # library(rgeos)
  
Return <- F

  if(substr(file,nchar(file)-2,nchar(file)) == "csv" | !missing(pos)){
    if(missing(pos)){
      #### check for header line:
      skip <- -1
      header_found <- F
      while(!header_found){
        skip <- skip +1
        header0 <- as.character(unlist(read.delim(file,sep=",",header=F,nrows=1,skip=skip,stringsAsFactors=F)))
        header_found <- any(grepl("Most.Likely",header0))
      }
      pos <- read.csv(file, header=T,sep=',', skip=skip)
      head(pos)
      names(pos) <- gsub('Most.Likely.', '', names(pos))
      names(pos) <- gsub('gitude', '', names(pos))
      names(pos) <- gsub('itude', '', names(pos))  
    }
#     if(!missing(v_area)){
#       r <- regions(v_area)
#       xlim <- r$xlim
#       ylim <- r$ylim
#     }else{
      xlim <- range(pos$Lon+c(.5,-.5))
      ylim <- range(pos$Lat+c(.5,-.5))
#     }
    if(!add) oceanmap::empty.plot(xlim=xlim,ylim=ylim,axes=T) #oceanmap::plotmap(raster::extent(c(xlim,ylim))) 
    
    if(type == "l"){ ## plot positions as line
      lines(pos$Lon, pos$Lat, ...)
    }else{ ## plot positions as dots
      data(cmap, package='oceanmap', envir = environment())
      if(length(pal) == 1 & pal[1] %in% names(cmap)) pal <- cmap[[pal]]
      tsteps <- 1:nrow(pos)
      pos$col<- .makeTransparent(colorRampPalette(pal)(length(tsteps)),alpha = alpha)#[1:100] #creates a color scale, add as many values as you want or use an existing scale
      points(pos$Lon, pos$Lat, col=pos$col, pch=pch, ...)
      Return <- T 
    }
    # 
    # # 
    # # 
    # # file <- "/home/robert/Dropbox/my_R-packages/RchivalTag.build/RchivalTag/inst/example_files/15P1019-104659-1-GPE3.nc"
    # # 
    # # #### original script
    # # StackedObject<-stack(file,varname="twelve_hour_likelihoods") #creates a raster object that contains
    # # MergedObject<-overlay(StackedObject,fun=mean) #merges all of the stacked likelihood surfaces using the function of your choice (can do mean, sum, etc, or make your own).
    # # MergedObject[is.na(MergedObject)]<-0 #Remove NA values
    # # plot(MergedObject)
    # # extent(MergedObject)
    # # 
    # # Raster.big<-raster(ncol=1200,nrow=900,ext=Boundaries) #creates the higher resolution grid
    # # Raster.HR<-resample(x=MergedObject,y=Raster.big,method="bilinear") #will resample the data object onto the higher resolution grid
    # # Raster.HR@data@values<-Raster.HR@data@values/sum(Raster.HR@data@values) #normalize the grid values so they sum to 1
    # # plot(Raster.HR)
    # # 
    # # RasterVals<-sort(Raster.HR@data@values) #sort the probability values
    # # Raster.breaks <- c(RasterVals[max(which(cumsum(RasterVals)<=0.05))],
    # #                    RasterVals[max(which(cumsum(RasterVals)<=0.25))],
    # #                    RasterVals[max(which(cumsum(RasterVals)<=0.5))],1) #sets breaks at the cumulative probabilities
    # # Raster.cols<-colorRampPalette(c("darkblue","blue","lightblue")) #creates a color scale, add as many values as you want or use an existing scale
    # # RasterCols<- c(Raster.cols(3)) #create the colors that you will use (there must be 1 fewer colors than
    # # plot(Raster.HR,col=RasterCols,breaks=Raster.breaks) #plot the polygons
    # # 
    # 
    # ### altered script
    # file <- "/home/robert/Dropbox/my_R-packages/RchivalTag.build/RchivalTag/inst/example_files/15P1019-104659-1-GPE3.nc"
    # # file <- system.file("example_files/15P1019-104659-1-GPE3.nc",package="RchivalTag")
    # 
    # ### extract all 0.5-polygons from all tags:
    # prob_lim <- .9
    # inst.pkg(oceanmap)
    # inst.pkg(ncdf4)
    # library(maptools)
    # library(rgeos)
    # alpha <- 70
    # pal <- 'jet'
    
  }else{
    if(substr(file,nchar(file)-2,nchar(file)) == ".nc"){
      
      data(cmap, package='oceanmap', envir = environment())
      
      if(prob_lim >= 1) stop('"prob_lim" needs to be smaller than 1. Please revise!')
      
      nc <- ncdf4::nc_open(file)
      #     print(nc)
      time <- .date2date.long("1970-01-01",tz="UTC",midday=F)+ncdf4::ncvar_get(nc,"twelve_hour_timestamps") # seconds since 1970-1-1
      lons <- ncdf4::ncvar_get(nc, "longitude")
      lats <- ncdf4::ncvar_get(nc, "latitude")
      
      pols <- list()
      if(!add) {
        xlim <- par()$usr[1:2]
        ylim <- par()$usr[3:4]
      }else{
#         if(!missing(v_area)){
#           r <- regions(v_area)
#           xlim <- r$xlim
#           ylim <- r$ylim
#         }else{
          if(missing(xlim)) xlim <- range(lons)
          if(missing(ylim)) ylim <- range(lats)
#         }
      }
      Boundaries <- raster::extent(c(xlim, ylim)) #creates a bounding box to include all of the different gridded area sizes
#       if(!add) oceanmap::plotmap(Boundaries) 
      if(!add) oceanmap::empty.plot(xlim=xlim,ylim=ylim,axes=T) #oceanmap::plotmap(raster::extent(c(xlim,ylim))) 
      
      #### load polygons:
      i <- 1
      for(i in 1:length(time)){
        #       print(i)
        Raster.LR0 <- raster::raster(file,varname = "twelve_hour_likelihoods",band = i)
        Raster.LR <- raster::extend(Raster.LR0, Boundaries) #then extends any of your surfaces with the set boundaries
        #You can then use stack() to stack multiple tags, and overlay() to merge them together into a single probability surface.
        #To interpolate a surface (resample it at a higher resolution):
        Raster.big <- raster::raster(ncol=1200,nrow=1200,ext=Boundaries) #creates the higher resolution grid
        Raster.HR <- raster::resample(x=Raster.LR,y=Raster.big,method="bilinear") #will resample the data object onto the higher resolution grid
        Raster.HR@data@values <- Raster.HR@data@values/sum(Raster.HR@data@values,na.rm = T) #normalize the grid values so they sum to 1
        
        RasterVals <- sort(Raster.HR@data@values) #sort the probability values
        Raster.breaks <- c(RasterVals[max(which(cumsum(RasterVals)<=prob_lim))])
        cl <- try(rasterToContour(Raster.HR,levels = Raster.breaks),silent = T)
        cl
        if(class(cl) != "try-error"){
          p <- maptools::SpatialLines2PolySet(cl)
          spolys <- maptools::PolySet2SpatialPolygons(p)
          pols[[i]] <- spolys
        }
      }
      
      df0 <- c()
      df0 <- data.frame(ncfile=file,tstart=time[1],tend=tail(time,1))
      df0$date.start <- as.Date(df0$tstart)
      df0$date.end <- as.Date(df0$tend)
      head(df0)
      
      if(length(pal) == 1 & pal[1] %in% names(cmap)) pal <- cmap[[pal]]
      ### set common colors
      tt <- as.numeric(difftime(time[2],time[1],units = "secs"))
      tsteps <- .num2date.long(seq(as.numeric(min(df0$tstart)),as.numeric(max(df0$tend)),by=tt),tz = "UTC",hours.offset = 0)
      Raster.cols <- .makeTransparent(colorRampPalette(pal)(length(tsteps)),alpha = alpha)#[1:100] #creates a color scale, add as many values as you want or use an existing scale
      df.col <- data.frame(tstep=tsteps) # merge colors and time steps
      df.col$color <- as.character(Raster.cols)
      df.col$color_full <- as.character(colorRampPalette(cmap$jet)(length(tsteps)))
      df.col$date.num <- as.numeric(df.col$tstep)
      
      pos <- tsub <- df.col[which(df.col$tstep %in% time),]
      Return <- T
      ### plot basic map:
      
      ### plot polygons:
      list_pols <- list()
      for(i in 1:length(time)){
        plot(pols[[i]],add=T,col=tsub$color[i],border=tsub$color[i], ...)
        add <- pols[[i]]@polygons
        add[[1]]@ID <- as.character(i)
        list_pols[i] <- add
      }
      all <- sp::SpatialPolygons(list_pols)
      merged <- try(unionSpatialPolygons(all,IDs=rep(1,length(all))),silent = T)
      if(!is(merged,"try-error")) plot(merged,add=T)
    }
  }
  if(Return) return(pos)
}



