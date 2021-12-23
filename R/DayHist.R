#' Circular day histogram
#' 
#' Will plot a series of times to a circular histogram over a 24-hour day.  
#' @param x A date-time object. Can be a POSIXct, POSIXlt, Date, Period, chron, yearmon, yearqtr, zoo, zooreg, timeDate, xts, its, ti, jul, timeSeries, or fts objects.  Essentially these are the same object types that can be used by the hour and minute functions from the lubridate package.
#' @param hist.zoom a zoom factor to appropriately scale histogram in plot area.
#' @param hist.col specifies the color the histogram bars will be
#' @param daynight.color a logical specifying if background coloration should be used to show daytime (between sunrise and sunset) and nighttime (between sunset and sunrise).
#' @param date Only used if daynight.color=TRUE. Specifies the date on which day-night background coloration time periods should be calculated.  Should follow the format "MM/DD/YYYY".
#' @param lon Only used if daynight.color=TRUE. Specifies the longitude of the location for which day-night background coloration should be calculated.  Should be in decimal format.
#' @param lat Only used if daynight.color=TRUE. Specifies the latitude of the location for which day-night background coloration should be calculated.  Should be in decimal format.
#' @param day.col Only used if daynight.color=TRUE. Specifies the color for which the daytime period of the background should be shaded.
#' @param night.col Only used if daynight.color=TRUE. Specifies the color for which the nighttime period of the background should be shaded.
#' @return returns a histogram of the input times.
#' @export
#' @importFrom plotrix radial.plot
#' @importFrom circular circular
#' @importFrom circular rose.diag
#' @importFrom lubridate hour
#' @importFrom lubridate minute
#' @importFrom sp SpatialPoints
#' @importFrom maptools sunriset

DayHist=function (x, hist.zoom=6, hist.col="tomato", daynight.color=T, date="03/20/2021",lon=-122.6396394,lat=48.1639127,day.col="lightgoldenrodyellow",night.col="lightcyan") {
  hour.dec=hour(x)+
  minute(x)/60
  conv.rad=(hour.dec/24)*2*pi #converting hour of day to radians
  inv.rad=(2*pi)-conv.rad #inverting it so that it goes clockwise
  shift.rad=inv.rad+(pi/2) #shifting so that 0 is on top
  times=circular(shift.rad)
  xaxis=seq(from=0,to=359,by=(360/24))*(pi/180)

  if(daynight.color){
    location=matrix(c(lon,lat), nrow=1)
    spot=SpatialPoints(location, proj4string=CRS("+proj=longlat +datum=WGS84"))
    setrise=strptime(date,format="%m/%d/%Y")
    rays=seq(from=0,to=pi*2,length.out=10000)
    col.rays=rep(night.col,length(rays))
    day.start=round((as.numeric(difftime(sunriset(spot, as.POSIXct(setrise), direction="sunrise", POSIXct.out=TRUE)$time,setrise))/24)*length(rays))
    day.end=round((as.numeric(difftime(sunriset(spot, as.POSIXct(setrise), direction="sunset", POSIXct.out=TRUE)$time,setrise))/24)*length(rays))
    col.rays[day.start:day.end]=day.col
    radial.plot(rep(c(0,1),length(rays)/2),rays,show.grid=T,clockwise=T,start=pi/2,line.col=col.rays,
                                                   label.pos=xaxis,labels=0:23,show.grid.labels = F)
    rose.diag(times,bins=24,col=hist.col,shrink=0.4,add=T,axes=F,radii.scale="linear",prop=hist.zoom)
    #points(times,pch=21,bg=hist.col,cex=0.5)
  }

  if(!daynight.color){
    rays=seq(from=0,to=pi*2,length.out=10000)
    radial.plot(rep(c(0,1),length(rays)/2),rays,show.grid=T,clockwise=T,start=pi/2,line.col="white",
                label.pos=xaxis,labels=0:23,show.grid.labels = F)
    rose.diag(times,bins=24,col=hist.col,shrink=0.4,add=T,axes=F,radii.scale="linear",prop=hist.zoom)
    #points(times,pch=21,bg=hist.col,cex=0.5)
  }
  
  
}
