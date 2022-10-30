library(lubridate)
## Functions
fromChronToDate <- function(x){
  date = format(as.POSIXct((x) * 86400, origin = "1970-01-01", tz = "UTC"), "%m/%d/%Y %H:%M:%S")
  return(date)
}

dateTimeConverter <- function(date,time){
  data_frm <- c(dates = "y-m-d", times = "h:m:s")
  dt=as.numeric(chron::chron(as.character(date), as.character(time), data_frm))
  return(dt)
}

grade <- function (x, dx) {
  if (dx > 1)
    warning("Not tested for grids larger than one")
  brks <- seq(floor(min(x)), ceiling(max(x)), dx)
  ints <- findInterval(x, brks, all.inside = TRUE)
  x <- (brks[ints] + brks[ints + 1])/2
  return(x)
}

grade_date <- function (x, dx) {
  brks <- seq.Date(from = date("2008-01-01"),to = date("2030-12-31"),by = dx)
  ints <- findInterval(x, brks, all.inside = TRUE)
  x <- brks[ints]
  return(x)
}
