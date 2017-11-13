library("Rfacebook")

fbAuth <- fbOAuth(app_id = "1881608938717379", 
                 app_secret = "6a5145b2b699d4644a8553518ff1833e",
                 extended_permissions = FALSE)

# Generate End date of the month
# got source from : https://www.r-bloggers.com/find-the-last-day-of-the-month/

eom <- function(date) {
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0, tz=attr(date,"tz"))
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}



mygetpage <- function(m_from, m_to, id){
  x <- seq(as.POSIXct(m_from),as.POSIXct(m_to),by="months")
  dates <- data.frame(before=x, after=eom(x))
  
  result <- apply(dates, 1, function(x) getPage(page=id, token=fbAuth, n=1000, since=x[1], until=x[2]))
  result <- do.call(rbind,result)
  return(result)
}


chungang <- mygetpage("2014-02-1","2017-10-01","190747347803005")
seogang <- mygetpage("2014-01-1","2017-10-1","413238928809895")
hanyang <- mygetpage("2014-02-1","2017-10-1","580434565381308")
skku <- mygetpage("2014-01-1","2017-10-1","626784727386153")
korea <- mygetpage("2014-01-1","2017-10-1","206910909512230")
yonsei <- mygetpage("2014-01-01","2017-10-1","180446095498086")

save(chungang, seogang, hanyang, skku, korea, yonsei, file="hyungjun_univs.RData")
