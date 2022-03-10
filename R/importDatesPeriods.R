#' Import dates (years and periods)
#'
#' Import periods and years that will be used to run the simulations
#'
#' @name importDatesPeriods
#' @aliases importDatesPeriods
#' @param filename the file containing the dates of each floral periods. The file should contain four columns: \code{Year} the year, \code{Period}
#' the period number, \code{Start} the starting date of the period (in the format "dd-mm-yyyy"), \code{End} the ending date of the period
#' (in the same format as the starting date). There could be multiple lines for the same year and the same period, corresponding to sub-periods
#' or to different visits during the same period. In this case the function returns the largest range of dates.
#' @return a data frame with four columns: \code{Year}, \code{Period}, \code{Start} and \code{End} corresponding to the starting and ending date
#' of each period and each year
importDatesPeriods <- function(filename="data/periodDates.txt"){
  dates <- read.table(filename,header = TRUE)
  dates$Start <- lubridate::dmy(dates$Start)
  dates$End <- lubridate::dmy(dates$End)

  periodDates <- lapply(unique(dates$Year), FUN = function(y){t(sapply(unique(dates$Period[dates$Year==y]),
                                                      FUN = function(p){
                                                        c(y,p,min(format(dates$Start[dates$Period==p&dates$Year==y],"%d-%m-%Y")),
                                                          max(format(dates$End[dates$Period==p&dates$Year==y],"%d-%m-%Y")))}))})
  periodDates <- do.call(rbind,periodDates)
  periodDates <- data.frame(periodDates)
  names(periodDates) <- c("Year","Period","Start","End")
  periodDates$Start <- lubridate::dmy(periodDates$Start)
  periodDates$End <- lubridate::dmy(periodDates$End)

  years <- unique(periodDates$Year)
  cleanDates <- lapply(years,FUN = function(y){
    temp <- periodDates[periodDates$Year==y,]
    temp$EndPreviousPeriod <- dplyr::lag(temp$End)
    temp$BetEndStart <- temp$EndPreviousPeriod + (temp$Start - temp$EndPreviousPeriod)/2 # to avoid dates which are not covered by any period, re-compute the cutting points of each period as the mean between end of last period and start of current period
    temp$Start[!is.na(temp$BetEndStart)] <- temp$BetEndStart[!is.na(temp$BetEndStart)]
    temp$End[1:(nrow(temp)-1)] <- na.omit(dplyr::lead(temp$Start))
    temp$EndPreviousPeriod <- temp$BetEndStart <- NULL
    return(temp)
  })

  periodDates <- do.call(rbind,cleanDates)

  return(periodDates)
}
