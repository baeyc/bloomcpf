#' Import flower data
#'
#' Import flower data from one or several studies among COST, STEP
#'
#' @name importFlowerData
#' @aliases importFlowerData
#' @param timetable a data frame containing the temporal information, as outputed by the \code{importDatesPeriods} function
#' @param studies a list with the names of the studies to be included, among \code{"COST"}, \code{"STEP"} (study \code{"AP"} is not covered since
#' it provides number of flowers or number of inflorescence, which is difficult to convert to a percentage of soil covered by flowers)
#' @param perSpecies a boolean indicating whether the data should be imported at the species level or aggregated (i.e. floral cover for all the species)
#' @return a data frame with the following variables: \describe{
#'   \item{\code{Year}}{the year of the study}
#'   \item{\code{Source}}{the name of the study}
#'   \item{\code{SiteID}}{the ID of the sampling site (usually composed of the landscape ID and the transect number or subs-ite number)}
#'   \item{\code{LandscapeID}}{the ID of the landscape (depends on the study, and can be used to match the landscape maps)}
#'   \item{\code{Repetition}}{the repetition number, for the cases where several measurements were made at the same site at different dates for a given year and period}
#'   \item{\code{Landuse}}{the type of landuse}
#'   \item{\code{Border}}{a boolean indicating whether data were obtained on a border of the landuse category}
#'   \item{\code{Period}}{the period of observation}
#'   \item{\code{FlowerCoverPoint}}{the flower cover in percentage, as a point estimate (i.e. a number between 0 and 100)}
#'   \item{\code{FlowerCoverInt}}{a factor variable giving the flower cover in percentage, as an interval estimate (i.e. gives the interval in which lies the flower cover).}
#' }
#'
#' @examples
#' # With a timetable imported from a file
#' timetable <- importDatesPeriods()
#' fldata <- importFlowerData(timetable,c("COST","STEP"))
#'
#' # With a timetable build by the user
#' timetable <- data.frame(Year=c(2011,2011),
#'                         Period=c(1,2),
#'                         Start=as.Date(c("2011-05-09","2011-06-13")),
#'                         End=as.date(c("2011-06-13","2011-07-29")))
#' fldata <- importFlowerData(timetable,c("STEP"))
#'
importFlowerData <- function(timetable,studies,perSpecies=FALSE){
  # Create data frame that will be produced by the function
  flowerData <- data.frame(Year=numeric(),
                           Source=factor(),
                           SiteID=factor(),
                           LandscapeID=factor(),
                           Repetition=numeric(),
                           Landuse=numeric(),
                           Border=numeric(),
                           Period=numeric(),
                           FlowerCoverPoint=numeric(),
                           FlowerCoverInt=numeric())

  flowerDataSpecies <- data.frame(Year=numeric(),
                                  Source=factor(),
                                  SiteID=factor(),
                                  LandscapeID=factor(),
                                  Repetition=numeric(),
                                  Landuse=numeric(),
                                  Border=numeric(),
                                  Period=numeric(),
                                  Species=factor(),
                                  FlowerCoverPoint=numeric(),
                                  FlowerCoverInt=numeric())

  # Arrange info from dates
  years <- unique(timetable$Year)
  periods <- unique(timetable$Period)

  # Get dates from COST study
  datesCOST <- openxlsx::read.xlsx("data/data_COST.xlsx", sheet = "Read me", rows = 10:20, cols = 2:6, detectDates = T)
  datesCOST$Start <- as.Date(datesCOST$Start,origin = "1899-12-31")-1 # reported error of Excel thinking that 1900 is a leap year, which is not
  datesCOST$End <- as.Date(datesCOST$End,origin = "1899-12-31")-1 # reported error of Excel thinking that 1900 is a leap year, which is not
  classCOST <- 0:6 # categories of flower cover in the COST project
  labelsCOST <- c("0","<2%","2-6%","6-10%","10-20%","20-50%","50-100%") # upper bounds on the percentages of flower cover of each category

  # Get all data
  if (!perSpecies){
    if ("COST" %in% studies) flowerCOSTall <- openxlsx::read.xlsx("data/data_COST.xlsx", sheet = "flower")
    if ("STEP" %in% studies){
      flowerSTEPall <- openxlsx::read.xlsx("data/data_STEP.xlsx", sheet = "plantdata", detectDates = T)
      flowerSTEPall$Date <- as.Date(flowerSTEPall$Date,origin = "1899-12-31")-1 # reported error of Excel thinking that 1900 is a leap year, which is not
      flowerSTEPall$Country <- NULL # we don't need this information

      # aggregate flower cover over species
      flowerSTEPall <- aggregate(FlowerCover ~ Year + StudyAreaId + StudySite + SamplingRound + Period + Transect + Date, data=flowerSTEPall,FUN=sum)
    }
    # if ("AP" %in% studies){
    #   wb <- XLConnect::loadWorkbook("data/data_AP.xls", create = T)
    #   flowerAP <- XLConnect::readWorksheet(wb, sheet = "Förekomst")
    #   flowerAP$Year <- as.numeric(substr(flowerAP$Datum,1,4))
    #   flowerAP$Datum <- as.Date(flowerAP$Datum,origin="1899-12-31")-1
    # }
  }else{
    if ("COST" %in% studies) flowerCOSTall <- openxlsx::read.xlsx("data/data_COST.xlsx", sheet = "flower species")
    if ("STEP" %in% studies){
      flowerSTEPall <- openxlsx::read.xlsx("data/data_STEP.xlsx", sheet = "plantdata", detectDates = T)
      flowerSTEPall$Date <- as.Date(flowerSTEPall$Date,origin = "1899-12-31")-1 # reported error of Excel thinking that 1900 is a leap year, which is not
      flowerSTEPall$Country <- NULL # we don't need this information
    }
    # if ("AP" %in% studies){
    #   wb <- XLConnect::loadWorkbook("data/data_AP.xls", create = T)
    #   flowerAP <- XLConnect::readWorksheet(wb, sheet = "Förekomst")
    #   flowerAP$Year <- as.numeric(substr(flowerAP$Datum,1,4))
    #   flowerAP$Datum <- as.Date(flowerAP$Datum,origin="1899-12-31")-1
    # }
  }

  for (y in years){
    datesWithinYear <- timetable[timetable$Year==y,]
    for (i in periods){
      datesWithinPeriod <- datesWithinYear[datesWithinYear$Period==i,]

      ## Get data from COST study
      ## ------------------------
      # Extract data for the year and period concerned
      if ("COST" %in% studies){
        periodCOST <- datesCOST[datesCOST$Year == y,]

        if (nrow(periodCOST)>0){
          # Create a dummy variable to indicate if the dates asked by the user match the dates of the observations
          periodCOST$toinclude <- (datesWithinPeriod$Start <= periodCOST$Start) & (periodCOST$End <= datesWithinPeriod$End)

          # Get the species
          if (perSpecies){
            flowerCOST <- merge(flowerCOSTall,periodCOST[c("Year","Visit.number","toinclude")],by = c("Year","Visit.number"))
            flowerCOST <- flowerCOSTall[flowerCOSTall$toinclude,]

            d <- data.frame()
            for (k in 1:nrow(flowerCOST)){
              id <- flowerCOST[k,1:5]
              species <- flowerCOST[k,6:198]
              zeroVar <- (species==0)
              species <- data.frame(Species=names(species)[!zeroVar],FlowerCoverInt=species[!zeroVar],row.names = NULL)
              temp <- cbind(id[rep(row.names(id),nrow(species)),],species)
              d <- rbind(d,temp)
            }
            rm(temp)
            flowerCOST <- d
            rm(d)

            flowerCOST$rep <- as.numeric(substr(flowerCOST$Sampling.site,2,2))
            flowerCOST$rep[is.na(flowerCOST$rep)] <- 0
            flowerCOST$Sampling.site[substr(flowerCOST$Sampling.site,1,1)=="c"] <- "Crop_nonflowering"
            flowerCOST$Sampling.site[substr(flowerCOST$Sampling.site,1,1)=="g"] <- "Grassland_seminatural"
            flowerCOST$Sampling.site[substr(flowerCOST$Sampling.site,1,1)=="l"] <- "Grassland_improved"
            colnames(flowerCOST)[colnames(flowerCOST)=="Sampling.site"] <- "Landuse"
            flowerCOST$Border <- flowerCOST$Transect.position=="border"
            flowerCOST$SiteID <- paste(flowerCOST$Landscape,flowerCOST$rep,sep="_")
            if (length(na.omit(flowerCOST$rep))==0) flowerCOST$rep <- NULL # delete the "rep" variable if its empty
            colnames(flowerCOST)[colnames(flowerCOST)=="Landscape"] <- "LandscapeID"
            flowerCOST$Transect.position <- NULL
            flowerCOST$toinclude <- NULL
            flowerCOST$rep <- NULL
            colnames(flowerCOST)[colnames(flowerCOST)=="Flower.cover"] <- "FlowerCoverInt"
            flowerCOST$FlowerCoverPoint <- NA
            flowerCOST$Source <- "COST"
            flowerCOST$Period <- i
            colnames(flowerCOST)[colnames(flowerCOST)=="Visit.number"] <- "Repetition"

            flowerDataSpecies <- rbind(flowerDataSpecies,flowerCOST)
          }else{
            flowerCOST <- merge(flowerCOSTall,periodCOST[c("Year","Visit.number","toinclude")],by = c("Year","Visit.number"))
            flowerCOST <- flowerCOST[flowerCOST$toinclude,]

            flowerCOST$rep <- as.numeric(substr(flowerCOST$Sampling.site,2,2))
            flowerCOST$rep[is.na(flowerCOST$rep)] <- 0
            flowerCOST$Sampling.site <- NULL
            flowerCOST$Border <- flowerCOST$Transect.position=="border"
            flowerCOST$SiteID <- paste(flowerCOST$Landscape,flowerCOST$rep,sep="_")
            if (length(na.omit(flowerCOST$rep))==0) flowerCOST$rep <- NULL # delete the "rep" variable if its empty
            colnames(flowerCOST)[colnames(flowerCOST)=="Landscape"] <- "LandscapeID"
            flowerCOST$Transect.position <- NULL
            flowerCOST$toinclude <- NULL
            flowerCOST$rep <- NULL
            colnames(flowerCOST)[colnames(flowerCOST)=="Flower.cover"] <- "FlowerCoverInt"
            flowerCOST$FlowerCoverPoint <- NA
            flowerCOST$Source <- "COST"
            flowerCOST$Period <- i
            colnames(flowerCOST)[colnames(flowerCOST)=="Visit.number"] <- "Repetition"

            flowerData <- rbind(flowerData,flowerCOST)
          }
        }
      }

      ## Get data from STEP study
      ## ---------------------------
      if ("STEP" %in% studies){
        flowerSTEP <- flowerSTEPall[datesWithinPeriod$Start <= flowerSTEPall$Date & flowerSTEPall$Date <= datesWithinPeriod$End,]

        if (perSpecies){
          if (nrow(flowerSTEP) > 0){
            colnames(flowerSTEP)[colnames(flowerSTEP)=="FlowerCover"] <- "FlowerCoverPoint"
            colnames(flowerSTEP)[colnames(flowerSTEP)=="scientfic.name"] <- "Species"
            colnames(flowerSTEP)[colnames(flowerSTEP)=="SamplingRound"] <- "Repetition"
            flowerSTEP$PlantGenus <- NULL
            flowerSTEP$PlantSpecies <- NULL
            flowerSTEP$SiteID <- paste(flowerSTEP$StudyAreaId,flowerSTEP$Transect,sep="_")
            flowerSTEP$FlowerCoverInt <- NA
            flowerSTEP$Source <- "STEP"
            flowerSTEP$Period <- i
            # Identify whether its a border or not
            flowerSTEP$Border <- FALSE
            flowerSTEP$Border[grep("B",flowerSTEP$StudySite)] <- TRUE
            flowerSTEP$Border[flowerSTEP$StudySite=="MFC" & flowerSTEP$Transect==2] <- TRUE # Transect 2 for MFC is on the border
            flowerSTEP$StudySite[flowerSTEP$StudySite %in% c("SNH","FB","CFB")] <- "Grassland_seminatural"
            flowerSTEP$StudySite[flowerSTEP$StudySite=="MFC"] <- "Crop_osr_automn"
            flowerSTEP$StudySite[flowerSTEP$StudySite=="FST"] <- "Crop_osr_spring"
            colnames(flowerSTEP)[colnames(flowerSTEP)=="StudySite"] <- "Landuse"
            flowerSTEP$StudyAreaId <- flowerSTEP$Date <- flowerSTEP$Transect <- NULL

            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint==0] <- 0
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>0 & flowerSTEP$FlowerCoverPoint<=2] <- 1
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>2 & flowerSTEP$FlowerCoverPoint<=6] <- 2
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>6 & flowerSTEP$FlowerCoverPoint<=10] <- 3
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>10 & flowerSTEP$FlowerCoverPoint<=20] <- 4
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>20 & flowerSTEP$FlowerCoverPoint<=50] <- 5
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>50] <- 6

            flowerDataSpecies <- rbind(flowerDataSpecies,flowerSTEP)
          }
        }else{
          if (nrow(flowerSTEP) > 0){
            colnames(flowerSTEP)[colnames(flowerSTEP)=="FlowerCover"] <- "FlowerCoverPoint"
            colnames(flowerSTEP)[colnames(flowerSTEP)=="SamplingRound"] <- "Repetition"
            flowerSTEP$SiteID <- paste(flowerSTEP$StudyAreaId,flowerSTEP$Transect,sep="_")
            colnames(flowerSTEP)[colnames(flowerSTEP)=="StudyAreaId"] <- "LandscapeID"
            flowerSTEP$FlowerCoverInt <- NA
            flowerSTEP$Source <- "STEP"
            flowerSTEP$Period <- i
            # Identify whether its a border or not
            flowerSTEP$Border <- FALSE
            flowerSTEP$Border[grep("B",flowerSTEP$StudySite)] <- TRUE
            flowerSTEP$Border[flowerSTEP$StudySite=="MFC" & flowerSTEP$Transect==2] <- TRUE # Transect 2 for MFC is on the border
            flowerSTEP$StudySite[flowerSTEP$StudySite %in% c("SNH","FB","CFB")] <- "Grassland_seminatural"
            flowerSTEP$StudySite[flowerSTEP$StudySite=="MFC"] <- "Crop_osr_automn"
            flowerSTEP$StudySite[flowerSTEP$StudySite=="FST"] <- "Crop_osr_spring"
            colnames(flowerSTEP)[colnames(flowerSTEP)=="StudySite"] <- "Landuse"
            flowerSTEP$StudyAreaId <- flowerSTEP$Date <- flowerSTEP$Transect <- NULL

            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint==0] <- 0
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>0 & flowerSTEP$FlowerCoverPoint<=2] <- 1
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>2 & flowerSTEP$FlowerCoverPoint<=6] <- 2
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>6 & flowerSTEP$FlowerCoverPoint<=10] <- 3
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>10 & flowerSTEP$FlowerCoverPoint<=20] <- 4
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>20 & flowerSTEP$FlowerCoverPoint<=50] <- 5
            flowerSTEP$FlowerCoverInt[flowerSTEP$FlowerCoverPoint>50] <- 6

            flowerData <- rbind(flowerData,flowerSTEP)
          }
        }
      }

      # ## Get data from AP study
      # ## ------------------------
      # if ("AP" %in% studies){
      #   flowerAP <- flowerAPall[flowerAPall$Datum >= min(datesWithinPeriod) & flowerAPall$Datum <= max(datesWithinPeriod),]
      #
      #   if (nrow(flowerAP)>0){
      #     flowerAP$Habitattyp[flowerAP$Habitattyp %in% c("vägkant","bete","grödzon")] <- "Grassland_seminatural"
      #     flowerAP$Habitattyp[flowerAP$Habitattyp=="säd"] <- "Crop_nonflowering"
      #     flowerAP$Habitattyp[flowerAP$Habitattyp=="vall"] <- "Grassland_improved"
      #     colnames(flowerAP)[colnames(flowerAP)=="Habitattyp"] <- "Landuse"
      #     flowerAP$SiteID <- paste(flowerAP$Lokal,flowerAP$Ruta,sep="_")
      #     flowerAP$Lokal <- flowerAP$Ruta <- flowerAP$Datum <- flowerAP$hab_nr <- flowerAP$inv_area_m2 <- NULL
      #     flowerAP$Sum.Rutor.av.Hab <- flowerAP$Antal <- flowerAP$Enhet <- flowerAP$hab_ny <- flowerAP$notering <- NULL
      #     colnames(flowerAP)[colnames(flowerAP)=="Art_kort"] <- "Species"
      #     colnames(flowerAP)[colnames(flowerAP)=="blomdens"] <- "FlowerCoverPoint"
      #     flowerAP$hab_agg <- NULL
      #     flowerAP$FlowerCoverInt <- NA
      #     flowerAP$Source <- "AP"
      #     flowerAP$Period <- i
      #     flowerAP$Repetition <- flowerAP$omg
      #     flowerAP$Border <- NA
      #     flowerAP$omg <- NULL
      #
      #     flowerDataSpecies <- rbind(flowerDataSpecies,flowerAP)
      #   }
      # }

    }
  }

  if (perSpecies){
    flowerDataSpecies$Species <- sub("([A-Za-z]+).*", "\\1", flowerDataSpecies$Species) # keep only the first word
    row.names(flowerDataSpecies) <- NULL

    flowerDataSpecies$FlowerCoverInt <- factor(flowerDataSpecies$FlowerCoverInt,levels = classCOST, labels = labelsCOST)
    flowerDataSpecies$Landuse <- as.factor(flowerDataSpecies$Landuse)
    flowerDataSpecies$Species <- as.factor(flowerDataSpecies$Species)
    flowerDataSpecies$SiteID <- as.factor(flowerDataSpecies$SiteID)
    flowerDataSpecies$Source <- as.factor(flowerDataSpecies$Source)
  }else{
    flowerData$FlowerCoverInt <- factor(flowerData$FlowerCoverInt,levels = classCOST, labels = labelsCOST)
    flowerData$Landuse <- as.factor(flowerData$Landuse)
    flowerData$SiteID <- as.factor(flowerData$SiteID)
    flowerData$Source <- as.factor(flowerData$Source)
  }

  if (perSpecies){
    return(flowerDataSpecies)
  }else{
    return(flowerData)
  }
}


