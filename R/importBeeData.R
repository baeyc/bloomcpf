#' Import bee data
#'
#' Import bee data from either one or both COST and STEP studies. For a given timetable, extract the data of the number of bees observed at each
#' location, and gives information about the locations (coordinates, ...)
#'
#' @name importBeeData
#' @aliases importBeeData
#' @param timetable a data frame containing the temporal information, as outputed by the \code{importDatesPeriods} function
#' @param studies a list with the names of the studies to be included, among \code{"COST"}, \code{"STEP"}
#' @param beeSpecies the bee species for which abundances are requested, in the format "Genus species". Default is \code{"Bombus terrestris"}.
#' @return a data frame with the following variables: \describe{
#'   \item{\code{Year}}{the year of the study}
#'   \item{\code{Source}}{the name of the study}
#'   \item{\code{SiteID}}{the ID of the sampling site (usually composed of the landscape ID and the transect number or subs-ite number)}
#'   \item{\code{Landscape}}{the ID of the landscape (depends on the study, and can be used to match the landscape maps)}
#'   \item{\code{Repetition}}{the repetition number, for the cases where several measurements were made at the same site at different dates for a given year and period}
#'   \item{\code{Landuse}}{the type of landuse}
#'   \item{\code{Period}}{the period of observation}
#'   \item{\code{SizeAreaHa}}{the size, in hectares, of the sampled area}
#'   \item{names of bee species}{one or several variable(s) with the number of bees observed at each location and each data, with the name of the variable(s)}
#' }
#' @importFrom dplyr %>%
#' @importFrom tidyr %>%
#' @examples
#' # With a timetable imported from a file
#' timetable <- importDatesPeriods()
#' beedata <- importBeeData(timetable,c("COST","STEP"))
#'
#' # With a timetable build by the user
#' timetable <- data.frame(Year=c(2011,2011),
#'                         Period=c(1,2),
#'                         Start=as.Date(c("2011-05-09","2011-06-13")),
#'                         End=as.date(c("2011-06-13","2011-07-29")))
#' beedata <- importBeeData(timetable,c("STEP"))
#'

importBeeData <- function(timetable,studies,species="Bombus terrestris"){
  # Get the species
  genusAndSpecies <- t(sapply(1:length(species),function(i){strsplit(species[i]," ")[[1]]}))
  genusAndSpeciesForCOST <- t(sapply(1:length(species),function(i){sub(" ",".",species[i])[[1]]})) # create name to be matched in the COST database

  # load all data from STEP and COST
  if ("COST" %in% studies){
    beesCOSTall <- openxlsx::read.xlsx("data/data_COST.xlsx", sheet = "bumblebee")
    #coordCOSTall <- openxlsx::read.xlsx("data/coord_COST.xlsx")

    # Get dates from COST study
    datesCOST <- openxlsx::read.xlsx("data/data_COST.xlsx", sheet = "Read me", rows = 10:20, cols = 2:6, detectDates = T)
    datesCOST$Start <- as.Date(datesCOST$Start,origin = "1899-12-31")-1 # reported error of Excel thinking that 1900 is a leap year, which is not
    datesCOST$End <- as.Date(datesCOST$End,origin = "1899-12-31")-1 # reported error of Excel thinking that 1900 is a leap year, which is not
    for (i in 1:max(datesCOST$Visit.number)){
      beesCOSTall$Period[beesCOSTall$Visit.number==i] <- unique(datesCOST$Period[datesCOST$Visit.number==i])
    }
  }
  if ("STEP" %in% studies){
    beesSTEPall <- openxlsx::read.xlsx("data/data_STEP.xlsx", sheet = "pollinatordata")
    beesSTEPall$Country <- NULL
    beesSTEPall$Date <- as.Date(beesSTEPall$Date,origin="1899-12-31")-1
    beesSTEPall$genusSpecies <- paste(beesSTEPall$genus,beesSTEPall$species,sep=".")
    beesSTEPall <- beesSTEPall[beesSTEPall$genusSpecies!="NA.NA",]

    # delete missing data not concerning species under study avoid side effects during dplyr with NA values
    beesSTEPall <- as.data.frame(beesSTEPall %>% tidyr::drop_na(StudySite, StudyAreaId, Year, Date))

    # count the number of bees at each date
    beesSTEPall <- beesSTEPall %>%
      dplyr::group_by(Year,StudyAreaId,StudySite,Date,genusSpecies) %>%
      dplyr::summarise(n=dplyr::n()) %>%
      tidyr::spread(genusSpecies, n)
    beesSTEPall <- beesSTEPall[,c("Year","StudyAreaId","StudySite","Date",genusAndSpeciesForCOST)]

    #coordSTEPall <- openxlsx::read.xlsx("data/Sweden_STEP_WP5_data_landuse_corrected20160122.xlsx", sheet = 1)
    #coordSTEPall <- coordSTEPall[,2:7] # delete columns not useful here
  }

  years <- unique(timetable$Year)
  periods <- unique(timetable$Period)

  beesData <- data.frame() # initialize data frame

  for (y in years){
    datesWithinYear <- timetable[timetable$Year==y,]
    for (i in periods){
      datesWithinPeriod <- datesWithinYear[datesWithinYear$Period==i,]

      if (nrow(datesWithinPeriod)>0){
        if ("COST" %in% studies){
          if (y %in% unique(beesCOSTall$Year)){
            #coordCOST <- coordCOSTall[coordCOSTall$Year==y,]
            #coordCOST$Landscape <- coordCOST$farm_ID

            # Arrange file
            for (genus in genusAndSpeciesForCOST){
              beesCOST <- beesCOSTall[beesCOSTall$Year==y & beesCOSTall$Period==i,c("Year","Landscape","Sampling.site","Transect.position","Period",genus)]
              #beesCOST <- merge(beesCOST,coordCOST,by=c("Year","Landscape"))
              datesCOST <- datesCOST[datesCOST$Year==y,]

              if (nrow(beesCOST)>0){
                beesCOST$rep <- NULL
                beesCOST$Border <- beesCOST$Transect.position=="border"
                beesCOST$Source <- "COST"
                beesCOST$Sampling.site[beesCOST$Sampling.site=="c"] <- "CER"
                beesCOST$Sampling.site[beesCOST$Sampling.site=="c1"] <- "CER1"
                beesCOST$Sampling.site[beesCOST$Sampling.site=="c2"] <- "CER2"
                beesCOST$Sampling.site[beesCOST$Sampling.site=="g"] <- "GR"
                beesCOST$Sampling.site[beesCOST$Sampling.site=="g1"] <- "GR1"
                beesCOST$Sampling.site[beesCOST$Sampling.site=="g2"] <- "GR2"
                beesCOST$Sampling.site[beesCOST$Sampling.site=="l"] <- "L"
                beesCOST$Sampling.site[beesCOST$Sampling.site=="l1"] <- "L1"
                beesCOST$Sampling.site[beesCOST$Sampling.site=="l2"] <- "L2"
                colnames(beesCOST)[colnames(beesCOST)=="Sampling.site"] <- "Landuse"
                beesCOST$Transect.position <- NULL

                colnames(beesCOST)[colnames(beesCOST)==genus] <- "NumberOfBees"
                beesCOST$Species <- genus

                # count the number of bees at each date
                beesCOST <- beesCOST %>%
                  dplyr::group_by(Year,Landuse,Period,Landscape,Species) %>%
                  dplyr::summarise(NumberOfBees=sum(NumberOfBees))
                beesCOST <- data.frame(beesCOST)

                beesCOST$Source <- "COST"
              }
            }

            beesData <- rbind(beesData,beesCOST)
          }
        }

        if ("STEP" %in% studies){
          if (y %in% unique(beesSTEPall$Year)){
            #beesSTEP <- beesSTEPall[beesSTEPall$Year==y,]
            beesSTEP <- beesSTEPall[datesWithinPeriod$Start <= beesSTEPall$Date & beesSTEPall$Date <= datesWithinPeriod$End,]

            for (genus in species){

              if (nrow(beesSTEP)>0){
                beesSTEP$Period <- i
                beesSTEP$Date <- NULL
                #beesSTEP <- merge(beesSTEP,coordSTEPall,by=c("Year","StudyAreaId","StudySite"),all.y=T)

                beesSTEP$Landscape <- beesSTEP$StudyAreaId
                colnames(beesSTEP)[colnames(beesSTEP)=="StudySite"] <- "Landuse"
                colnames(beesSTEP)[colnames(beesSTEP)=="size_site_ha"] <- "SizeAreaHa"
                beesSTEP$StudyAreaId <- beesSTEP$Repetition <- NULL

                # Reshape data frame to get one column for the name of the species and one column for the nb of bees
                beesSTEP <- reshape2::melt(beesSTEP,id.vars = c("Year","Landuse","Period","Landscape" ))
                # variable and value are the names of the melted variables from melt function
                colnames(beesSTEP)[colnames(beesSTEP)=="variable"] <- "Species"
                colnames(beesSTEP)[colnames(beesSTEP)=="value"] <- "NumberOfBees"

                beesSTEP$Source <- "STEP"
              }

            }

            # Delete coordinates
            beesSTEP$Lat_dd <- beesSTEP$Long_dd <- NULL

            beesData <- rbind(beesData,beesSTEP)
          }
        }
      }
    }
  }

  # Set factor variables
  #beesData$Landuse <- as.factor(beesData$Landuse)
  beesData$Period <- as.factor(beesData$Period)
  beesData$Landscape <- as.factor(beesData$Landscape)
  beesData$Species <- as.factor(beesData$Species)
  beesData$Source <- as.factor(beesData$Source)

  beesData$NumberOfBees[is.na(beesData$NumberOfBees)] <- 0
  beesData <- unique(beesData) # remove identical lines if any

  beesData <- beesData[beesData$Year%in%years,]

  # Add size of sampling site, 100*2m^2 for COST, 150*1m^2 for STEP
  beesData$SizeAreaInSqMeters <- 100*2
  beesData$SizeAreaInSqMeters[beesData$Source=="STEP"] <- 150

  return(beesData)
}
