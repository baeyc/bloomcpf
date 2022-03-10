# Computation of summary statistics
# -> proportion of zeros and interquartile range per year, per period, per landscape and per year, per period, per habitat

library(tidyr)
library(dplyr)
library(reshape2)

realData <- na.omit(readRDS("data/simulated_data/simulatedObject")@beeData)
realData$dataPointID <- rownames(realData)
abcTable <- readRDS("analyses/results/abcTables/abcTable.rds")

# recoding of landuse categories
realData$LU <- NA
realData$LU[realData$Landuse %in% c("CER","CER1","CER2")] <- "Cereal"
realData$LU[realData$Landuse %in% c("CFB","FB")] <- "FieldBorder"
realData$LU[realData$Landuse %in% c("FST")] <- "FLowerStrip"
realData$LU[realData$Landuse %in% c("GR","GR1","GR2","L","L1","L2","SNH")] <- "Grassland"
realData$LU[realData$Landuse %in% c("MFC")] <- "MassFloweringCrop"
table(realData$LU)

realData$year_period_landscape <- paste0(realData$Year,"_",realData$Period,"_",realData$Landscape)
realData$year_period_habitat <- paste0(realData$Year,"_",realData$Period,"_",realData$LU)

catYearperland <- unique(realData$year_period_landscape)
catYearperLU <- unique(realData$year_period_habitat)

abcTable <- abcTable %>% dplyr::select(starts_with("abundance"))

res <- data.frame()
t0 <- Sys.time()
for (j in 1:nrow(abcTable)){
  if(j%%500 == 0){
    print(j)
    print(Sys.time() - t0)
  }
  tt <- abcTable[j,]

  perYearPerLand <- lapply(catYearperland, FUN = function(i){
    tempData <- realData[realData$year_period_landscape==i,]
    namesData <- paste0("abundance",tempData$dataPointID)
    temp <- as.numeric(tt %>% dplyr::select(all_of(namesData)))
    c(sum(temp==0),quantile(temp,0.75)-quantile(temp,0.25))
  })
  perYearPerHab <- lapply(catYearperLU, FUN = function(i){
    tempData <- realData[realData$year_period_habitat==i,]
    namesData <- paste0("abundance",tempData$dataPointID)
    temp <- as.numeric(tt %>% dplyr::select(all_of(namesData))) # uncomment for abc tables
    c(sum(temp==0),quantile(temp,0.75)-quantile(temp,0.25))
  })

  all <- c(unlist(perYearPerLand),unlist(perYearPerHab))
  names(all) <- c(paste0(rep(catYearperland,each=2),"_",c("nbOf0","IQR")),
                  paste0(rep(catYearperLU,each=2),"_",c("nbOf0","IQR")))
  all <- as.data.frame(t(all))
  all$iteration <- j

  res <- rbind(res,all)
}

saveRDS(res,paste0("analyses/results/summStatSet_abcTable.rds"))
