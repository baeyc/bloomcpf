# Computation of summary statistics

library(tidyr)
library(dplyr)
library(reshape2)

data <- readRDS("data/realObservedData.rds")

# recoding of landuse categories
data$LU <- NA
data$LU[data$Landuse %in% c("CER","CER1","CER2")] <- "Cereal"
data$LU[data$Landuse %in% c("CFB","FB")] <- "FieldBorder"
data$LU[data$Landuse %in% c("FST")] <- "FLowerStrip"
data$LU[data$Landuse %in% c("GR","GR1","GR2","L","L1","L2","SNH")] <- "Grassland"
data$LU[data$Landuse %in% c("MFC")] <- "MassFloweringCrop"
table(data$LU)

data$lu_period <- paste0(data$LU,"_p",data$Period)

catLU <- unique(data$LU)
catLUper <- unique(data$lu_period)

# uncomment to compute summary stat on abcTable
abcTable <- abcTable %>% dplyr::select(starts_with("abundance"))

res <- data.frame()
t0 <- Sys.time()
for (j in 1:nrow(abcTable)){
  if(j%%500 == 0){
    print(j)
    print(Sys.time() - t0)
  }
  tt <- abcTable[j,]

  perLU <- lapply(catLU, FUN = function(i){
    tempData <- data[data$LU==i,]
    namesData <- paste0("abundance",tempData$dataPointID)
    #temp <- as.numeric(tt %>% dplyr::select(all_of(namesData))) # uncomment for abc tables
    temp <- tempData$simulatedNbOfBees # uncomment for one-row observed data
    c(sum(temp==0),quantile(temp,c(0.25,0.5,0.75)),mean(temp),min(temp),max(temp))
  })
  perLUPer <- lapply(catLUper, FUN = function(i){
    tempData <- data[data$lu_period==i,]
    namesData <- paste0("abundance",tempData$dataPointID)
    #temp <- as.numeric(tt %>% dplyr::select(all_of(namesData))) # uncomment for abc tables
    temp <- tempData$simulatedNbOfBees # uncomment for one-row observed data
    c(sum(temp==0),quantile(temp,c(0.25,0.5,0.75)),mean(temp),min(temp),max(temp))
  })

  all <- c(do.call(c,perLU),do.call(c,perLUPer))
  names(all) <- c(paste0(rep(catLU,each=7),"_",c("nbOf0","q25","q50","q75","mean","min","max")),
                  paste0(rep(catLUper,each=7),"_",c("nbOf0","q25","q50","q75","mean","min","max")))
  all <- as.data.frame(t(all))
  all$iteration <- j

  res <- rbind(res,all)
}

saveRDS(res,"analyses/results/summStatSet_realData.rds")
