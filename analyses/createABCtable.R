devtools::load_all() # load the package

# 1. Import a saved simulation object
simu <- readRDS("data/simulatedObject.rds")

# 2. Set the priors of the model parameters
fixedParams <- modelParams(listData = list(tau0=prior(distRNG="fixed",hyperParams=list(cst=1000)),
                                           f0=prior(distRNG="fixed",hyperParams=list(cst=0.1)),
                                           a=prior(distRNG="fixed",hyperParams=list(cst=1000)),
                                           b=prior(distRNG="fixed",hyperParams=list(cst=25000)),
                                           maxFlyingDist=prior(distRNG="fixed",hyperParams=list(cst=750))))
names(fixedParams) <- c("tau0","f0","a","b","maxFlyingDist")

priors <- modelParams(listData = list(tau0=prior(distRNG="rlnormTrunc",hyperParams=list(meanlog=log(1000),sdlog=1,max=1000)),
                                      f0 = prior(distRNG="rlnorm",hyperParams=list(meanlog=log(0.1),sdlog=1)),
                                      a = prior(distRNG="runif",hyperParams=list(min=100,max=1000)),
                                      b = prior(distRNG="runif",hyperParams=list(min=100,max=1000)),
                                      maxFlyingDist = prior(distRNG="fixed",hyperParams=list(cst=1000))))
names(priors) <- c("tau0","f0","a","b","maxFlyingDist")
saveRDS(priors,"data/priors_model_params.rds")

setPriors(simu) <- priors


# 3. Construct the ABC table
sizeABC <- 100 # change to a high value
abcTable <- runAndExtract(simu, landscapes = "all", size = sizeABC)
abcTable$average <- apply(abcTable[,1:9],1,mean) # mean of the 9 cells around the site

# Observation process
# First, sample from the priors of the statistical model parameters
# Scale parameters
beta <- rnorm(1,0,10)
betaPeriods <- c(0,rnorm(1,0,10),rnorm(1,0,10))

# sampling from the prior of random effect precisions if any random effects
#precisions <- rgamma(2,1,1) # for 2 random effects
#sigma <- 1/precisions

# sampling from the prior of noise variance
invsigma2 <- rgamma(1,1,1)
sigma2 <- 1/invsigma2

write.table("beta <- rnorm(1,0,10)
betaPeriods <- c(0,rnorm(1,0,10),rnorm(1,0,10))
invsigma2 <- rgamma(1,1,1)
sigma2 <- 1/invsigma2
",paste0("priors_",id,".txt"))

theta2 <- c(beta,betaPeriods,invsigma2,sigma2)
names(theta2) <- c("beta","betaPer1","betaPer2","betaPer3","invsigma2","sigma2")

# Sampling observations
predObs <- lapply(1:sizeABC, FUN = function(i){
  extPred <- abcTable[abcTable$iteration==i,]
  beta <- rnorm(1,0,10)
  betaPeriods <- c(0,rnorm(1,0,10),rnorm(1,0,10))
  invsigma2 <- rgamma(1,1,1)
  sigma2 <- 1/invsigma2
  theta2 <- c(beta,betaPeriods,invsigma2,sigma2)
  names(theta2) <- c("beta","betaPer1","betaPer2","betaPer3","invsigma2","sigma2")

  pois <- obsProcess(extPred,
             size = 1,
             beta = beta,
             fixed = list(period=betaPeriods),
             average = TRUE,
             obsDist = "rpois",
             noiseDist = "rnorm",
             noiseParams = c(mean=0,sqrt(sigma2)))
  cbind(extPred,matrix(theta2,nr=length(pois),nc=length(theta2)),pois)
})
predObs <- do.call(rbind, predObs)
names(predObs)[names(predObs)=="df"] <- "simulatedObs"
names(predObs)[names(predObs)%in%1:6] <- names(theta2)

# Merge with observations
data <- na.omit(simu@beeData)
data$dataPointID <- as.numeric(rownames(data))

abcTablefinal <- dplyr::left_join(predObs,data[,c("dataPointID","NumberOfBees")])
head(abcTablefinal)

saveRDS(abcTablefinal,"abcTable.rds")
