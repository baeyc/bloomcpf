# Posterior predictive check

devtools::load_all() # load the package
library(dplyr)
library(tidyr)
library(reshape2)

# 1. Import a saved simulation object (it contains the floral and nesting maps, for each study)
simu <- readRDS("data/simulatedObject.rds")

method <- "Iterated nonlinear adjustment 2.5%"

# 5. Set priors based on ABC posterior distribution
ABCpostsamples <- readRDS("analyses/results/results_samples_realdata.rds") %>%
  filter(type==method) %>% select(!type)

sampleSize <- nrow(ABCpostsamples)/length(unique(ABCpostsamples$param))
ABCpostsamples$id <- rep(1:sampleSize,8)

pred_obs <- data.frame()
for (i in 1:sampleSize){
  print(i)
  params_CPF <- ABCpostsamples %>% filter(id==i & param %in% c("tau0","f0","a","b")) %>% select(sample)
  params_CPF <- c(unlist(params_CPF),1000)
  names(params_CPF) <- c("tau0","f0","a","b","maxFlyingDist")

  modelRuns <- runAndExtract(simu,landscapes="all",size=1,params=params_CPF)
  modelRuns$average <- apply(modelRuns[,1:9],1,mean) # mean of the 9 cells around the site

  # Observation process
  beta <- unlist(ABCpostsamples %>% filter(id==i & param %in% c("beta")) %>% select(sample))
  betaPeriods <- unlist(ABCpostsamples %>% filter(id==i) %>% filter(grepl("betaPer",param)) %>% select(sample))

  sigma2 <- as.numeric(ABCpostsamples %>% filter(id==i & param %in% c("sigma2")) %>% select(sample))
  invsigma2 <- 1/sigma2

  # Sampling observations
  observations <- obsProcess(modelRuns,
                            size = 1,
                            beta = beta,
                            fixed = list(period=c(0,betaPeriods)),
                            average = TRUE,
                            obsDist = "rpois",
                            noiseDist = "rnorm",
                            noiseParams = c(mean=0,sd=sqrt(sigma2)))

  observations$dataPointID <- modelRuns$dataPointID
  observations$sample <- i
  pred_obs <- rbind(pred_obs,observations)
}

saveRDS(pred_obs,paste0("post_pred",method,".rds"))
