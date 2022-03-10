library(abc)
library(abctools)
library(tidyr)
library(dplyr)
library(caret)
library(here)
library(e1071)
library(ggplot2)
library(RColorBrewer)
library(EnvStats)
library(ggridges)
library(invgamma)
library(abcrf)
library(gbm)

devtools::load_all() # load the package

# Load abc Table
simuSumStat <- readRDS(here::here("analyses/results/summStatSet_abcTable.rds")) %>%
  dplyr::select(!starts_with("prob") & !starts_with("sum") & !iteration)

params <- readRDS(here::here("analyses/results/","params.rds")) %>%
  dplyr::select(!ends_with("Iteration"))

output_dir <- paste0("analyses/results/")


# Delete non-varying summary statistics
nonZeroVar <- nearZeroVar(simuSumStat, saveMetrics = TRUE)
namesNonZeroVar <- rownames(nonZeroVar)[!nonZeroVar$zeroVar]
simuSumStat <- simuSumStat %>% dplyr::select(all_of(namesNonZeroVar))


# Select 100 sets to act as "true data"
set.seed(1234)
idsim <- sample(1:nrow(simuSumStat), size=100)
saveRDS(indices,"analyses/results/indices.rds")

idsim <- sort(readRDS("analyses/results/indices.rds"))

obsSumStat <- split(simuSumStat[idsim,],seq(nrow(simuSumStat[idsim,])))
simuSumStat <- simuSumStat[-idsim,]

paramsSimu <- params[-idsim,]
paramsObs <- params[idsim,]

saveRDS(obsSumStat,paste0("analyses/results/simuData_set_of_100.rds"))
saveRDS(paramsObs,paste0("analyses/results/params_set_of_100.rds"))


# Define transformation to be applied to parameters
names_transf <- c("logit","log","logit","logit","none","none","none","log")
bounds_transf <- rbind(c(0,1000),c(NA,NA),
                       c(100,1000),c(100,1000),
                       c(NA,NA),c(NA,NA),
                       c(NA,NA),c(NA,NA))


results <- lapply(1:length(obsSumStat), FUN = function(i){
  # Rejection methods
  rej5 <- abc(obsSumStat[[i]],paramsSimu,simuSumStat,tol=0.05,method="rejection")
  rej25 <- abc(obsSumStat[[i]],paramsSimu,simuSumStat,tol=0.025,method="rejection")

  # Local linear approaches
  locHl5 <- abc(obsSumStat[[i]],paramsSimu,simuSumStat,tol=0.05,method="loclinear")
  locHl25 <- abc(obsSumStat[[i]],paramsSimu,simuSumStat,tol=0.025,method="loclinear")
  locHl5_tr <- abc(obsSumStat[[i]],paramsSimu,simuSumStat,tol=0.05,method="loclinear",transf=names_transf,logit.bounds=bounds_transf)

  # Local nonlinear approaches
  nlh5 <- abc(obsSumStat[[i]],paramsSimu,simuSumStat,tol=0.05,method="neuralnet",MaxNWts=10000,transf=names_transf,logit.bounds=bounds_transf)
  nlh25 <- abc(obsSumStat[[i]],paramsSimu,simuSumStat,tol=0.025,method="neuralnet",MaxNWts=10000,transf=names_transf,logit.bounds=bounds_transf)

  # Adaptice nonlinear approach
  m <- nlh25
  estim_densSupp <- e1071::svm(~.,data = m$adj.values, gamma = 0.1) # density support estimation -> predict which points are in the conditional prior density support
  in_density_support <- predict(estim_densSupp) # get id of points in the density support
  samples_is <- m$adj.values[in_density_support,] # select points in the density support
  simuSumStat_is <- simuSumStat[m$region,]
  simuSumStat_is <- simuSumStat_is[in_density_support,]
  nonZeroVar <- nearZeroVar(simuSumStat_is, saveMetrics = TRUE)
  namesNonZeroVar <- rownames(nonZeroVar)[!nonZeroVar$zeroVar]
  simuSumStat_is <- simuSumStat_is %>% dplyr::select(all_of(namesNonZeroVar))
  obsSumStat_is <- obsSumStat[[i]] %>% dplyr::select(all_of(namesNonZeroVar))
  nnmethod_25_is <- abc(obsSumStat_is,samples_is,simuSumStat_is,tol=1,method="neuralnet",MaxNWts=10000,
                        transf=names_transf,
                        logit.bounds=bounds_transf)

  m <- nlh5
  estim_densSupp <- e1071::svm(~.,data = m$adj.values, gamma = 0.1) # density support estimation -> predict which points are in the conditional prior density support
  in_density_support <- predict(estim_densSupp) # get id of points in the density support
  samples_is <- m$adj.values[in_density_support,] # select points in the density support
  simuSumStat_is <- simuSumStat[m$region,]
  simuSumStat_is <- simuSumStat_is[in_density_support,]
  nonZeroVar <- nearZeroVar(simuSumStat_is, saveMetrics = TRUE)
  namesNonZeroVar <- rownames(nonZeroVar)[!nonZeroVar$zeroVar]
  simuSumStat_is <- simuSumStat_is %>% dplyr::select(all_of(namesNonZeroVar))
  obsSumStat_is <- obsSumStat[[i]] %>% dplyr::select(all_of(namesNonZeroVar))
  nnmethod_5_is <- abc(obsSumStat_is,samples_is,simuSumStat_is,tol=1,method="neuralnet",MaxNWts=10000,
                       transf=names_transf,
                       logit.bounds=bounds_transf)


  # Extract mean, median and quantiles from samples
  dcomp_quant <- data.frame()
  for (p in names(params)){
    # Extract ABC quantiles for other methods
    rej5p <- rej5$unadj.values[,p]
    rej25p <- rej25$unadj.values[,p]
    locHl5p <- locHl5$adj.values[,p]
    locHl25p <- locHl25$adj.values[,p]
    locHl5_trp <- locHl5_tr$adj.values[,p]
    nlh25p <- nlh25$adj.values[,p]
    nlh5p <- nlh5$adj.values[,p]
    nn_25_is <- nnmethod_25_is$adj.values[,p]
    nn_5_is <- nnmethod_5_is$adj.values[,p]

    d <- data.frame(sample=c(rej5p,rej25p,locHl5p,locHl25p,locHl5_trp,nlh5p,nlh25p,nn_25_is,nn_5_is),
                    type=c(rep("Rejection 5%",length(rej5p)),rep("Rejection 2.5%",length(rej25p)),
                           rep("Linear adjustment 5%",length(locHl5p)),rep("Linear adjustment 2.5%",length(locHl25p)),
                           rep("Linear adjustment 5% with transformed parameters",length(locHl5_trp)),
                           rep("Nonlinear adjustment 5%",length(nlh5p)),rep("Nonlinear adjustment 2.5%",length(nlh25p)),
                           rep("Iterated nonlinear adjustment 2.5%",length(nn_25_is)),rep("Iterated nonlinear adjustment 5%",length(nn_5_is))))

    d <- d %>% group_by(type) %>% dplyr::summarise(across(.cols = everything(),
                                                   .fns = list(mean=~mean(.x,na.rm = TRUE),
                                                               median=~median(.x,na.rm = TRUE),
                                                               q2.5 = ~quantile(.x,0.025,na.rm=TRUE),
                                                               q97.5 = ~quantile(.x,0.975,na.rm=TRUE))))
    names(d) <- c("type","mean","median","q2.5","q97.5")
    d$param <- p
    dcomp_quant <- rbind(dcomp_quant,d)
  }


  # Compute weights for Random forests using Epanechnikov kernel
  Wepanech <- apply(simuSumStat,1,FUN=function(v){sum((v-obsSumStat[[i]])^2)})
  quW <- quantile(Wepanech,0.05)
  W <- ifelse(Wepanech<=quW,(3/4)*(1-Wepanech/quW),0)

  # Random forests
  list_results <- lapply(names(params), FUN = function(p){
    dataABCRF <- data.frame(param=paramsSimu[,p],sumsta=simuSumStat)
    resABCRF <- regAbcrf(formula=param~.,data=dataABCRF, ntree=500, verbose = F, case.weight=W)
    resABCRFnoW <- regAbcrf(formula=param~.,data=dataABCRF, ntree=500, verbose = F)

    names(obsSumStat[[i]]) <- colnames(dataABCRF[,-1])
    pred <- predict(object = resABCRF, obs=obsSumStat[[i]], training = dataABCRF, quantiles = c(0.025,0.5,0.975), paral = T, verbose=F)
    prednoW <- predict(object = resABCRFnoW, obs=obsSumStat[[i]], training = dataABCRF, quantiles = c(0.025,0.5,0.975), paral = T, verbose=F)
    #pred$param <- p
    predall <- as.data.frame(rbind(unlist(pred),unlist(prednoW)))
    predall$weights <- c("w","noW")
    return(predall)
  })

  resRF <- as.data.frame(do.call(rbind,list_results))
  resRF$param <- rep(names(params),each=2)
  resRF <- resRF %>% select(expectation,med,quantiles1,quantiles3,param)
  names(resRF) <- c("mean","median","q2.5","q97.5","param")
  resRF$type <- rep(c("weighted random forest","unweighted random forest"),length(names(params)))

  resall <- data.frame()

  ech <- simuSumStat[which(W>0),]
  nonZeroVar <- nearZeroVar(ech, saveMetrics = TRUE)
  namesNonZeroVar <- rownames(nonZeroVar)[!nonZeroVar$zeroVar]
  ech <- ech %>% dplyr::select(all_of(namesNonZeroVar))
  obsSumStat[[i]] <- obsSumStat[[i]] %>% dplyr::select(matches(namesNonZeroVar))

  # Boosting methods
  for (paramName in names(params)){
    fit_0.025 <- gbm.fit(x=ech, y=paramsSimu[which(W>0),paramName], distribution = list(name="quantile", alpha=0.025), verbose=F)
    fit_0.5 <- gbm.fit(x=ech, y=paramsSimu[which(W>0),paramName], distribution = list(name="quantile", alpha=0.5), verbose=F)
    fit_0.975 <- gbm.fit(x=ech, y=paramsSimu[which(W>0),paramName], distribution = list(name="quantile", alpha=0.975), verbose=F)
    fit_mean_l2 <- gbm.fit(x=ech, y=paramsSimu[which(W>0),paramName], distribution = "gaussian", w=W, verbose=F)
    fit_mean_l1 <- gbm.fit(x=ech, y=paramsSimu[which(W>0),paramName], distribution = "laplace", w=W, verbose=F)

    pred_0.025 <- predict.gbm(fit_0.025, newdata=obsSumStat[[i]])
    pred_0.5 <- predict.gbm(fit_0.5, newdata=obsSumStat[[i]])
    pred_0.975 <- predict.gbm(fit_0.975, newdata=obsSumStat[[i]])
    pred_mean_l2 <- predict.gbm(fit_mean_l2, newdata=obsSumStat[[i]])
    pred_mean_l1 <- predict.gbm(fit_mean_l1, newdata=obsSumStat[[i]])

    temp <- data.frame(param=paramName,q025=pred_0.025,med=pred_0.5,q975=pred_0.975,meanl2=pred_mean_l2,meanl1=pred_mean_l1)
    resall <- rbind(resall,temp)
  }

  resall_l1 <- resall %>% select(param,q025,med,q975,meanl1)
  resall_l2 <- resall %>% select(param,q025,med,q975,meanl2)
  names(resall_l1) <- names(resall_l2) <- c("param","q2.5","median","q97.5","mean")
  resall_l1$type <- "Gradient boosting with L1 loss"
  resall_l2$type <- "Gradient boosting with L2 loss"

  all_results <- full_join(dcomp_quant,full_join(resRF,full_join(resall_l1,resall_l2)))
  all_results$sample <- i

  results[[i]] <- all_results
})


saveRDS(results,"results_simuData.rds")
