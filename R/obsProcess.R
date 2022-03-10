#' Observation process
#'
#' Compute simulated observations from the model using a given observation process
#'
#' @name obsProcess
#' @aliases obsProcess
#' @param extPred extracted predictions from one or several run(s) of the model
#' @param size the sample size
#' @param beta the scale parameter (at the log scale, i.e. \eqn{log (\lambda^*) = log(\lambda) + \beta + ...})
#' @param cellSize the size of a cell in the rasters, in meters
#' @param fixed a list with names and values of the fixed effects to be added in the observation process.
#' Can be \code{"period"}, \code{"year"}, \code{"study"}. For example, for a fixed effect on the period,
#' where period is a three-level variables : \code{fixed=list(period=c(0,0.5,0.2))}.
#' At least one of the fixed effects should be equal to 0 and is then considered as the reference category
#' @param random a list with names of random effects to be added in the observation process. Can be
#' \code{"period"}, \code{"year"}, \code{"study"}. Gaussian distributions are assumed for random effects.
#' It takes for example the form \code{random=list(landscape=c(0,1.5))}, with 0 the mean of the random
#' effect and 1.5 the variance.
#' @param obsDist name of the (discrete) distribution linking intensity with counts. Currently only Poisson is
#' supported, as "rpois".
#' @param average boolean indicating whether one should average model predictions over the 9 neighboring cells
#' @param noiseDist name of the (continuous) distribution of the additive noise between log of real intensity
#' and log of intensity predicted by the model
#' @param noiseParams parameters for the \code{noiseDist} distribution
#' @return ...
obsProcess <- function(extPred,size,beta,cellSize=10,fixed=NULL,random=NULL,average=TRUE,obsDist="rpois",noiseDist="rnorm",noiseParams=list(mean=0,sd=1)){

  # Get intensity predicted by the model
  if (average){
    meanIntensity <- log(apply(extPred[,1:9],1,mean)) + beta
   }else{
    meanIntensity <- log(extPred$centerPoint) + beta
  }

  if (!is.null(fixed)){
    for (eff in names(fixed)){
      values <- fixed[[eff]]
      if (!0%in%values){stop(paste("At least one the fixed effect level should be 0 (reference category).
                             Please check again fixed effect",eff))}
      columnOfEff <- match(eff,tolower(names(extPred)))
      meanIntensity <- meanIntensity + values[extPred[,columnOfEff]]
    }
  }

  reAll <- numeric()
  if (!is.null(random)){
    for (eff in names(random)){
      values <- random[[eff]]
      columnOfEff <- match(eff,tolower(names(extPred)))
      levels <- unique(extPred[,columnOfEff])
      nbLevels <- length(levels)

      # Sample from random effect distribution
      re <- rnorm(nbLevels, mean = values[1], sd = sqrt(values[2]))

      meanIntensity <- meanIntensity + re[match(extPred$Landscape,levels)]

      reAll <- cbind(reAll,re)
    }
  }

  # Add the noise distribution
  epsilon <- do.call(noiseDist, c(list(n=1), noiseParams))

  meanIntensity <- meanIntensity + epsilon

  # Now, sample from the observation distribution
  if (obsDist!="rpois") stop("Currently only Poisson distribution (rpois) is supported")
  predictedObservations <- sapply(1:size,FUN = function(n){
    meanPoisson <- exp(meanIntensity)*cellSize^2/100 ## change co cellSize^2
    rpois(length(meanPoisson),as.numeric(meanPoisson))})

    df <- cbind(predictedObservations,reAll)
  df <- data.frame(df)

  return(df)
}
