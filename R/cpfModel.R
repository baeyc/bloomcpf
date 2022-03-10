#' Main function of the CPF model (Central-Place Foragers model)
#'
#' Main function of the CPF model, to be run on a given landscape (i.e. one given period
#' of a given year)
#'
#' @name cpfModel
#' @aliases cpfModel
#' @param tau0 : the species-specific asymptote of the isocline
#' @param f0 : the species-specific minimum floral value that will be visited by bees
#' @param a : the rate parameter for computing individual isocline asymptote from \code{tau0}
#' @param floralMat : a matrix (and not a raster) of floral values
#' @param nestMat : a matrix (and not a raster) of nesting values
#' @param cellSize : the size (in meters) of a cell
#' @param maxFlyingDist : the maximum distance bees can fly (in meters)
#' @return a raster with the intensity of visitation rates
cpfModel <- function(tau0,
                     f0,
                     a,
                     b,
                     floralMat,
                     nestMat,
                     cellSize,
                     maxFlyingDist){
  # Create a matrix indicating where the nests are
  Mnest01 = 1*(nestMat>0)

  uA <- unique(as.vector(floralMat))
  uA <- uA[uA>0]

  # calculate window for filtering
  maxNbCells <- floor(maxFlyingDist/cellSize)
  if (maxNbCells%%2 == 0) maxNbCells <- maxNbCells+1 # make sure it is an odd number
  ij <- cbind(1:maxNbCells, rep(1:maxNbCells, each= maxNbCells))
  dist <- cellSize*matrix(sqrt((ij[,1] - (maxNbCells + 1)/2) ^ 2 + (ij[,2] - (maxNbCells + 1)/2) ^ 2),ncol=maxNbCells, nrow = maxNbCells)

  # Calculate D_ij, the distance between each points with coordinates (floral value in cell j, distance from cell i to cell j)
  # and the species-specific isocline -> it tells us how far we are from the species isocline (which is some kind of biological threshold)
  floralMatA=floralMat*0;
  Dlist <- lapply(1:length(uA),FUN=function(j){
    floralMatA[floralMat==uA[j]]=1;
    h=tau0*(1-f0/uA[j])-dist;
    h[h<0]=0;
    # here we just average h for each value of uA -> we get the sum of D_ij for all cells j such that uA=uA[j]
    g=EBImage::filter2(floralMatA,h);
    #g=g*Mnest01;
    g[g<0]=0;
    #D=g;
    g
    })

  # Suitability: sum of all the D_ij for all floral values
  S <- Reduce('+',Dlist)

  # Compute individual isocline asymptote
  tau0i <- tau0/(1+exp((sqrt(S)-a)/b))
  #tau0i <- tau0*exp(-a*S^b)

  # Calculate intensity of visitation rates
  out = binValues(log(tau0i+1),nb=15)
  bin = out$bins
  eT = exp(unique(as.vector(out$discM)))-1

  sumIntensity <- lapply(1:length(eT), FUN=function(j){
    # discretize nesting matrix to work per binned value
    nestDisc <- 0*bin
    nestDisc[bin==j] <- nestMat[bin==j] #the amount of nests in the chosen cells
    pv = 0*bin
    pv <- lapply(1:length(uA), FUN=function(i){
      ht=(eT[j]-f0/uA[i]-dist)
      ht[ht<0]=0;
      ht <- ht/ifelse(sum(ht)>0,sum(ht),1)
      if(sum(ht)>0){
        pv_temp = EBImage::filter2(nestDisc,ht);
        pv_temp[pv_temp<0]=0
      }else{
        pv_temp <- 0*floralMat
      }
      return(pv_temp)
    })
    intensity <- Reduce('+',pv)
    return(intensity)
  })
  intensity <- Reduce('+',sumIntensity)
  intensity <- intensity/1000#/(cellSize^2*length(floralMat)) # intensity per unit surface

  return(lambda=intensity)
}


binValues <- function(m,nbBins){
  # Rounding the values inside the matrix
  m = round(m,4)
  ftemp = unique(as.vector(m))
  nbCat = length(ftemp)
  gc()
  out = kmeans(m[is.finite(m)],centers=min(nbBins,nbCat),nstart=10,iter.max = 100)
  newValuesm = out$centers
  binCluster = m*0
  binCluster[is.finite(m)] = out$cluster
  discm = m*0
  discm[is.finite(m)] = newValuesm[binCluster]
  discm[!is.finite(m)] <- 0
  binCluster[!is.finite(m)]<-NA
  return(list(discM = discm, bins = binCluster))
}

computeDelta <- function(floralMat,taumax){
  floralMatA=floralMat*0;
  uA <- unique(as.vector(floralMat))
  Dlist <- lapply(1:length(uA),FUN=function(i){
    floralMatA[floralMat==uA[i]]=1;
    h=taumax*(1-f0/uA[i])-dist;
    h[h<0]=0;
    g=EBImage::filter2(floralMatA,h); # here we just average h for each value of uA -> we get sum_i D_ij
    g[g<0]=0;
    g
  })
  return(Dlist)
}
