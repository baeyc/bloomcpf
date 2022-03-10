#' Create floral map
#'
#' Create map with floral values depending on the landuse category of each cell in the landscape.
#'
#' @name createFloralMap
#' @aliases createFloralMap
#' @param landscape a raster containing the landscape structure, with each pixel falling into one landuse category
#' @param catNames the names of the land use categories
#' @param method the method used to fill in the floral map: either \code{random} to generate randomly a different floral value in each cell, according
#' to a given probability distribution whose parameters depend on the land use category, or \code{fixed} to attribute the same floral value to all cells
#' of the same land use category
#' @param params the parameters to be used, given as a data frame with the following columns:
#' \code{catName} the name of the land use category, \code{distName} the name of the probability distribution which is assumed to depend on a maximum of
#' 2 parameters (when \code{method="random"}), \code{param1} and \code{param2} the values of those two parameters (when \code{method="random"}),
#' \code{flValue} the associated floral value (when \code{method="fixed"}).
#' The random distribution should return a number between 0 and 1. Currently, only \code{runif} and \code{rbeta} are supported.
#' @return a raster of the same dimension as \code{landscape}, filled in with floral values (between 0 and 1) depending on the land use category

createFloralMap <- function(landscape,catNames,method,param=NULL){
  if (method=="random" & is.null(param)) paste("Please specify a filename for the random distribution parameters")

  # Check that the parameter files contain information about all the categories found in the landscape
  if (method=="random"){
    catNamesInFile <- unique(param$catName)
    if (sum(catNames %in% catNamesInFile) == 1) stop("Some categories appearing in the landscape are not in the param data frame. Please complete")
  }
  if (method=="fixed"){
    catNamesInFile <- unique(param$catName)
    if (sum(catNames %in% catNamesInFile) == 1) stop("Some categories appearing in the landscape are not in the param data frame. Please complete")
  }

  # create the floral maps
  florMap <- matrix(0,nr=nrow(landscape),nc=ncol(landscape))
  nbcat <- length(catNames)

  for (i in 1:nbcat){
    mask <- raster::as.matrix(landscape==i)
    if (method=="fixed") florMap <- florMap + param$flValue[i]*mask
    if (method=="random"){
      distrib <- as.character(param$distName[i])
      a <- param$param1[i]
      b <- param$param2[i]
      if ( (distrib=="runif" & (a!=b)) | (distrib=="rbeta")){
        mask[mask==T] <- matrix(eval(parse(text=distrib))(sum(mask),a,b),nr=nrow(florMap),nc=ncol(florMap))
        florMap <- florMap + mask
      }else if (distrib=="runif" & (a==b)){
        florMap <- florMap + a*mask
      }else{
        stop("Please check the random parameters for the generation of floral maps. It should be runif(a,b) with a<=b or rbeta")
      }
    }
  }

  return(raster::raster(florMap))
}
