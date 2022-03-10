#' Create nesting map
#'
#' Create map with nesting values depending on the landuse category of each cell in the landscape.
#'
#' @name createNestMap
#' @aliases createNestMap
#' @param landscape a raster containing the landscape structure, with each pixel falling into one landuse category
#' @param catNames the names of the land use categories
#' @param nestVal a data frame containing two variables: \code{catName} the name of the landuse category and \code{nestval} the corresponding
#' nesting values as an ordered factor variable, with the lower level corresponding to the lower nesting value.
#' @return a raster of the same dimension as \code{landscape}, filled in with nesting values depending on the land use category

createNestMap <- function(landscape,catNames,nestValues){
  # Check that the parameter files contain information about all the categories found in the landscape
  catNamesInFile <- unique(nestValues$catName)
  if (sum(catNames %in% catNamesInFile) == 1) stop("Some categories appearing in the landscape are not in the nestValues data frame. Please complete")

  # create the floral map
  nestMap <- matrix(0,nr=nrow(landscape),nc=ncol(landscape))
  nbcat <- length(catNames)

  for (i in 1:nbcat){
    mask <- raster::as.matrix(landscape==i)
    nestMap <- nestMap + nestValues$nestval[i]*mask
  }

  return(raster::raster(nestMap))
}
