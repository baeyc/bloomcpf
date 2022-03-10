#' Import landuse categories coding
#'
#' Import the landuse categories coding that was used to produce the rasters, from a file. It makes the correspondance between the original land use
#' cover classification (e.g. as given by the National Swedish Land Cover Database ,Nationella MarktäckeData (NMD)), and the classification used to
#' produce the rasters (usually with some classes being gathered).
#'
#' @name importLUcat
#' @aliases importLUcat
#' @param filename the name of the (csv) file containing all the original land use types and the new land use classes used in the rasters. Default
#' is \code{"data/landuseCategories.csv"}.
#' It should contains the following variables : \code{Land.Use} the name of the land use category, and \code{q} other
#' columns, corresponding to the \code{q} new categories appearing in the rasters.
#' @return a list with the following objects: \code{newCatNames} the names of the categories used in the rasters, \code{oldToNew} a list of the same size
#' as the original LU categories, and giving the new associated category , and \code{newToold}, a list of size equal to the number of new land use categories
#' and giving all the original land use codes falling into each new land use category

importLUcat <- function(filename="data/landuseCategories.csv"){
  lucat <- read.csv(filename)
  catNames <- colnames(lucat)[-1]

  newToOld <- lapply(1:length(catNames),FUN = function(i){temp <- lucat$NMDblockcode[lucat[,i+1]==1]; temp[!is.na(temp)]})
  names(newToOld) <- catNames

  oldToNew <- lapply(unique(lucat$NMDblockcode),FUN = function(i){temp <- lucat[lucat$NMDblockcode==i,-1]; row.names(na.omit(t(temp)))})
  names(oldToNew) <- unique(lucat$NMDblockcode)

  return(list(newCatNames=catNames,oldToNew=oldToNew,newToOld=newToOld))
}
