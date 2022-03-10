#' Class "simuParams"
#'
#' An object of the \code{simuParams} class, storing the parameters of the simulation
#'
#' @name simuParams-class
#' @docType class
#' @aliases simuParams-class simuParams
#' @slot studies a list of the study(ies) to be included in the simulation
#' @slot aggregateFl a logical value indicating whether flower data should be aggregated accross species
#' @slot beeSpecies a list with the name(s) of the bee species to be included
#' @slot randomFloral a logicial indicating whether floral maps should be generated using random values
#' @slot floralValues a data frame with the floral values to be used to generate the floral maps
#' @slot nestingValues a data frame with the nesting values to be used to generate the nesting maps
simuParams <- setClass("simuParams",
                          slots = c(studies="character",
                                    aggregateFl="logical",
                                    beeSpecies="character",
                                    randomFloral="logical",
                                    floralValues="list",
                                    nestingValues="list"
                                    ),

                          # Set the default values for the slots
                          prototype=list(
                            studies=c("COST","STEP"),
                            aggregateFl=TRUE,
                            beeSpecies="Bombus terrestris",
                            randomFloral=FALSE,
                            floralValues=read.table("data/fixedFlValues.txt",header=T,sep=","),
                            nestingValues=data.frame(catName=read.table("data/nestValues.txt",header=T,sep=",")[,1],
                                                     nestval=read.table("data/nestValues.txt",header=T,sep=",")[,2])
                          ),

                          validity=function(object)
                          {
                            if (object@randomFloral & !("distName" %in% names(object@floralValues))) stop("You choose random generation of floral values but the
                                                                                            floral value data frame does not contain information about
                                                                                            the random distributions to be used.")
                            if (!object@randomFloral & ("distName" %in% names(object@floralValue))) stop("You choose fixed generation of floral values but you provided
                                                                                           random distributions in the floral value data frame.")
                            return(TRUE)
                          }
)


#' @title setFloralValues
#' @export
"setFloralValues<-" <- function(object, value){ object }
setGeneric("setFloralValues<-")
setMethod("setFloralValues<-",
          signature(object = "simuParams"),
          function(object,value){
            object@floralValues <- value
            object
          })

#' @title setNestingValues
#' @export
"setNestingValues<-" <- function(object, value){ object }
setGeneric("setNestingValues<-")
setMethod("setNestingValues<-",
          signature(object = "simuParams"),
          function(object,value){
            object@nestingValues <- value
            object
          })
