#' Class "modelParams"
#'
#' An object of the \code{modelParams} class, storing the parameters to be fitted
#'
#' @name modelParams-class
#' @docType class
#' @aliases modelParams-class modelParams
#' @slot tau0 the asymptote of the isocline function
#' @slot f0 the minimum floral value that will be visited by bees
#' @slot a the scale parameter used in the fitness function (see @details)
#' @slot maxFlyingDist the maximum flying distance (fixed)
#' @slot priors a list with the priors for all the parameters, where the names of the list elements are
#' the names of the parameters
#' @details the isocline function is defined as ...
#' An object of the modelParams class contains the following slots:

modelParams <- setClass("modelParams",
                       contains="SimpleList",
                       # Set the default values for the slots
                       prototype=prototype(elementType="prior"))
