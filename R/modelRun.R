#' Run simulations from a model
#'
#' Run simulations from a given model, at a given time and in given landscapes
#'
#' @name run
#' @aliases run
#' @param time a list containing the temporal information, as produced by function \code{importDatesPeriods}
#' @param data a list containing the floral and bee data
#' @param landscape a list containing the spatial and landscape information
#' @param flowermodel the name of the flower data model (currently, the only case covered is \code{joint}, which means that flower data are modeled jointly
#' with the pollination model. Later cases will include the possibility to fit a flower model prior to running the pollination model)
#' @param beemodel the name of the pollination model to be used (currently, either \code{cpf} for the central-place foragers model of Olsson et al. (2015)
#' or \code{diff} for the diffusion model of Haussler et al. (2016))
#' @param params a list containing the names and values of the model parameters
#' @return a run of the model for the chosen dates and landscapes, given the input set of parameters

run <- function(time=list(years=c(2011,2012),
                          periods=matrix(c("15-05-2011","15-06-2011",
                                           "15-06-2011","15-07-2011",
                                           "15-05-2012","15-06-2012",
                                           "15-06-2012","15-07-2012"),nc=2,byrow = T)),
                data=list(floral)
){

}


