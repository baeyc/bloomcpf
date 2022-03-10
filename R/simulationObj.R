#' Class "simulationObj"
#'
#' An object of the \code{simulationObj} class, storing the settings of the simulation
#'
#' @name simulationObj-class
#' @docType class
#' @aliases simulationObj-class simulationObj
#' @slot time a data frame containing the temporal information (years and periods)
#' @slot floralData a data frame containing the floral data, with the location of the sampling, the landuse, the year, the period and the observed
#' floral coverage for each of these characteristics, and optionally the landscape ID
#' @slot beeData a data frame containing the bee data, with the location of the sampling, the landuse, the year, the period and the observed
#' bee abundance for each of these characteristics, and optionally the landscape ID
#' @slot model the name of the model used to run the simulation, either \code{cpf} for the central-place foragers model of Olsson et al. (2015)
#' or \code{diff} for the diffusion model of Haussler et al. (2016)
#' @slot params a list of (possibly several) parameters values
#' @slot landscapes a list of the raster maps
#' @slot lucat the landuse categories
#'
#' @import EnvStats
#' @references Olsson, O. et al., 2015. Modeling pollinating bee visitation rates in heterogeneous landscapes from foraging theory
#' \emph{Ecological Modelling}, 316, 133-143.
#' @references Haussler, J et al., 2017. Predicting pollinator capital and pollination service responses to enhancing floral and
#' nesting resources. \emph{Ecology and evolution}, 7(6):1898-1908.
simulationObj <- setClass("simulationObj",
                          slots = c(time="list",
                                    floralData="list",
                                    beeData="list",
                                    model="character",
                                    landscapes="list",
                                    floralMaps="list",
                                    nestMaps="list",
                                    yearRasterID="list",
                                    lucat="list",
                                    simuParams="simuParams",
                                    modelParams="modelParams"),
                          #obsParams="obsParams"),

                          # Set the default values for the slots
                          prototype=list(
                          ),

                          validity=function(object)
                          {
                            return(TRUE)
                          }
)


#' @title initialize
#' @describeIn initialize Initialize a simulation object
#' @param studies the list of the studies to be included in the simulation
#' @param aggrFl a logical value indicating whether floral values should be aggregated (TRUE) or detailed at the species level (FALSE)
#' @param beeSpecies a character indicating the bee species to be considered in the simulation
#' @exportMethod initialize
#'
setMethod("initialize",
          "simulationObj",
          function(.Object, studies, aggregateFloral, beeSpecies){
            .Object@simuParams@studies <- studies
            .Object@simuParams@aggregateFl <- aggregateFloral
            .Object@simuParams@beeSpecies <- beeSpecies
            .Object@model <- "cpf"
            return(.Object)
          })

#' @title setTime
#' @export
"setTime<-" <- function(object, value){ object }
setGeneric("setTime<-")
setMethod("setTime<-", signature(object = "simulationObj"), function(object, value) {
  object@time <- value
  object@floralData <- importFlowerData(object@time,object@simuParams@studies,!object@simuParams@aggregateFl)
  object@beeData <- importBeeData(object@time,object@simuParams@studies,object@simuParams@beeSpecies)
  object
})


#' @title setModel<-
#' @export
"setModel<-" <- function(object, value){ object }
setGeneric("setModel<-")
setMethod("setModel<-", signature(object = "simulationObj"), function(object, value) {
  object@model <- value
  object
})


#' @title setPriors
#' @export
#'
"setPriors<-" <- function(object, value){ object }
setGeneric("setPriors<-")
setMethod("setPriors<-", signature(object = "simulationObj"), function(object, value) {
  object@modelParams <- value
  object
})

#' @title setLUcat
#' @export
#'
"setLUcat<-" <- function(object, value){ object }
setGeneric("setLUcat<-")
setMethod("setLUcat<-", signature(object = "simulationObj"), function(object, value) {
  object@lucat <- value
  object
})


# #' @name setFloralValues<-
# #' @title setFloralValues<-
# #' @describeIn Import all the maps needed for the simulation (landscapes, floral and nesting values maps)
# #' @rdname setFloralValues-methods
# #' @exportMethod setFloralValues<-
#"setFloralValues<-" <- function(object, value){ object }
#setGeneric("setFloralValues", function(object){standardGeneric("setFloralValues")})
#setMethod("setFloralValues<-",
#          "simulationObj",
#          function(object,value){
#            object@simuParams@floralValues <- value
#            object
#          })


# #' @name setNestingValues<-
# #' @title setNestingValues<-
# #' @describeIn Import all the maps needed for the simulation (landscapes, floral and nesting values maps)
# #' @rdname setNestingValues-methods
# #' @exportMethod setNestingValues<-
#"setNestingValues<-" <- function(object, value){ object }
#setGeneric("setNestingValues", function(object){standardGeneric("setNestingValues")})
#setMethod("setNestingValues<-",
#         "simulationObj",
#         function(object,value){
#           object@simuParams@nestingValues <- value
#           object
#         })

setGeneric("getYears", function(object){standardGeneric("getYears")})
setGeneric("getPeriods", function(object){standardGeneric("getPeriods")})
setGeneric("importMaps", function(object,saveFloral){standardGeneric("importMaps")})
setGeneric("updateFloralMaps", function(object){standardGeneric("updateFloralMaps")})
setGeneric("getLandscape", function(object){standardGeneric("getLandscape")})
setGeneric("plotMaps", function(object,id,ggplot=FALSE){standardGeneric("plotMaps")})
setGeneric("run", function(object,landscapes,params=NULL){standardGeneric("run")})
setGeneric("extractPredictions", function(object,modelRun){standardGeneric("extractPredictions")})
setGeneric("runAndExtract", function(object,landscapes,size=1,params=NULL){standardGeneric("runAndExtract")})


#' @title getYears
#' @describeIn getYears Import all the maps needed for the simulation (landscapes, floral and nesting values maps)
#' @exportMethod getYears
setMethod("getYears",
          "simulationObj",
          function(object){
            years <- unique(object@time$Year)
            return(years)
          })


#' @title getPeriods
#' @describeIn getPeriods Import all the maps needed for the simulation (landscapes, floral and nesting values maps)
#' @exportMethod getPeriods
setMethod("getPeriods",
          "simulationObj",
          function(object){
            per <- lapply(unique(object@time$Year),FUN = function(y){unique(object@time$Period[object@time$Year==y])})
            names(per) <- unique(object@time$Year)
            return(per)
          })


#' @title importMaps
#' @describeIn importMaps Import all the maps needed for the simulation (landscapes, floral and nesting values maps).
#' @exportMethod importMaps
setMethod("importMaps",
          "simulationObj",
          function(object,saveFloral){
            if (nrow(object@beeData) == 0) stop("There is no data in your simulation. Please use setTime to trigger data importation.")

            years <- getYears(object)
            periods <- getPeriods(object)
            load("data/info.Rdata") # load the data frame making the connection between landscapes ID and raster ID
            load("data/all.Rdata") # load the data frame making the connection between landscapes ID and raster ID
            info <- info[info$Year%in%years,]
            all.sp <- all.sp[all.sp@data$Year%in%years,]
            if (!("rasterID" %in% names(object@beeData))){
              beeDataAndCoords <- merge(object@beeData,all.sp@data,by.x=c("Year","Landscape","Landuse","Source"),by.y=c("Year","landscapeID","LU","source"),all.x=T,all.y=T)
              colnames(beeDataAndCoords)[colnames(beeDataAndCoords)=="V1"] <- "x"
              colnames(beeDataAndCoords)[colnames(beeDataAndCoords)=="V2"] <- "y"
              beeDataAndCoords$x_SWEREF99 <- beeDataAndCoords$y_SWEREF99 <- NULL
              object@beeData <- beeDataAndCoords
            }#else, it means that function importMaps has already been called -> there is no need to re-merge the data
            object@beeData$rasterID <- NULL

            # now, merging with info data frame contaning the raster ID
            object@beeData<- merge(object@beeData,info,by.x=c("Year","Landscape"),by.y=c("Year","landscapeID"))
            object@beeData$x_SWEREF99 <- object@beeData$y_SWEREF99 <- NULL
            colnames(object@beeData)[colnames(object@beeData)=="ID"] <- "rasterID"

            object@yearRasterID <- unique(object@beeData[,c("Year","Landscape","rasterID")])
            #object@landscapes
            cat("Importing rasters...\n")
            ll <- lapply(unique(object@beeData$rasterID),FUN = function(i){
              r <- raster::raster(paste0("data/generated_rasters/",i,"_raster.tif"))
              cellSize <- raster::res(r)
              if (cellSize[1] != cellSize[2]) stop(paste("Resolution must be the same in both x and y directions. Check again raster with ID"))
              return(r)
            })
            names(ll) <- unique(object@beeData$rasterID)

            # Check consistency between maps
            spatialChar <- lapply(1:length(ll),FUN = function(i){
              data.frame(resoX=raster::res(ll[[i]])[1],resoY=raster::res(ll[[i]])[2],crs=as.character(ll[[i]]@crs))
            })
            spatialChar <- do.call(rbind,spatialChar)
            uniqResX <- unique(spatialChar$resoX)
            uniqResY <- unique(spatialChar$resoX)
            uniqCRS <- unique(spatialChar$crs)
            if (length(uniqResX)>1) stop("Spatial resolution must be the same for all landscapes. Please check again the rasters")
            if (length(uniqResY)>1) stop("Spatial resolution must be the same for all landscapes. Please check again the rasters")
            if (length(uniqCRS)>1) stop("Coordinate system must be the same for all landscapes. Please check again the rasters")

            if (saveFloral){
              dir.create("data/generated_floralmaps")
              dir.create("data/generated_nestingmaps")
              cat("Creating floral maps... This may take a few minutes.\n")
              method <- ifelse(object@simuParams@randomFloral,"random","fixed")
              lapply(1:length(ll),FUN = function(i){
                #Landscape <- object@yearRasterID$Landscape[object@yearRasterID$rasterID==names(object@landscapes)[i]]
                year <- object@yearRasterID$Year[object@yearRasterID$rasterID==names(ll)[i]][1]
                pers <- unique(object@time$Period[object@time$Year==year])
                lapply(pers,FUN = function(j){
                  fl <- createFloralMap(landscape = ll[[i]],
                                        catNames = object@lucat$newCatNames,
                                        method = method,
                                        param = object@simuParams@floralValues[object@simuParams@floralValues$period==j,])
                  raster::extent(fl) <- raster::extent(ll[[i]])
                  sp::proj4string(fl) <- sp::proj4string(ll[[i]])
                  raster::writeRaster(fl,paste0("data/generated_floralmaps/floralmap_",names(ll)[i],"_",j,".tif"),format="GTiff",overwrite=TRUE)
                })
              })

              cat("Creating nesting maps... This may take a few minutes.\n")
              lapply(1:length(ll),FUN = function(i){
                ne <- createNestMap(landscape = ll[[i]],
                                    catNames =object@lucat$newCatNames,
                                    nestValues = object@simuParams@nestingValues)
                raster::extent(ne) <- raster::extent(ll[[i]])
                sp::proj4string(ne) <- sp::proj4string(ll[[i]])
                raster::writeRaster(ne,paste0("data/generated_nestingmaps/nestingmap_",names(ll)[i],".tif"),format="GTiff",overwrite=TRUE)
              })
            }

            object@landscapes <- ll
            # object@floralMaps <- fl
            # object@nestMaps <- ne

            object
          })


#' @title getLandscape
#' @describeIn getLandscape get all the landscape IDs
#' @exportMethod getLandscape
setMethod("getLandscape",
          "simulationObj",
          function(object){
            return(unique(object@yearRasterID[]))
          })


#' @title plotMaps
#' @describeIn plotMaps Plot maps of the landscape, and/or floral and nesting maps
#' @param object the \code{simulationObj} from which to plot
#' @param type the type of maps to be plotted, could be one of \code{landscape} for the landuse rasters, \code{floral} for the floral maps
#' or \code{nest} for the nesting maps
#' @param id the landscape ID. Can be a vector
#' @exportMethod plotMaps
setMethod("plotMaps",
          "simulationObj",
          function(object,id,ggplot=FALSE){

            wrongID <- id[!(id %in% object@yearRasterID$Landscape)]
            correctID <- id[(id %in% object@yearRasterID$Landscape)]

            if (length(wrongID)>0) warning(paste("There is no landscape(s) id: ",paste0(wrongID,collapse = " ")))
            if (length(wrongID)==length(id)) stop("Please provide valid landscape IDs")

            cat <- object@lucat$newCatNames
            col <- object@landscapes[[1]]@legend@colortable[1:length(cat)] # get color scales

            # how to divide the graphical window
            n <- length(correctID)
            nrow <- floor(sqrt(n))

            if (ggplot){
              plist <- list() # list of plots
              for (i in 1:length(correctID)){
                rasterID <- unique(object@yearRasterID$rasterID[object@yearRasterID$Landscape==correctID[i]])
                dd <- raster::as.data.frame(object@landscapes[[as.character(rasterID)]], xy = TRUE)
                names(dd) <- c("x","y","raster_cat")
                plist[[i]] <- ggplot2::ggplot() + ggplot2::geom_raster(data=dd,ggplot2::aes(x = x, y = y, fill = raster_cat),alpha=0.5) +
                  ggplot2::scale_fill_manual(name="Landuse",values=col,breaks=cat) + ggplot2::ggtitle(paste("Landscape",correctID[i])) +
                  ggplot2::geom_point(data=object@beeData[object@beeData$rasterID==i,],ggplot2::aes(x=x,y=y,shape=Source),size=3) +
                  ggplot2::xlim(c(min(dd$x),max(dd$x))) + ggplot2::ylim(c(min(dd$y),max(dd$y))) +
                  ggplot2::scale_color_discrete(name="Study")
              }
              do.call("grid.arrange", c(plist, nrow=nrow))
            }else{
              par(mfrow=c(nrow,nrow+1))
              for (i in 1:length(correctID)){
                rasterID <- unique(object@yearRasterID$rasterID[object@yearRasterID$Landscape==correctID[i]])
                raster::plot(object@landscapes[[as.character(rasterID)]],main=paste("Landscape",correctID[i]))
              }
            }
          })



#' @title run
#' @describeIn run Run the model for a given set of (fixed) parameters or for given priors
#' @param object the \code{simulationObj} with the settings of the simulation
#' @param landscapes the landscape IDs for which to run the simulation. Default is "all", otherwise it
#' should be a character vector with names of landscapes
#' @param params (optional) a vector with parameter values to run the model. Each element of the vector should has
#' a name corresponding to one of the model parameters
#' @return a nested list of maps of size the number of landscapes, where each element of this list is
#' also a list of size the number of years for that landscape and the number of periods with predicted
#' intensities of visitation rates. In the end, we have three levels of lists, and to access predictions
#' from landscape "ID", in year "Y" and period "P", we can access it by modelOutput[["ID"]][["Y"]][["P"]]
#' @exportMethod run
setMethod("run",
          "simulationObj",
          function(object,landscapes,params=NULL){
            # landscape ids
            if ("all" %in% landscapes){
              correctID <- unique(object@yearRasterID$Landscape)
            }else{
              wrongID <- landscapes[!(landscapes %in% object@yearRasterID$Landscape)]
              correctID <- landscapes[(landscapes %in% object@yearRasterID$Landscape)]
            }

            # get parameter vector
            if (is.null(params)){
              if (length(simu@modelParams)==0) stop("You should provide a list of parameter values, or define priors for your simulation")
              paramNames <- names(object@modelParams)
              paramValues <- sapply(paramNames,FUN=function(p){
                prior <- object@modelParams[[p]]
                if (prior@distRNG=="fixed"){
                  val <- prior@hyperParams$cst
                }else{
                  val <- do.call(prior@distRNG, c(list(n = 1), prior@hyperParams))
                }
                val
              })
            }else{
              paramValues <- params
              paramNames <- names(params)
            }

            # How to define the floral values
            method <- ifelse(object@simuParams@randomFloral,"random","fixed")

            modelOutput <- list()
            for (i in 1:length(correctID)){
              years <- object@yearRasterID$Year[object@yearRasterID$Landscape==correctID[i]]

              modelOutput[[i]] <- list()
              for (j in 1:length(years)){
                # get raster ID for landscape i and year j
                rasterID <- object@yearRasterID$rasterID[object@yearRasterID$Landscape==correctID[i] &
                                                           object@yearRasterID$Year==years[j]]

                if (!dir.exists("data/generated_floralmaps")){
                  stop("You need to create floral maps before")
                }else{
                  nm <- raster::raster(paste0("data/generated_nestingmaps/nestingmap_",as.character(rasterID),".tif"))

                  pers <- unique(object@time$Period[object@time$Year==years[j]])

                  modelOutput[[i]][[j]] <- lapply(pers, FUN=function(k){
                    fm <- raster::raster(paste0("data/generated_floralmaps/floralmap_",as.character(rasterID),"_",j,".tif"))

                    pred <- cpfModel(paramValues["tau0"],
                                     paramValues["f0"],
                                     paramValues["a"],
                                     paramValues["b"],
                                     floralMat = raster::as.matrix(fm),
                                     nestMat = raster::as.matrix(nm),
                                     cellSize = 10,
                                     maxFlyingDist = paramValues["maxFlyingDist"])
                    pred <- raster::raster(pred)
                    raster::extent(pred) <- raster::extent(object@landscapes[[as.character(rasterID)]])
                    sp::proj4string(pred) <- sp::proj4string(object@landscapes[[as.character(rasterID)]])
                    pred
                  })
                }
              }
              names(modelOutput[[i]]) <- years
            }
            names(modelOutput) <- correctID

            return(list(modelParams=paramValues,modelOutput=modelOutput))
          })


#' @title extractPredictions
#' @describeIn extractPredictions extract model predictions in a 9-cell neighborooh around each data point
#' @param the \code{simulationObj} with the settings of the simulation
#' @param Landscape the landscape ID for which to run the simulation. Default is "all", otherwise it
#' should be a character vector with names of landscapes
#' @exportMethod extractPredictions
setMethod("extractPredictions",
          "simulationObj",
          function(object,modelRun){
            modelOutput <- modelRun$modelOutput
            Landscape <- names(modelOutput)

            # Extract data corresponding to the landscapes on which predictions were computed
            points <- object@beeData[object@beeData$Landscape%in%Landscape,]
            points$dataID <- rownames(points)

            # creating a square buffer of 9 cells around each data points
            cellSize <- raster::res(modelOutput[[1]][[1]][[1]])[1]
            listSquareCoords <- lapply(1:nrow(points), FUN=function(i){
              xCoords <- seq(points$x[i] - cellSize, points$x[i] + cellSize, length.out = 3)
              yCoords <- seq(points$y[i] - cellSize, points$y[i] + cellSize, length.out = 3)
              d <- expand.grid(xCoords,yCoords)
              names(d) <- c("x","y")
              d$dataID <- points$dataID[i]
              d$year <- points$Year[i]
              d$Landscape <- points$Landscape[i]
              return(d)
            })
            squareCoords <- do.call(rbind,listSquareCoords)

            # Transform coordinates into spatial points, all the landscapes having the same projection
            # system (it was checked when importing rasters) and thus assigning the CRS of the first element to all the points
            points_spdf <- sp::SpatialPointsDataFrame(squareCoords[,c("x","y")],proj4string = modelOutput[[1]][[1]][[1]]@crs, squareCoords)

            outPerLandscape <- lapply(1:length(Landscape), FUN = function(i){
              years <- object@yearRasterID$Year[object@yearRasterID$Landscape==Landscape[i]]
              nbyears <- length(years)

              outPerYear <- lapply(1:nbyears, FUN = function(j){
                periods <- unique(object@time$Period[object@time$Year==years[j]])
                nbpers <- length(periods) # number of periods

                outPerPeriods <- lapply(1:nbpers, FUN = function(p){
                  spdfRaster <- raster::rasterToPoints(modelOutput[[i]][[j]][[p]],spatial=TRUE)
                  ids <- unique(squareCoords$dataID[squareCoords$year==years[j] & squareCoords$Landscape==Landscape[i]])
                  out <- t(sapply(ids, FUN = function(k){
                    as.numeric(c(modelOutput[[i]][[j]][[p]][points_spdf[points_spdf@data$dataID==k,]],k))
                  }))
                  out <- as.data.frame(out)
                  names(out) <- c("lowerLeft","lowerMiddle","lowerRight",
                                  "middleLeft","centerPoint","middleRight",
                                  "upperLeft","upperMiddle","upperRight","dataPointID")
                  out$Period <- p
                  return(out)
                })
                outPerPeriods <- do.call(rbind,outPerPeriods)
                outPerPeriods$Year <- years[j]
                return(outPerPeriods)
              })
              outPerYear <- do.call(rbind,outPerYear)
              outPerYear$Landscape <- Landscape[i]
              return(outPerYear)
            })
            outPerLandscape <- do.call(rbind,outPerLandscape)

            paramInMatrixForm <- matrix(rep(modelRun$modelParams,each=nrow(outPerLandscape)),nr=nrow(outPerLandscape), byrow = F)
            params <- as.data.frame(paramInMatrixForm)
            names(params) <- names(modelRun$modelParams)
            outPerLandscape <- cbind(outPerLandscape,params)

            # merge with data
            dataToMerge <- points[,c("Year","SizeAreaInSqMeters","Landscape","Period","NumberOfBees","Landuse","Species")]
            dataToMerge$dataPointID <- as.numeric(rownames(dataToMerge))
            dataToMerge$Period <- as.integer(dataToMerge$Period)
            dataToMerge$Landscape <- as.character(dataToMerge$Landscape)
            rownames(dataToMerge) <- NULL

            df <- dplyr::inner_join(dataToMerge,outPerLandscape)

            return(df)
          })


#' @title runAndExtract
#' @describeIn runAndExtract Run the model for a given set of (fixed) parameters or for given priors, and extract
#' the model predictions in a 9-cell neighborhood around each data point
#' @param object the \code{simulationObj} with the settings of the simulation
#' @param landscapes the landscape ID for which to run the simulation. Default is "all", otherwise it
#' should be a character vector with names of landscapes
#' @param size the number of model runs
#' @param params (optional) a data frame with parameter values to run the model.
#' @return a data frame containing the 9 predictions around each data point, the year, the period, the landscape ID,
#' the parameter values that were used to generate the predictions, and the run ID
#' @exportMethod run
setMethod("runAndExtract",
          "simulationObj",
          function(object,landscapes,size=1,params=NULL){
            # landscape ids
            if ("all" %in% landscapes){
              correctID <- unique(object@yearRasterID$Landscape)
            }else{
              wrongID <- landscapes[!(landscapes %in% object@yearRasterID$Landscape)]
              correctID <- landscapes[(landscapes %in% object@yearRasterID$Landscape)]
            }

            if (is.null(params)){
              if (length(simu@modelParams)==0) stop("You should provide a list of parameter values, or define priors for your simulation")
              paramNames <- names(object@modelParams)
              paramValues <- sapply(paramNames,FUN=function(p){
                prior <- object@modelParams[[p]]
                if (prior@distRNG=="fixed"){
                  val <- rep(prior@hyperParams$cst,size)
                }else{
                  val <- do.call(prior@distRNG, c(list(n = size), prior@hyperParams))
                }
                val
              })
            }else{
              paramValues <- params
              paramNames <- names(params)
            }

            if (size==1){
              paramValues <- matrix(paramValues,nr=1,nc=length(paramNames))
              dimnames(paramValues) <- list("1",paramNames)
            }

            # Extract data point coordinates
            # Extract data corresponding to the landscapes on which predictions were computed
            points <- na.omit(object@beeData[object@beeData$Landscape%in%correctID,])
            points$dataID <- rownames(points)

            # creating a square buffer of 9 cells around each data points
            cellSize <- raster::res(object@landscapes[[1]])[1]
            listSquareCoords <- lapply(1:nrow(points), FUN=function(i){
              xCoords <- seq(points$x[i] - cellSize, points$x[i] + cellSize, length.out = 3)
              yCoords <- seq(points$y[i] - cellSize, points$y[i] + cellSize, length.out = 3)
              d <- expand.grid(xCoords,yCoords)
              names(d) <- c("x","y")
              d$dataID <- points$dataID[i]
              d$year <- points$Year[i]
              d$period <- points$Period[i]
              d$Landscape <- points$Landscape[i]
              return(d)
            })
            squareCoords <- do.call(rbind,listSquareCoords)

            # Transform coordinates into spatial points, all the landscapes having the same projection
            # system (it was checked when importing rasters) and thus assigning the CRS of the first element to all the points
            points_spdf <- sp::SpatialPointsDataFrame(squareCoords[,c("x","y")],proj4string = object@landscapes[[1]]@crs, squareCoords)

            # Generating model runs
            predOverLandscapes <- lapply(1:length(correctID), FUN = function(i){
              years <- object@yearRasterID$Year[object@yearRasterID$Landscape==correctID[i]]

              predoverYears <- lapply(1:length(years), FUN = function(j){
                # get raster ID for landscape i and year j
                rasterID <- object@yearRasterID$rasterID[object@yearRasterID$Landscape==correctID[i] &
                                                           object@yearRasterID$Year==years[j]]

                if (!dir.exists("data/generated_floralmaps")) stop("You need to create floral maps before")

                  nm <- raster::raster(paste0("data/generated_nestingmaps/nestingmap_",as.character(rasterID),".tif"))

                  pers <- unique(object@time$Period[object@time$Year==years[j]])

                  predOverPeriods <- lapply(pers, FUN=function(p){
                    fm <- raster::raster(paste0("data/generated_floralmaps/floralmap_",as.character(rasterID),"_",p,".tif"))

                    extractedValues <- lapply(1:size,FUN = function(k) {
                      pred <- cpfModel(paramValues[k,"tau0"],
                                       paramValues[k,"f0"],
                                       paramValues[k,"a"],
                                       paramValues[k,"b"],
                                       floralMat = raster::as.matrix(fm),
                                       nestMat = raster::as.matrix(nm),
                                       cellSize = cellSize,
                                       maxFlyingDist = paramValues[k,"maxFlyingDist"])
                      pred <- raster::raster(pred)
                      raster::extent(pred) <- raster::extent(object@landscapes[[as.character(rasterID)]])
                      sp::proj4string(pred) <- sp::proj4string(object@landscapes[[as.character(rasterID)]])

                      spdfRaster <- raster::rasterToPoints(pred,spatial=TRUE)
                      ids <- unique(squareCoords$dataID[squareCoords$year==years[j] & squareCoords$Landscape==correctID[i] & squareCoords$period==p])
                      out <- t(sapply(ids, FUN = function(m){
                        as.numeric(c(pred[points_spdf[points_spdf@data$dataID==m,]],m))
                      }))
                      out <- as.data.frame(out)
                      names(out) <- c("lowerLeft","lowerMiddle","lowerRight",
                                      "middleLeft","centerPoint","middleRight",
                                      "upperLeft","upperMiddle","upperRight","dataPointID")
                      out$Period <- p
                      out$Year <- years[j]
                      out$Landscape <- correctID[i]
                      out$iteration <- k
                      dfParams <- as.data.frame(matrix(rep(paramValues[k,],each=nrow(out)),nr=nrow(out)))
                      names(dfParams) <- colnames(paramValues)
                      out <- cbind(out,dfParams)
                      return(out)
                    })
                    df <- do.call(rbind,extractedValues)
                    return(df)
                  })
                  dfPerPeriods <- do.call(rbind,predOverPeriods)
                  return(dfPerPeriods)
              })
              dfPerYears <- do.call(rbind,predoverYears)
              return(dfPerYears)
            })
            dfPerLandscape <- do.call(rbind,predOverLandscapes)

            return(dfPerLandscape)
          })
