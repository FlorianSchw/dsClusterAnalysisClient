#'
#' @title Conducts k-means clustering of a given data set
#' @description This function is similar to the native R function 'kmeans' from stats
#' @details The function calls the server-side function \code{kmeansDS} that computes the
#' k-means clustering of a data set (type 'data.frame' or 'matrix'). 
#' The function creates a new object on the server-side, which is of class 'kmeans'.
#' The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{kmeans.newobj} by default.
#' @param df is a string character of the data set
#' @param num.clust specifies the number of clusters for the computation 
#' @param vbleSelec specifies the max. number of iterations allowed
#' @param crit.varsel relates to the number of random sets if clusters is a number and not a set of initial cluster centers
#' @param initModel refers to the algorithm of calculating the kmeans and can be either 'Hartigan-Wong', 'Lloyd', 'Forgy' or 'MacQueen' 
#' @param nbcores is the name of the new object which is created with this function
#' @param discrim is a logical or integer specifying whether tracing information on the progress of the algorithm is procuded for the Hartigan-Wong algorithm
#' @param nbSmall is a logical or integer specifying whether tracing information on the progress of the algorithm is procuded for the Hartigan-Wong algorithm
#' @param iterSmall is a logical or integer specifying whether tracing information on the progress of the algorithm is procuded for the Hartigan-Wong algorithm
#' @param nbKeep is a logical or integer specifying whether tracing information on the progress of the algorithm is procuded for the Hartigan-Wong algorithm
#' @param iterKeep is a logical or integer specifying whether tracing information on the progress of the algorithm is procuded for the Hartigan-Wong algorithm
#' @param tolKeep represents the number at which point two successive models are defined to be converged; default is 1e-7
#' @param num.iterations the number of iterations for finding SLMA clusters in each respective datasource
#' @param newobj is a logical or integer specifying whether tracing information on the progress of the algorithm is procuded for the Hartigan-Wong algorithm
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#' @return the object specified by the \code{newobj} argument of \code{ds.kmeans} or default name \code{kmeans.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import DSI
#' @import dsBaseClient
#' @import methods
#' @import dplyr
#' @import datashieldDescriptives
#' @export
#' 


ds.varSelLcm_Test2 <- function(df = NULL, num.clust = NULL, vbleSelec = TRUE, crit.varsel = "BIC", initModel = 50, nbcores = 1, nbSmall = 250, iterSmall = 20, nbKeep = 50, iterKeep = 1000, tolKeep = 1e-7, num.iterations = 5, newobj = NULL, datasources = NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  if(is.null(df)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  if(is.null(num.clust)){
    stop("Please specify the number of clusters!", call.=FALSE)
  }
  
  if(!(num.iterations > 1)){
    stop("The number of iterations should be more than 1.", call.=FALSE)
  }
  
  defined <- dsBaseClient:::isDefined(datasources, df)
  
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- dsBaseClient:::checkClass(datasources, df)
  
  # Check whether the input is of type 'data.frame' or 'matrix'
  if(!('data.frame' %in% typ)){
    stop("Only objects of type 'data.frame' are allowed for the clustering.", call.=FALSE)
  }
  
  
  # call the server side function that does the operation
  cally <- call("varSelLcmDS1", df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep)
  initialRun <- DSI::datashield.aggregate(datasources, cally)
  
  studies_in_analysis <- length(datasources)
  
  
  study_data_names <-c()
  for (i in 1:length(datasources)){
    
    ds.dmtC2S(dfdata = initialRun[[i]], newobj = paste0("StudyData", i), datasources = datasources[-i])
    study_data_names[i] <- paste0("StudyData", i)
    
  }
  
  newobj_pre <- "cluster_pre"
  callz <- call("varSelLcmDS2", df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep, num.iterations)
  outcome <- DSI::datashield.aggregate(datasources, callz)
  
  
  
  
  #outcome <- list()
  
  
  
  
  
  return(outcome)
  
}

