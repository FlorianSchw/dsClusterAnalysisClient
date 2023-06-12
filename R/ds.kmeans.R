#'
#' @title Conducts k-means clustering of a given data set
#' @description This function is similar to the native R function 'kmeans' from stats
#' @details The function calls the server-side function \code{kmeansDS} that computes the
#' k-means clustering of a data set (type 'data.frame' or 'matrix'). 
#' The function creates a new object on the server-side, which is of class 'kmeans'.
#' The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{kmeans.newobj} by default.
#' @param df.name is a string character of the data set
#' @param clusters specifies the number of clusters for the computation 
#' @param iter.max specifies the max. number of iterations allowed
#' @param nstart relates to the number of random sets if clusters is a number and not a set of initial cluster centers
#' @param algorithm refers to the algorithm of calculating the kmeans and can be either 'Hartigan-Wong', 'Lloyd', 'Forgy' or 'MacQueen' 
#' @param newobj is the name of the new object which is created with this function
#' @param trace is a logical or integer specifying whether tracing information on the progress of the algorithm is procuded for the Hartigan-Wong algorithm
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#' @param seed is an integer for setting the seed
#' @return the object specified by the \code{newobj} argument of \code{ds.kmeans} or default name \code{kmeans.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import DSI
#' @import dsBaseClient
#' @import methods
#' @export
#' 


ds.kmeans <- function(df.name = NULL, clusters = NULL, iter.max = 10L, nstart = 1L, algorithm = "Hartigan-Wong", trace = FALSE, seed = 123, newobj = NULL, datasources = NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  if(is.null(df.name)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  
  defined <- dsBaseClient:::isDefined(datasources, df.name)
  
  
  if(is.null(clusters)){
    stop("Please provide the number of clusters to be computed!", call.=FALSE)
  }
  

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- dsBaseClient:::checkClass(datasources, df.name)
  
  # Check whether the input is of type 'data.frame' or 'matrix'
  if(!('data.frame' %in% typ) && !('matrix' %in% typ)){
   stop("Only objects of type 'data.frame' or 'matrix' are allowed for the  k-means clustering.", call.=FALSE)
  }
  

  checkColumnsDF1(df.name = df.name, datasources = datasources)
  
  
  # create a name by default if the user does not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "kmeans.newobj"
  }
  

    # Needs to be checked for one of the six methods in a list
  allowedmethods <- c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")
  
  if(!(algorithm %in% allowedmethods)){
    stop("The algorithm needs to be one of the following: 'Hartigan-Wong', 'Lloyd', 'Forgy' or 'MacQueen'.", call.=FALSE)
  }
  
  
  # call the server side function that does the operation
  cally <- call("kmeansDS", df.name, clusters, iter.max, nstart, algorithm, trace, seed)
  DSI::datashield.assign(datasources, newobj, cally)
  

  
}

# ASSIGN funtion
# ds.kmeans





