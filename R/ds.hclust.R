#'
#' @title Conducts hierarchical clustering of a given dissimilarity structure
#' @description This function is similar to the native R function 'hclust' from stats
#' @details The function calls the server-side function \code{hclustDS} that computes the
#' hierarchical clustering of a dissimilarity structure (type 'dist'). The dissimilarity structure can be computed via the ds.dist function.
#' The function creates a new object on the server-side, which is of class 'hclust'.
#' The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{hclust.newobj} by default.
#' @param diss is a string character of the dissimilarity structure
#' @param method specifies the method for the calculation of the hierarchical clustering and can be either 'ward.D', 'ward.D2', 'single', 
#' 'complete', 'average', 'mcquitty', 'median' or 'centroid'
#' @param newobj is the name of the new object which is created with this function
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#' @return the object specified by the \code{newobj} argument of \code{ds.hclust} or default name \code{hclust.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import DSI
#' @import dsBaseClient
#' @import methods
#' @export
#' 


ds.hclust <- function(diss = NULL, method = "ward.D2", newobj = NULL, datasources = NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  if(is.null(diss)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- dsBaseClient::ds.class(diss, datasources)
  
  
  # Check whether the input is of type 'dist'
  if(!('dist' %in% typ)){
    stop("Only objects of type 'dist' are allowed for the computation of the clustering.", call.=FALSE)
  }
  
  
  # create a name by default if the user does not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "hclust.newobj"
  }
  
  
  # Needs to be checked for one of the six methods in a list
  allowedmethods <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
  
  if(!(method %in% allowedmethods)){
    stop("Method needs to be one of the following: 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid'.", call.=FALSE)
  }
  
  
  # call the server side function that does the operation
  cally <- call("hclustDS", diss, method)
  DSI::datashield.assign(datasources, newobj, cally)

  
}













