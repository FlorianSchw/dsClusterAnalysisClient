#'
#' @title Computes the distance matrix of a given data set
#' @description This function is similar to the native R function from stats
#' @details The function calls the server-side function \code{distDS} that computes the
#' distance matrix of a data set (type data.frame or matrix) and assigns the new object to the server-side, which will be of class 'dist'.
#' The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{dist.newobj} by default.
#' @param df.name is a string character of the data set (either a data.frame or a matrix)
#' @param method specifies the method for the distance matrix calculation and can be either 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'
#' @param newobj is the name of the new object which is created with this function
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#' @return the object specified by the \code{newobj} argument of \code{ds.dist} or default name \code{dist.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import DSI
#' @import dsBaseClient
#' @import methods
#' @export
#' 


ds.dist <- function(df.name=NULL, method = "euclidean", newobj=NULL, datasources=NULL){
  
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
  
  
  # Needs check if df.name exists everywhere and that the columns in df.name have the same name in all studies
  defined <- isDefined(datasources, df.name)
  
  
  # if the input object is not defined in all studies then return an error message
  if(defined == FALSE){
    stop("The dataframe is not defined in all the studies!", call.=FALSE)
  }
  
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, df.name)
  
  # Check whether the input is either of type data frame or matrix
  if(!('data.frame' %in% typ) && !('matrix' %in% typ)){
    stop("Only objects of type 'data frame' or 'matrix' are allowed.", call.=FALSE)
  }
  
  
  # Check whether all columns in the data frame exist in every source
  
  column.names <- list()
  for (i in 1:length(datasources)){
    column.names[[i]] <- dsBaseClient::ds.colnames(df.name, datasources=datasources[i])[[1]]
  }
  
  allNames <- unique(unlist(column.names))
  
  # if the data sets do not share the same columns then the function stops
  check.indicator <- c()
  for (i in 1:length(datasources)){
    if(length(setdiff(allNames,column.names[[i]])) > 0){
      check.indicator[i] <- 1
    }else{
      check.indicator[i] <- 0}
  }
  
  if(!(sum(check.indicator)==0)){
    stop("The data frames do not have the same columns. There are columns missing in some data frames!", call.=FALSE)
  }
  
  
  class.list <- lapply(allNames, function(x){dsBaseClient::ds.class(paste0(df.name, '$', x), datasources=datasources)})
  class.vect1 <- lapply(class.list, function(x){unlist(x)})
  class.vect2 <- lapply(class.vect1, function(x){x[which(x != 'NULL')[[1]]]})
  class.vect2 <- unname(unlist(class.vect2))
  
  
  # Check whether the columns in the data set are either of type numeric or integer
  if(!('numeric' %in% class.vect2) && !('integer' %in% class.vect2)){
    stop("The data frames contain columns which are not of type 'numeric' or 'integer'.", call.=FALSE)
  }
  
  
  # create a name by default if the user does not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "dist.newobj"
  }
  
  
  # Needs to be checked for one of the six methods in a list
  allowedmethods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  
  if(!(method %in% allowedmethods)){
    stop("Method needs to be one of the following: 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'.", call.=FALSE)
  }
  
  
  # call the server side function that does the operation
  cally <- call("distDS", df.name, method)
  DSI::datashield.assign(datasources, newobj, cally)
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}


