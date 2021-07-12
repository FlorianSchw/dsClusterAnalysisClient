############### Plotting function for kmeans and hclust




#'
#' @title Draws a basic plot for clustering methods
#' @description This function produces a plot for kmeans or hclust clustering data
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
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#' @return the object specified by the \code{newobj} argument of \code{ds.kmeans} or default name \code{kmeans.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import DSI
#' @import dsBaseClient
#' @import methods
#' @export
#' 






ds.clusterPlot <- function(df.name=NULL, k = NULL, h = NULL, k_colors = NULL, color_labels_by_k = FALSE, main = "Cluster Dendrogram", xlab = "Samples", ylab = "Height", newobj=NULL, datasources=NULL){
  
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
  
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- dsBaseClient::ds.class(df.name, datasources)
  
  # Check whether the input is either of type data frame or matrix
  if(!('hclust' %in% typ)){
    stop("Only objects of type 'hclust' are allowed.", call.=FALSE)
  }
  
  
  # create a name by default if the user does not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "dist.newobj"
  }
  
  
  # call the server side function that does the operation
  cally <- call("clusterPlotDS", df.name, k, h, k_colors, color_labels_by_k, main, xlab, ylab)
  DSI::datashield.aggregate(datasources, newobj, cally)
  
  
}

























