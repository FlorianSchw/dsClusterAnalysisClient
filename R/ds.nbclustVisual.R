#'
#' @title Determines optimal number of clusters for dataset
#' @description This function is similar to the R function 'fviz_nbclust' from the factoextra package
#' @details The function uses partitioning methods to find optimal numbers of clusters for a given dataset and visualizes the results.
#' @param df.name is a string character of the data set and can be either a matrix or data frame
#' @param FUNcluster is a partitioning function 
#' @param method the method to be used for estimating the optimal number of clusters. Possible values are "silhouette", "wss" and "gap_stat".
#' @param diss 'dist' object as produced by ds.dist. If diss = NULL, dist(df.name) is computed with the default "euclidean" setting.
#' @param k.max maximum number of clusters to be considered (has to be at least 2)
#' @param nboot number of Monte Carlo bootstrap samples. This argument is only used for determining the number of clusters for gap statistics
#' @param verbose logical values, If TRUE, the result of progress is printed. 
#' @param barfill fill color for bars
#' @param barcolor outline color for bars 
#' @param linecolor color for lines 
#' @param print.summary logical value. If TRUE, the optimal number of clusters are printed 
#' @return a ggplot2 image suggesting optimal number of clusters
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @importFrom factoextra fviz_nbclust
#' @export
#' 



ds.nbclustVisual <- function(df.name=NULL, FUNcluster = c("kmeans", "hcut", "cluster::pam", "cluster::fanny", "cluster::clara"), method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 10, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE, datasources=NULL){

  
  
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
  if(!('matrix' %in% typ) && !('data.frame' %in% typ)){
    stop("Only objects of type 'matrix' or 'data.frame' are allowed.", call.=FALSE)
  }
  
  
  # call the server side function that does the operation
  cally <- call("nbclustVisualDS", df.name, FUNcluster, method, diss, k.max, nboot, verbose, barfill, barcolor, linecolor, print.summary)
  outcome <- DSI::datashield.aggregate(datasources, cally)
  
  return(outcome) 
  
}

# AGGREGATE function
# ds.nbclustVisual

