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
#' @param datasources is a DSConnection object
#' @return a ggplot2 image suggesting optimal number of clusters
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export
#' 



ds.nbclustVisual <- function(df.name = NULL, FUNcluster = NULL, method = NULL, diss = NULL, k.max = 10, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE, datasources=NULL){

  
  
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
  
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- dsBaseClient:::checkClass(datasources, df.name)
  
  
  # Check whether the input is either of type data frame or matrix
  if(!('matrix' %in% typ) && !('data.frame' %in% typ)){
    stop("Only objects of type 'matrix' or 'data.frame' are allowed.", call.=FALSE)
  }
  
  
  if(is.null(FUNcluster)){
    stop("Please provide the clustering function in the 'FUNcluster' argument.", call.=FALSE)
  }
  
  
  # Allowed clustering functions
  clusterfunctions <- c("kmeans", "hcut", "cluster::pam", "cluster::clara", "cluster::fanny")
  
  if(!(FUNcluster %in% clusterfunctions)){
    stop("The clustering function needs to be one of the following: 'kmeans', 'hcut', 'cluster::pam', 'cluster::clara' or 'cluster::fanny'.", call.=FALSE)
  }
  
  
  if(is.null(method)){
    stop("Please provide the method for estimating the optimal number of clusters in the 'method' argument.", call.=FALSE)
  }
  
  
  # Allowed methods
  allowedmethods <- c("silhouette", "wss", "gap_stat")
  
  if(!(method %in% allowedmethods)){
    stop("The method for estimating the optimal number of clusters needs to be one of the following: 'silhouette', 'wss' or 'gap_stat'.", call.=FALSE)
  }
  
  
  # Check whether the input object diss is of type 'dist'
  if(!(is.null(diss))){
      
     typ2 <- dsBaseClient::ds.class(diss, datasources)
  
     if(!('dist' %in% typ2)){
       stop("The 'diss' argument is not of type 'dist'.", call.=FALSE)
     }
     
  }
  

  
  
  checkColumnsDF1(df.name = df.name, datasources = datasources)
  
  
  
  
  # call the server side function that does the operation
  cally <- call("nbclustVisualDS", df.name, FUNcluster, method, diss, k.max, nboot, verbose, barfill, barcolor, linecolor, print.summary)
  outcome <- DSI::datashield.aggregate(datasources, cally)
  
  return(outcome) 
  
}

# AGGREGATE function
# ds.nbclustVisual

