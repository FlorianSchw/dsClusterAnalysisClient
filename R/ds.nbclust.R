#'
#' @title Determines optimal number of clusters for dataset
#' @description This function is similar to the R function 'NBClust' from the NBClust package
#' @details The function uses partitioning methods to find optimal numbers of clusters for a given dataset.
#' @param df.name is a string character of the data set and can be either a matrix or data frame
#' @param diss is a dissimilarity structure which will be calculated according to the distance method
#' @param distance specifies the method for the distance matrix calculation and can be either 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'
#' @param min.nc specifies the minimum number of clusters
#' @param max.nc specifies the maximum number of clusters
#' @param method describes the clustering method and can be either "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans" or "ward.D"
#' @param index describes the clustering index and can be either "kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" or "alllong"         
#' @param alphaBeale value for "beale" clustering index
#' @return a summary suggesting the optimal number of clusters
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export
#' 



ds.nbclust <- function(df.name = NULL, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = NULL, index = "all", alphaBeale = 0.1, datasources=NULL){
  
  
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
  
  
  if(is.null(method)){
    stop("Please provide a method for the calculation!", call.=FALSE)
  }
  
  
  if(!(is.null(diss)) && !(is.null(distance))){
    stop("Both, diss and distance are not NULL! Only one argument needs to be given.", call.=FALSE)
  }
  
  
  if(is.null(diss) && is.null(distance)){
    stop("Both, diss and distance are NULL! One argument needs to be given.", call.=FALSE)
  }
  
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- dsBaseClient::ds.class(df.name, datasources)
  
  
  # Check whether the input is either of type data frame or matrix
  if(!('matrix' %in% typ) && !('data.frame' %in% typ)){
    stop("Only objects of type 'matrix' or 'data.frame' are allowed.", call.=FALSE)
  }
  
  # call the server side function that does the operation
  cally <- call("nbclustDS", df.name, diss, distance, min.nc, max.nc, method, index, alphaBeale)
  outcome <- DSI::datashield.aggregate(datasources, cally)
  
  return(outcome) 
  
}

# AGGREGATE function
# ds.nbclust

