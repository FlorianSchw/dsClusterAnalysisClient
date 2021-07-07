#'
#' @title Cuts the tree of a hclust object
#' @description This function is similar to the native R function from stats
#' @details The function calls the server-side function \code{cutreeDS} that computes the
#' clusters for a given number of clusters k or height h, and assigns the new object to the server-side.
#' The new object is named by the user using the \code{newobj} argument, otherwise it is named \code{cutree.newobj} by default.
#' @param tree is a string character specifying the name of the hclust object. k and h specify the number of clusters or height of the tree at which
#' the tree should be cut.
#' @return the object specified by the \code{newobj} argument of \code{ds.cutree} or default name \code{cutree.newobj}
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export
#' 


ds.cutree <- function(tree, k = NULL, h = NULL, newobj = NULL, datasources = NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  if(is.null(tree)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  
  if(is.null(k) && is.null(h)){
    stop("Please provide a number for k or h for cutting the tree.", call.=FALSE)
  }
  
  # Needs check if df.name exists everywhere and that the columns in df.name have the same name in all studies
  defined <- isDefined(datasources, tree)
  
  
  # if the input object is not defined in all studies then return an error message
  if(defined == FALSE){
    stop("The dataframe is not defined in all the studies!", call.=FALSE)
  }
  
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, tree)

  
  # Check whether the input is either of type data frame or matrix
  if(!('hclust' %in% typ)){
    stop("Only objects of type 'hclust' are allowed.", call.=FALSE)
  }
  
 
  # create a name by default if the user does not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "cutree.newobj"
  }
  
  
  
  # call the server side function that does the operation
  cally <- call("cutreeDS", tree, k, h)
  DSI::datashield.assign(datasources, newobj, cally)
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
  
}























