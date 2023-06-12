#'
#' @title Checks whether columns in a data frame are the same and of typ 'numerical'
#' @description This function checks a data frame and its columns which should be the same and of typ 'numerical'
#' @details The function looks for differences in data frames components.
#' @param df.name is the name of the data frame
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login
#' @return this function conducts checks only
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' 




checkColumnsDF2 <- function(df.name, datasources){
  
  
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
  if(!('numeric' %in% class.vect2)){
    stop("The data frames contain columns which are not of type 'numeric'.", call.=FALSE)
  }
}