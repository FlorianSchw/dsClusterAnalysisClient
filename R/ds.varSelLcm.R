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
#' @importFrom magrittr %>%
#' @export
#' 


ds.varSelLcm <- function(df = NULL, num.clust = NULL, vbleSelec = TRUE, crit.varsel = "BIC", initModel = 50, nbcores = 1, nbSmall = 250, iterSmall = 20, nbKeep = 50, iterKeep = 1000, tolKeep = 1e-7, num.iterations = 5, newobj = NULL, datasources = NULL){
  
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
  
  studies_in_analysis <- length(datasources)
  
  if(is.null(newobj)){
    
    newobj <- "ClusterFinal"
    
  }
  
  if(studies_in_analysis == 1){
    
    call_single <- call("varSelLcmSingleDS1", df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep)
    DSI::datashield.assign(datasources, newobj, call_single)
    
    message <- paste0("The clusters have been successfully assigned on the server-side.")
    return(message)
    

  } else if(studies_in_analysis > 1){
    
  

  
  # call the server side function that does the operation
  cally <- call("varSelLcmDS1", df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep)
  initialRun <- DSI::datashield.aggregate(datasources, cally)
  
  
  message("1st part completed.")
  
  
  
  server_col_names <- list()
  
  for (i in 1:length(datasources)){
    
    curr_server_name <- eval(parse(text=paste0("initialRun$", datasources[[i]]@name)))
    server_col_names[[i]] <- names(curr_server_name)
    
  }
  
  server_col_names_comb <- unique(unlist(server_col_names))
  
  initialRun_mod <- data.frame(matrix(nrow = num.clust, ncol = length(server_col_names_comb)))
  colnames(initialRun_mod) <- server_col_names_comb
  
  initialRun_mod_list <- list()
  
  for (i in 1:length(datasources)){
    
    initialRun_mod_list[[i]] <- initialRun_mod
  }
  
  names(initialRun_mod_list) <- names(initialRun)
  
  for (k in 1:length(initialRun_mod_list)){
    for (i in 1:length(server_col_names_comb)){
      if(server_col_names_comb[i] %in% server_col_names[[k]]){
        
        initialRun_mod_list[[k]][,server_col_names_comb[i]] <- initialRun[[k]][,server_col_names_comb[i]]
        
      }
    }
  }
  
  
  
  ######################################
  ######################################
  ######################################
  ######################################
  
  variables_number <- ds.dim(x = df, type = "combine")[[1]][2]
  
  initialRun_char_vect <- paste0(as.character(unlist(initialRun_mod_list)), collapse = ",")
  colnames_char_vect <- paste0(as.character(colnames(initialRun_mod_list[[1]])), collapse = ",")
  entries_per_study <- variables_number * num.clust
  
  ######################################
  ######################################
  ######################################
  ######################################
  ######################################
  
  newobj_pre <- "cluster_pre"
  callz <- call("varSelLcmDS2", df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep, num.iterations, initialRun_char_vect, colnames_char_vect, entries_per_study)
  DSI::datashield.assign(datasources, newobj_pre, callz)
  
  
  callww <- call("varSelLcmDS3", df)
  results_obj <- DSI::datashield.aggregate(datasources, callww)
  
  message("2nd part completed.")
  
  
  
  ######################################
  ######################################
  ######################################
  ### can still be improved 
  
  summary_data <- list()
  model_data <- list()
  discrim_data <- list()
  irrelevant_names <- list()
  
  performance <- as.data.frame(matrix(nrow = studies_in_analysis, ncol = 6))
  colnames(performance) <- c("ServerName", "Loglikelihood", "AIC", "BIC", "ICL", "Davies_Bouldin")
  
  for (i in 1:length(results_obj)){
    
    results_obj[[i]][[1]]$ServerName <- names(results_obj)[i]
    summary_data[i] <- results_obj[[i]][1]
    irrelevant_names[[i]] <- results_obj[[i]][2]
    discrim_data[[i]] <- results_obj[[i]][3]
    performance[i, "ServerName"] <- names(results_obj)[i]
    performance[i, "Loglikelihood"] <- results_obj[[i]][[4]]
    performance[i, "AIC"] <- results_obj[[i]][[5]]
    performance[i, "BIC"] <- results_obj[[i]][[6]]
    performance[i, "ICL"] <- results_obj[[i]][[7]]
    performance[i, "Davies_Bouldin"] <- results_obj[[i]][[8]]
    
  }
  
  comparison_obj <- bind_rows(summary_data)
  
  order_object <- comparison_obj %>%
    relocate(c(ServerName,results_values))
  
  
  exclude_list <- c("ServerName", "results_values")
  
  
  variable_to_be_excluded <- unique(unlist(irrelevant_names))
  position_col <- c()
  
  for (e in 1:length(colnames(order_object))){
    
    
    logi_int <- strsplit(colnames(order_object), "_X_")[[e]][2] %in% variable_to_be_excluded
    position_col[e] <- logi_int
    
  }
  
  final_exclude_list <- c(exclude_list,
                          colnames(order_object)[position_col])
  
  
  variable_importance <- bind_rows(discrim_data)
  #variable_irrelevance <- bind_rows(irrelevant_names)
  
  
  matching_vector <- rep(NA, dim(order_object)[1])
  
  for (n in 2:studies_in_analysis){
    
    first_server <- seq(from = 1, to = num.clust, by = 1)
    additional_server <- seq(from = (n-1)*num.clust + 1, to = (n)*num.clust, by = 1)
    
    #### needs adjustment which columns to take into account
    
    status <- TRUE
    count <- 0
    while(status){
      
      count <- count + 1
      
      matching_cluster <- VarSelLCM::VarSelCluster(x = order_object[c(first_server, additional_server), !(colnames(order_object) %in% final_exclude_list)],
                                                   gvals = num.clust)
      
      matching_indiv <- matching_cluster@partitions@zMAP
      
      matching_vector[first_server] <- 1:num.clust
      
      serv1 <- matching_indiv[seq(from = 1, to = num.clust, by = 1)]
      serv2 <- matching_indiv[seq(from = num.clust + 1, to = 2*num.clust, by = 1)]
      
      status1 <- all(seq(1:num.clust) %in% serv1)
      status2 <- all(seq(1:num.clust) %in% serv2)
      
      if(all(status1, status2)){
        status <- FALSE
      }
      
      if(count > 250){
        status <- FALSE
      }
      
    }  
    
    
    pos <- c()
    for (ll in 1:length(serv2)){
      
      pos[ll] <- which(serv1 == serv2[ll])
      
    }
    matching_vector[additional_server] <- pos
  }
  
  
  order_object$Matching <- matching_vector
  matching_object <- subset(order_object, select = c("ServerName", "results_values", "Matching"))
  

  
  for (zz in 1:length(datasources)){
    
    final_match_df <- matching_object %>%
      filter(ServerName == datasources[[zz]]@name) %>%
      select(results_values, Matching)
    
    final_match_char <- paste0(as.character(unlist(as.vector(final_match_df))), collapse = ",")
    final_colnames_char <- paste0(as.character(colnames(final_match_df)), collapse = ",")
    
    callx <- call("varSelLcmDS4", num.clust, final_match_char, final_colnames_char)
    
    #current_datasource <- datasources[[zz]]@name
    
    DSI::datashield.assign(datasources[[zz]], newobj, callx)
    
    
  }
  
  
  
  model_information <- list(order_object,
                            variable_importance,
                            irrelevant_names,
                            performance,
                            matching_cluster)
  
  
  outcome <- model_information
  
  
  
  
  
  return(outcome)
  
  }
  
}
