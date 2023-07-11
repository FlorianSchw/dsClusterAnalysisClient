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
#' @param discrim is a logical or integer specifying whether tracing information on the progress of the algorithm is procuded for the Hartigan-Wong algorithm
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
#' @import dplyr
#' @import datashieldDescriptives
#' @export
#' 


ds.varSelLcm_Alternate2 <- function(df = NULL, num.clust = NULL, vbleSelec = TRUE, crit.varsel = "BIC", initModel = 50, nbcores = 1, nbSmall = 250, iterSmall = 20, nbKeep = 50, iterKeep = 1000, tolKeep = 1e-7, num.iterations = 5, newobj = NULL, datasources = NULL){
  
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
  
  
  # call the server side function that does the operation
  newobj_first <- "cluster_ind"
  cally <- call("varSelLcm_DA_DS1", df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep)
  initialRun <- DSI::datashield.assign(datasources, newobj_first, cally)
  
  studies_in_analysis <- length(datasources)
  
  
  study_data_names <-c()
  for (i in 1:length(datasources)){
    
    ds.dmtC2S(dfdata = initialRun[[i]], newobj = paste0("StudyData", i), datasources = datasources[-i])
    study_data_names[i] <- paste0("StudyData", i)
    
  }
  
  newobj_pre <- "cluster_pre"
  callz <- call("varSelLcmDS2", df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep, num.iterations)
  DSI::datashield.assign(datasources, newobj_pre, callz)
  
  finalcheck <- dsBaseClient:::isAssigned(datasources, newobj_pre)
  
  
  #### conns[1] wird als Argument genutzt in der datashield_descriptives function... that needs a change
  
  df_classes <- datashieldDescriptives::datashield_descriptive(ds.class, opal_connection = conns[1], df = df)
  
  non_factor_variables <- df_classes %>%
    filter(!df_classes == "factor")
  
  factor_variables <- df_classes %>%
    filter(df_classes == "factor")
  
  
  new_vector_names <- c()
  
  total_length <- ds.length(x = paste0(df,"$", row.names(factor_variables)[1]), type = "split") 
  
  
  #### 1000 is still hard coded; needs change
  for (ii in 1:length(datasources)){
    for (k in 1:num.clust){
      
      ds.make(toAssign = rep(k, total_length[ii]), newobj = paste0("Special_Clustering_vector_", k), datasources = datasources[ii])
      new_vector_names[k] <- paste0("Special_Clustering_vector_", k)
      
    }
  }
  
  ds.dataFrame(x = c(paste0(df), 
                     "cluster_pre",
                     paste0(new_vector_names)), newobj = "Special_Clustering_Df")
  
  list_means <- list()
  for (ii in 1:length(non_factor_variables[,1])){
    
    list_means[[ii]] <- ds.meanSdGp(x = paste0("Special_Clustering_Df$", row.names(non_factor_variables)[ii]), y = "Special_Clustering_Df$cluster_pre", type = "split")
    
  }
  
  new_dataframe_names <- c()
  status_creation_dfs <- list()
  for (t in 1:length(new_vector_names)){
    tryCatch({
      
      
      ds.dataFrameSubset(df.name = "Special_Clustering_Df",
                         V1.name = "Special_Clustering_Df$cluster_pre",
                         V2.name = paste0("Special_Clustering_Df$",new_vector_names[t]),
                         Boolean.operator = "==",
                         newobj = paste0("Special_Clustering_Df2_", t))
      
      status_creation_dfs[[t]] <- "OK"
      
      new_dataframe_names[t] <- paste0("Special_Clustering_Df2_", t)
      
    },
    
    #### Placeholder for now; there needs to be code that deals with failing DS function because of nfilter; if (error) then set to 0, 
    #### doesn't matter for the clustering part whether it is 0, 1 or 2
    
    error = function(e){
      message("An Error Occured")
      print(e)
    }
    
    )
  }
  
  total_factor_lengths <- list()
  
  new_factor_list <- list()
  
  for (zz in 1:length(new_dataframe_names)){
    
    factor_levels <- c()
    for (ff in 1:length(factor_variables[,1])){
      
      tryCatch({
        factor_levels[ff] <- ds.levels(x = paste0(new_dataframe_names[zz],"$", row.names(factor_variables)[ff]))
      },
      
      #### Placeholder for now; there needs to be code that deals with failing DS function because of nfilter; if (error) then set to 0, 
      #### doesn't matter for the clustering part whether it is 0, 1 or 2
      
      error = function(e){
        message("An Error Occured")
        print(e)
      }
      
      )
      
      
    }
    
    new_factor_list[[zz]] <- factor_levels
    
  }   
  
  
  levels_appearing <- bind_rows(new_factor_list)
  levels_appearing_unique <- unique(as.numeric(levels_appearing$Levels))
  
  factor_expressions <- c()
  
  
  for (ii in 1:length(datasources)){
    
    for (zz in 1:length(levels_appearing_unique)){
      
      ds.make(toAssign = rep(levels_appearing_unique[zz], total_length[ii]), newobj = paste0("Special_Clustering_Factor_", zz), datasources = datasources[ii])
      factor_expressions[zz] <- paste0("Special_Clustering_Factor_", zz)
      
    }
    
  }    
  
  
  ds.dataFrame(x = c(paste0(df), 
                     "cluster_pre",
                     paste0(factor_expressions),
                     paste0(new_vector_names)), newobj = "Special_Clustering_Factor")
  
  
  pivot_factor_list <- list()
  length_pivot_factor_list <- 0
  
  #### At this point the data.frame is split into multiple df's depending on their individual cluster number in order to assess
  #### how the categorical values are distributed in the different clusters
  #### this might be challenging if the data is extreme between studies which could lead to < n.filter appearances of cluster numbers
  #### here we need a fail safe for the nfilter problem
  
  new_list <- list()
  factorLength_names <- c()
  xxxx_vector <- c()
  length_experimental <- 0
  experimental <- list()
  
  
  for (uu in 1:length(factor_variables[,1])){
    
    factor_lengths <- list()
    
    
    for (pp in 1:length(levels_appearing_unique)){
      
      test1 <- levels_appearing_unique[pp] %in% as.numeric(new_factor_list[[1]][[uu]]$Levels)
      #status2 <- "Fail"
      
      if(test1){
        
        
        for (rr in 1:studies_in_analysis){
          
          for (tt in 1:num.clust){
            
            tryCatch(
              
              {ds.dataFrameSubset(df.name = "Special_Clustering_Factor",
                                  V1.name = paste0("Special_Clustering_Factor$", row.names(factor_variables)[uu]),
                                  V2.name = paste0("Special_Clustering_Factor$", factor_expressions[pp]),
                                  Boolean.operator = "==",
                                  newobj = paste0("Clustering_FactorLength_", pp),
                                  datasources = datasources[rr])
                
              },
              
              #### Placeholder for now; there needs to be code that deals with failing DS function because of nfilter; if (error) then set to 0, 
              #### doesn't matter for the clustering part whether it is 0, 1 or 2
              
              error = function(e){
                message("An Error Occured")
                print(e)
              }
            )
            
            tryCatch(
              {ds.dataFrameSubset(df.name = paste0("Clustering_FactorLength_", pp),
                                  V1.name = paste0("Clustering_FactorLength_", pp, "$cluster_pre"),
                                  V2.name = paste0("Clustering_FactorLength_", pp, "$Special_Clustering_vector_", tt),
                                  Boolean.operator = "==",
                                  newobj = paste0("XXXX_DS_Error_", tt),
                                  datasources = datasources[rr])
                
                xxxx_check <- as.numeric(unlist(ds.length(x = paste0("XXXX_DS_Error_", tt, "$", row.names(factor_variables)[uu]), 
                                                          type = "split", datasources = datasources[rr])))
                
                experimental[[length_experimental + 1]] <- c(xxxx_check,
                                                             uu,
                                                             pp,
                                                             rr,
                                                             tt)
                
                length_experimental <- length(experimental)
                
                
              },
              #### Placeholder for better version
              error = function(e){
                message("An Error Occured")
                print(e)
              }
            )
            
          }
          
        }
        
      }
    }  
  }
  
  
  #### Re-Shaping factor object   
  #pivot_factor_df <- bind_cols(pivot_factor_list)
  
  
  experimental_df <- as.data.frame(t(bind_cols(experimental)))
  
  colnames(experimental_df) <- c("Length",
                                 "uu",
                                 "pp",
                                 "Server",
                                 "Cluster")
  
  experimental_df <- experimental_df %>%
    mutate(Variable = paste0(row.names(factor_variables)[uu], "_", levels_appearing_unique[pp])) %>%
    select(c(Server, Cluster, Variable, Length)) %>%
    pivot_wider(names_from = Variable,
                values_from = Length)
  
  
  server_cluster_pairs <- data.frame(Server = rep(1:studies_in_analysis, num.clust),
                                     Cluster = rep(1:num.clust, each = studies_in_analysis))
  
  rows_to_add <- server_cluster_pairs[which(!(interaction(server_cluster_pairs[1:2]) %in% interaction(experimental_df[1:2]))),]
  
  factor_length_df <- full_join(experimental_df, rows_to_add)
  
  for (uu in 1:length(factor_variables[,1])){
    
    current_vector <- factor_length_df %>%
      select(contains(row.names(factor_variables)[uu])) %>%
      rowSums()
    
    factor_length_df <- factor_length_df %>%
      mutate(across(contains(row.names(factor_variables)[uu]), ~ .x / current_vector * 100))
    
  }
  
  factor_length_df <- factor_length_df %>%
    mutate(Server = paste0("Server", Server))
  
  ##### Re-shaping continuous variable means
  pivot_cont_object <- data.frame(matrix(NA,nrow = num.clust * studies_in_analysis, ncol = 1 ))
  
  for (o in 1:length(list_means)){
    
    pivot_variable <- as.data.frame(list_means[[o]][[1]]) %>%
      pivot_longer(cols = everything(),
                   names_to = "Server",
                   values_to = paste0(row.names(non_factor_variables)[o]))
    
    pivot_cont_object[[paste0(row.names(non_factor_variables)[o])]] <- unlist(pivot_variable[2])
    server_object <- unlist(pivot_variable[1])
    
  }
  
  pivot_cont_object <- pivot_cont_object[-1]
  pivot_cont_object$Server <- server_object
  pivot_cont_object$Cluster <- rep(1:num.clust, each = studies_in_analysis)
  
  
  #### Merging the cont. and cat. variable tables by cluster and server
  order_object_both_variables <- left_join(x = pivot_cont_object, y = factor_length_df, by = c("Server", "Cluster"))
  
  order_object_both_variables <- order_object_both_variables %>%
    relocate(c(Server,Cluster))
  
  
  matching_vector <- rep(NA, dim(order_object_both_variables)[1])
  exclude_list <- c("Server", "Cluster",
                    names(which(colSums(is.na(order_object_both_variables)) > 0)))
  
  for (n in 2:studies_in_analysis){
    
    first_server <- seq(from = 1, to = dim(order_object_both_variables)[1], by = studies_in_analysis)
    additional_server <- seq(from = n, to = dim(order_object_both_variables)[1], by = studies_in_analysis)
    
    #### needs adjustment which columns to take into account
    
    
    matching_indiv <- VarSelLCM::VarSelCluster(x = order_object_both_variables[c(first_server, additional_server), 
                                                                               !(colnames(order_object_both_variables) %in% exclude_list)],
                                               gvals = num.clust)@partitions@zMAP
    
    matching_vector[first_server] <- 1:num.clust
    
    serv1 <- matching_indiv[seq(from = 1, to = length(matching_indiv)/2, by = 1)]
    serv2 <- matching_indiv[seq(from = length(matching_indiv)/2 + 1, to = length(matching_indiv), by = 1)]
    
    pos <- c()
    for (ll in 1:length(serv2)){
      
      pos[ll] <- which(serv1 == serv2[ll])
      
    }
    matching_vector[additional_server] <- pos
  }
  
  
  order_object_both_variables$Matching <- matching_vector
  matching_object <- subset(order_object_both_variables, select = c("Server", "Cluster", "Matching"))
  
  final_match <- list()
  
  for (zz in 1:length(datasources)){
    
    final_match[[zz]] <- matching_object %>%
      filter(Server == datasources[[zz]]@name)
    
    ds.dmtC2S(dfdata = final_match[[zz]], newobj = "ClusterMatching", datasources = datasources[zz])
    
  }
  
  obj_created_check <- dsBaseClient:::isAssigned(datasources, "ClusterMatching")
  
  
  if(is.null(newobj)){
    
    newobj <- "ClusterFinal"
    
  }
  
  callx <- call("varSelLcmDS3", num.clust)
  DSI::datashield.assign(datasources, newobj, callx)
  
  obj_created_check <- dsBaseClient:::isAssigned(datasources, newobj)
  
  
  #### Clean-up of temporary objects created on the server-side should only be done at end of function writing
  #### in order to still see objects with dsLite
  
  #ds.rm(x.names = c("ClusterMatching",
  #                 xxxx_vector,
  #                "Special_Clustering_Factor",
  #               factor_expressions,
  #              new_vector_names,
  #             new_dataframe_names,
  #            "cluster_pre",
  #           "Special_Clustering_Df",
  #          study_data_names))
  
  
  
  
  
  outcome <- order_object_both_variables
  
  
  
  return(outcome)
  
}

