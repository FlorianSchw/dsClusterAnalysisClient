
connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER", "PM_BMI_CATEGORICAL", "DIS_DIAB", "LAB_TRIG"))

test_that("ds.nbclustVisual errors", {
  
  # Creating differing test data.frames in the respective connections for testing if columns check works correctly
  ds.dataFrame(x = c("D$LAB_TSC", "D$GENDER", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df1", datasources = ds.test_env$connections)
  ds.completeCases("test_df1", newobj = "test_df2", datasources = ds.test_env$connections)
  
  ds.asCharacter(x.name = "D$LAB_TSC", newobj = "char.obj", datasources = ds.test_env$connections[1])
  ds.asNumeric(x.name = "D$LAB_TSC", newobj = "char.obj", datasources = ds.test_env$connections[2])
  ds.asNumeric(x.name = "D$LAB_TSC", newobj = "char.obj", datasources = ds.test_env$connections[3])
  ds.dataFrame(x = c("char.obj", "D$GENDER", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df3", datasources = ds.test_env$connections)
  
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG", "D$DIS_DIAB"), newobj = "test_df4", datasources = ds.test_env$connections[1])
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG"), newobj = "test_df4", datasources = ds.test_env$connections[2])
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG"), newobj = "test_df4", datasources = ds.test_env$connections[3])
  
  
  res1 <- ds.nbclustVisual(df.name = "test_df2", FUNcluster = "kmeans", method = "silhouette")
  
    # Actual Test Start
  expect_error(ds.nbclustVisual(), "Please provide the name of the input object!", fixed = TRUE)
  expect_error(ds.nbclustVisual(df.name = "test_df1"), "Please provide the clustering function in the 'FUNcluster' argument.", fixed = TRUE)
  expect_error(ds.nbclustVisual(df.name = "test_df1", FUNcluster = "kmeans"), "Please provide the method for estimating the optimal number of clusters in the 'method' argument.", fixed = TRUE)
  expect_error(ds.nbclustVisual(df.name = "test_df1", FUNcluster = "kmean"), "The clustering function needs to be one of the following: 'kmeans', 'hcut', 'cluster::pam', 'cluster::clara' or 'cluster::fanny'.", fixed = TRUE)
  expect_error(ds.nbclustVisual(df.name = "test_df1", FUNcluster = "kmeans", method = "silhouett"), "The method for estimating the optimal number of clusters needs to be one of the following: 'silhouette', 'wss' or 'gap_stat'.", fixed = TRUE)
  expect_error(ds.nbclustVisual(df.name = "D$LAB_TSC", FUNcluster = "kmeans", method = "silhouette"), "Only objects of type 'matrix' or 'data.frame' are allowed.", fixed = TRUE)
  expect_error(ds.nbclustVisual(df.name = "test_df1", FUNcluster = "kmeans", method = "silhouette", diss = "test_df2"), "The 'diss' argument is not of type 'dist'.", fixed = TRUE)
  expect_error(ds.nbclustVisual(df.name = "test_df4", FUNcluster = "kmeans", method = "silhouette"), "The data frames do not have the same columns. There are columns missing in some data frames!", fixed = TRUE)
  expect_error(ds.nbclustVisual(df.name = "test_df3", FUNcluster = "kmeans", method = "silhouette"), "The data frames contain columns which are not of type 'numeric' or 'integer'.", fixed = TRUE)
  
  expect_error(expect_warning(ds.nbclustVisual(df.name = "test_df1", FUNcluster = "kmeans", method = "silhouette"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE))
  err1 <- datashield.errors()
  #expect_equal(err1$sim1, "Command 'nbclustVisualDS(\"test_df1\", \"kmeans\", \"silhouette\", NULL, 10, 100, FALSE, \"steelblue\", \"steelblue\", \"steelblue\", TRUE)' failed on 'sim1': Error while evaluating 'dsClusterAnalysis::nbclustVisualDS(\"test_df1\", \"kmeans\", \"silhouette\", NULL, 10, 100, FALSE, \"steelblue\", \"steelblue\", \"steelblue\", TRUE)' -> Error : The data frames contains NAs.", fixed = TRUE)
  #expect_error(ds.nbclustVisual(df.name = "test_df1", FUNcluster = "kmeans", method = "silhouette", k.max = 10000), "For this clustering method 'k.max' must be between 1 and nrow(df.name).", fixed = TRUE)
  
  expect_silent(res1)  

})


disconnect.studies.dataset.cnsim()
