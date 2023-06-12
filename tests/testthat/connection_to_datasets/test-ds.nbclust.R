



connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER", "PM_BMI_CATEGORICAL", "DIS_DIAB", "LAB_TRIG"))

test_that("ds.nbclust errors", {
  
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
  
  ds.asNumeric(x.name = "D$LAB_TSC", newobj = "num.obj1", datasources = ds.test_env$connections)
  ds.asNumeric(x.name = "D$DIS_DIAB", newobj = "num.obj2", datasources = ds.test_env$connections)
  ds.asNumeric(x.name = "D$LAB_TRIG", newobj = "num.obj3", datasources = ds.test_env$connections)
  ds.dataFrame(x = c("num.obj1", "num.obj2", "num.obj3"), newobj = "test_df5", datasources = ds.test_env$connections)
  
  
  ds.dist("test_df1", newobj = "diss.obj")
  
  ds.completeCases("test_df5", newobj = "test_df6", datasources = ds.test_env$connections)
  res1 <- ds.nbclust(df.name = "test_df6", method = "kmeans")
  
  # Actual Test Start
  
  expect_error(ds.nbclust(), "Please provide the name of the input object!", fixed = TRUE)
  expect_error(ds.nbclust(df.name = "test_df1"), "Please provide a method for the calculation!", fixed = TRUE)
  expect_error(ds.nbclust(df.name = "test_df1", method = "kmeans", diss = "diss.obj"), "Both, diss and distance are not NULL! Only one argument needs to be given.", fixed = TRUE)
  expect_error(ds.nbclust(df.name = "test_df1", method = "kmeans", distance = NULL), "Both, diss and distance are NULL! One argument needs to be given.", fixed = TRUE)
  expect_error(ds.nbclust(df.name = "D$LAB_TSC", method = "kmeans"), "Only objects of type 'matrix' or 'data.frame' are allowed.", fixed = TRUE)
  expect_error(ds.nbclust(df.name = "test_df1", method = "kmeans", distance = NULL, diss = "test_df2"), "The 'diss' argument is not of type 'dist'.", fixed = TRUE)
  expect_error(ds.nbclust(df.name = "test_df4", method = "kmeans"), "The data frames do not have the same columns. There are columns missing in some data frames!", fixed = TRUE)
  expect_error(ds.nbclust(df.name = "test_df3", method = "kmeans"), "The data frames contain columns which are not of type 'numeric'.", fixed = TRUE)
  
  expect_error(expect_warning(ds.nbclust(df.name = "test_df1", method = "kmeans"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE))
  #err1 <- datashield.errors()
  #expect_equal(err1$sim1, "Command 'nbclustVisualDS(\"test_df1\", \"kmeans\", \"silhouette\", NULL, 10, 100, FALSE, \"steelblue\", \"steelblue\", \"steelblue\", TRUE)' failed on 'sim1': Error while evaluating 'dsClusterAnalysis::nbclustVisualDS(\"test_df1\", \"kmeans\", \"silhouette\", NULL, 10, 100, FALSE, \"steelblue\", \"steelblue\", \"steelblue\", TRUE)' -> Error : The data frames contains NAs.", fixed = TRUE)

  expect_silent(res1)  
  
})


disconnect.studies.dataset.cnsim()
