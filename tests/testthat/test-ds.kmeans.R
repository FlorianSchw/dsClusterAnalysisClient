

#context("ds.kmeans - errors")


connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER", "PM_BMI_CATEGORICAL", "DIS_DIAB", "LAB_TRIG"))

test_that("ds.kmeans errors", {
  
  # Creating differing test data.frames in the respective connections for testing if columns check works correctly
  ds.dataFrame(x = c("D$LAB_TSC", "D$GENDER", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df1", datasources = ds.test_env$connections)
  ds.dataFrame(x = c("D$LAB_TSC", "D$LAB_TRIG"), newobj = "test_df10", datasources = ds.test_env$connections)
  ds.completeCases("test_df10", newobj = "test_df11")
  
  ds.asCharacter(x.name = "D$LAB_TSC", newobj = "char.obj", datasources = ds.test_env$connections[1])
  ds.asNumeric(x.name = "D$LAB_TSC", newobj = "char.obj", datasources = ds.test_env$connections[2])
  ds.asNumeric(x.name = "D$LAB_TSC", newobj = "char.obj", datasources = ds.test_env$connections[3])
  ds.dataFrame(x = c("char.obj", "D$GENDER", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df2", datasources = ds.test_env$connections)
  
  
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG", "D$DIS_DIAB"), newobj = "test_df3", datasources = ds.test_env$connections[1])
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG"), newobj = "test_df3", datasources = ds.test_env$connections[2])
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG"), newobj = "test_df3", datasources = ds.test_env$connections[3])
  
  
  
  # Actual Test Start
  expect_error(ds.kmeans(), "Please provide the name of the input object!", fixed = TRUE)
  expect_error(ds.kmeans(df.name = "D$LAB_TSC"), "Please provide the number of clusters to be computed!", fixed = TRUE)
  expect_error(ds.kmeans(df.name = "test_df1"), "Please provide the number of clusters to be computed!", fixed = TRUE)
  expect_error(ds.kmeans(df.name = "D$LAB_TSC", 4), "Only objects of type 'data.frame' or 'matrix' are allowed for the  k-means clustering.", fixed = TRUE)
  expect_silent(ds.kmeans(df.name = "test_df11", clusters = 4))  
  expect_error(ds.kmeans(df.name = "test_df10", clusters = 4), "There are some DataSHIELD errors, list them with datashield.errors()")
  expect_error(ds.kmeans(df.name = "test_df1", 4, algorithm = "abc"), "The algorithm needs to be one of the following: 'Hartigan-Wong', 'Lloyd', 'Forgy' or 'MacQueen'.", fixed = TRUE)
  expect_error(ds.kmeans(df.name = "test_df2", 4), "The data frames contain columns which are not of type 'numeric' or 'integer'.", fixed = TRUE)
  expect_error(ds.kmeans(df.name = "test_df3", 4), "The data frames do not have the same columns. There are columns missing in some data frames!", fixed = TRUE)
})


disconnect.studies.dataset.cnsim()
