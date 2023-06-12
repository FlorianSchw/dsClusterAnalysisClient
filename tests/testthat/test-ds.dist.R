

#context("ds.dist - errors")


connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER", "PM_BMI_CATEGORICAL", "DIS_DIAB", "LAB_TRIG"))

test_that("ds.dist errors", {
  
  # Creating differing test data.frames in the respective connections for testing if columns check works correctly
  ds.dataFrame(x = c("D$LAB_TSC", "D$GENDER", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df1", datasources = ds.test_env$connections[1])
  ds.dataFrame(x = c("D$LAB_TSC", "D$LAB_TRIG", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df1", datasources = ds.test_env$connections[2])
  ds.dataFrame(x = c("D$LAB_TSC", "D$LAB_TRIG", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df1", datasources = ds.test_env$connections[3])
  
  
  ds.dataFrame(x = c("D$LAB_TSC", "D$LAB_TRIG", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df2", datasources = ds.test_env$connections[1])
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG", "D$DIS_DIAB"), newobj = "test_df2", datasources = ds.test_env$connections[2])
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG", "D$DIS_DIAB"), newobj = "test_df2", datasources = ds.test_env$connections[3])
  
  
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG", "D$DIS_DIAB"), newobj = "test_df3", datasources = ds.test_env$connections[1])
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG"), newobj = "test_df3", datasources = ds.test_env$connections[2])
  ds.dataFrame(x = c("D$LAB_TSC", "D$PM_BMI_CATEGORICAL", "D$LAB_TRIG"), newobj = "test_df3", datasources = ds.test_env$connections[3])
  
  
  ds.asCharacter(x.name = "D$LAB_TSC", newobj = "char.obj", datasources = ds.test_env$connections[1])
  ds.asNumeric(x.name = "D$LAB_TSC", newobj = "char.obj", datasources = ds.test_env$connections[2])
  ds.asNumeric(x.name = "D$LAB_TSC", newobj = "char.obj", datasources = ds.test_env$connections[3])
  ds.dataFrame(x = c("char.obj", "D$GENDER", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df4", datasources = ds.test_env$connections)
  
  
  ds.completeCases("D", newobj = "D_clean", datasources = ds.test_env$connections)
  
  ds.dataFrame(x = c("D$LAB_TSC", "D$DIS_DIAB"), newobj = "test_df6", datasources = ds.test_env$connections[1])
  
  # Actual Test Start
  expect_error(ds.dist(), "Please provide the name of the input object!", fixed = TRUE)
  expect_error(ds.dist("D$LAB_TSC"), "Only objects of type 'data frame' or 'matrix' are allowed.", fixed = TRUE)
  expect_error(ds.dist("test_df1"), "The data frames do not have the same columns. There are columns missing in some data frames!", fixed = TRUE)
  expect_silent(ds.dist("test_df2"))
  expect_error(ds.dist("test_df3"), "The data frames do not have the same columns. There are columns missing in some data frames!", fixed = TRUE)
  expect_error(ds.dist("test_df4"), "The data frames contain columns which are not of type 'numeric' or 'integer'.", fixed = TRUE)
  expect_error(ds.dist("D_clean", method = "euclidea"), "Method needs to be one of the following: 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'.", fixed = TRUE)
  expect_error(ds.dist("D_clean", method = "x"), "Method needs to be one of the following: 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary' or 'minkowski'.", fixed = TRUE)
  expect_error(ds.dist("test_df6"))
})


disconnect.studies.dataset.cnsim()

