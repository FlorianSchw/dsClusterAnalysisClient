



connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER", "PM_BMI_CATEGORICAL", "DIS_DIAB", "LAB_TRIG"))

test_that("ds.clusterPlot errors", {
  
  # Creating differing test data.frames in the respective connections for testing if columns check works correctly
  ds.dataFrame(x = c("D$LAB_TSC", "D$GENDER", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df1", datasources = ds.test_env$connections)
  ds.dist(df.name = "test_df1", newobj = "test_diss1", datasources = ds.test_env$connections)
  ds.hclust(diss = "test_diss1", newobj = "test_hclust1")
  
  
  ds.hclust(diss = "test_diss1", newobj = "test_hclust2", datasources = ds.test_env$connections[1])
  ds.hclust(diss = "test_diss1", newobj = "test_hclust2", datasources = ds.test_env$connections[2])
  ds.dataFrame(x = c("D$LAB_TSC", "D$GENDER", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_hclust2", datasources = ds.test_env$connections[3])
  
  
  # Actual Test Start
  expect_error(ds.clusterPlot(), "Please provide the name of the input object!", fixed = TRUE)

  expect_error(ds.clusterPlot(tree = "D$LAB_TSC", k = 4), "Only objects of type 'hclust' are allowed.", fixed = TRUE)
  expect_error(ds.clusterPlot(tree = "test_df1", k = 4), "Only objects of type 'hclust' are allowed.", fixed = TRUE)
  expect_error(ds.clusterPlot(tree = "test_diss1", k = 4), "Only objects of type 'hclust' are allowed.", fixed = TRUE)
  expect_error(ds.clusterPlot(tree = "test_hclust2", k = 4), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
  expect_silent(ds.clusterPlot(tree = "test_hclust1", k = 4))
  expect_error(ds.clusterPlot(tree = "test_hclust1", k = 4, h = 10), "Please specify only one of 'k' or 'h'.", fixed = TRUE)
})


disconnect.studies.dataset.cnsim()
