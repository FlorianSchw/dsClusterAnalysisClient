


#context("ds.hclust - errors")


connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER", "PM_BMI_CATEGORICAL", "DIS_DIAB", "LAB_TRIG"))

test_that("ds.hclust errors", {
  
  # Creating differing test data.frames in the respective connections for testing if columns check works correctly
  ds.dataFrame(x = c("D$LAB_TSC", "D$GENDER", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_df1", datasources = ds.test_env$connections)
  
  ds.dist(df.name = "test_df1", newobj = "test_diss1", datasources = ds.test_env$connections[1])
  ds.dist(df.name = "test_df1", newobj = "test_diss1", datasources = ds.test_env$connections[2])
  ds.dataFrame(x = c("D$LAB_TSC", "D$GENDER", "D$PM_BMI_CATEGORICAL", "D$DIS_DIAB"), newobj = "test_diss1", datasources = ds.test_env$connections[3])
  
  
  ds.dist(df.name = "test_df1", newobj = "test_diss2", datasources = ds.test_env$connections)
  
  
   
  
  
  # Actual Test Start
  expect_error(ds.hclust(), "Please provide the name of the input object!", fixed = TRUE)
  expect_error(ds.hclust("D$LAB_TSC"),  "Only objects of type 'dist' are allowed for the computation of the clustering.", fixed = TRUE)
  expect_error(ds.hclust("test_diss1"), " End of process!", fixed = TRUE)
  expect_error(ds.hclust("test_diss2", method = "wardD"), "Method needs to be one of the following: 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid'.", fixed = TRUE)
  expect_silent(ds.hclust("test_diss2"))
  })

disconnect.studies.dataset.cnsim()

