test_that("datashield_descriptive Errors", {
  
  case1 <- ds.varSelLcm(df = "D", 
                        num.clust = 2)
  
  expect_silent(case1)
  
})
















