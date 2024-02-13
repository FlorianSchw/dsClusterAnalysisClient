test_that("datashield_descriptive Errors", {
  
  case1 <- ds.varSelLcm(df = "D", 
                        num.clust = 4)
  
  expect_silent(case1)
  
})



?str_replace
?regex
