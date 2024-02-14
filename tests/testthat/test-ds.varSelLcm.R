test_that("datashield_descriptive Errors", {
  
  case1 <- ds.varSelLcm(df = "D", 
                        num.clust = 4)
  
  expect_silent(case1)
  
})



case1 <- ds.varSelLcm(df = "D", 
                      num.clust = 4)


#### Latest warning messages and errors

# $Server2
# [1] "Final cluster creation caused one cluster to have between 1 and 2 observations."
# 
# $Server3
# [1] "Final cluster creation caused one categorical variable to have between 1 and 2 observations in one cluster."
# 
# $Server4
# [1] "Final cluster creation caused one cluster to have between 1 and 2 observations."



# In addition: Warning messages:
#   1: In max(R[i, ][is.finite(R[i, ])]) :
#   no non-missing arguments to max; returning -Inf
# 2: In max(R[i, ][is.finite(R[i, ])]) :
#   no non-missing arguments to max; returning -Inf
# 3: In max(R[i, ][is.finite(R[i, ])]) :
#   no non-missing arguments to max; returning -Inf