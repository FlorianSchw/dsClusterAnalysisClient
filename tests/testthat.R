# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

#### Is dsBase and dsBaseClient really necessary??
#remotes::install_github("datashield/dsBase", ref = "6.3.0", dependencies=TRUE)
#remotes::install_github("FlorianSchw/dsClusterAnalysis", dependencies=TRUE)

remotes::install_github('datashield/DSLite')
remotes::install_github('datashield/DSOpal')


library(testthat)
library(dsBaseClient)
library(dsBase)
library(DSLite)
library(DSOpal)
library(DSI)
library(dsClusterAnalysisClient)
library(dsClusterAnalysis)


test_check("dsClusterAnalysisClient")
