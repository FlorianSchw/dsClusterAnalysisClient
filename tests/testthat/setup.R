# This is the setup.R file which loads necessary DataSHIELD packages and connects to the Opal Server on the VM
# Be aware that in order to conduct testing the VM needs to be started prior

#context("setup- initialization")

library(DSOpal)
library(dsBaseClient)
library(DSI)
library(dsClusterAnalysisClient)
#library(DSLite)


# Login into the VM Opal Server and connect to the CNSIM data
# VM "Workshop VM (Special)"

source("connection_to_datasets/login_details.R")
source("connection_to_datasets/init_testing_datasets.R")
source("connection_to_datasets/init_studies_datasets.R")






#context("setup - done")


