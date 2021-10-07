# dsClusterAnalysisClient
Client-side package for cluster analysis in DataSHIELD


## About DataSHIELD

DataSHIELD is a software package for non-disclosive federated data analysis using a client - server function architecture. For further information, visit https://www.datashield.ac.uk. The most comprehensive packages - dsBaseClient and dsBase - can be found at https://github.com/datashield/dsBase and https://github.com/datashield/dsBaseClient, respectively.
The dsClusterAnalysisClient package together with the dsClusterAnalysis package (https://github.com/FlorianSchw/dsClusterAnalysis) provide additional functionality for DataSHIELD.


## Introduction


# Installation

install.packages("devtools")

library(devtools)

devtools::install_github("FlorianSchw/dsClusterAnalysisClient")

library(dsClusterAnalysisClient)


# Setting up a Virtual Machine to simulate the server side

In order to use this packages DataSHIELD functionality, the complementary server-side package dsClusterAnalysis needs to be installed on a server together with Opal. For simple development/testing purposes, setting up a Virtual Machine with an Opal/DataSHIELD Image is sufficient in imitating the server side of a real-world application. First of all, you need to download and install VirtualBox following the instructions here: https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/1146454017/v6.1+Windows+Installation+Instructions. 

Subsequently, you need to download the VM Image from this link: https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/1696825345/Workshop+VM+Special. Please note, that there are multiple different VM Images available on the DataSHIELD Wiki with different versions of DataSHIELD, IPs, usernames and passwords. 
Import the VM Image into VirtualBox and follow the setup of the VM Image (https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/1146454017/v6.1+Windows+Installation+Instructions). Start the Virtual Machine and login after the prompt. Login to the Opal Servers using the administrator password. 

Once, you are logged in, you want to update the dsBase package to the newest version and install the dsClusterAnalysis package. Further information on how to do this, can be found here under the “DataSHIELD Administration”: https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/12943477/Opal+management



Alternatively, you can also install the dsClusterAnalysis package by using the following R Code:

install.packages(“opalr”)

library(opalr)

o <- opal.login("administrator", "datashield_test&", url = "http://192.168.56.150:8080/")

dsadmin.install_github_package(o, "dsClusterAnalysis", "FlorianSchw", ref = "main")

opal.logout(o)


Note, that following the installation of the packages, you need to publish the methods of the dsClusterAnalysis package under the Administration -> DataSHIELD -> Packages tab.

# Vignette

In case you want to have a look at the vignette of this package, the above specified VM Image will be able to build the vignette when downloading and installing the package. For this, the VM Image needs to be already running prior to installation of the client side package. To install the dsClusterAnalysisClient package with the vignette, you need to adjust the install function. Please be adviced that installing the package including the vignette can take considerably more time.

devtools::install_github("FlorianSchw/dsClusterAnalysisClient", build_vignettes = TRUE)

## Acknowledgements

I am thankful for the help and support of the DataSHIELD Developer Team.


## Contact

Florian.Schwarz@dife.de


