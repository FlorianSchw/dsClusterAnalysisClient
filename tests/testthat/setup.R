
#### Path important for testing and check
FullData <- as.data.frame(read.csv("testsets/TestData_Clustering.csv", sep = ",", dec = "."))
#FullData <- as.data.frame(read.csv("tests/testthat/testsets/TestData_Clustering.csv", sep = ",", dec = "."))

#FullData <- TestData_Clustering
#class(FullData)

#### Re-adjust unit type
FullData$Sex <- as.factor(FullData$Sex)
FullData$Education <- as.factor(FullData$Education)
FullData$Hypertension <- as.factor(FullData$Hypertension)
FullData$Eyes <- as.factor(FullData$Eyes)
FullData$Children <- as.integer(FullData$Children)

#FullData <- subset(FullData, select = -c(1))

#### From here on specific cases could be made
Data1 <- FullData[c(1:500),]
Data2 <- FullData[c(501:1000),]
Data3 <- FullData[c(1001:1500),]
Data4 <- FullData[c(1501:2000),]


#### Defining the server-side data
dslite.server <<- DSLite::newDSLiteServer(tables=list(Data1=Data1,
                                                      Data2=Data2,
                                                      Data3=Data3,
                                                      Data4=Data4))



#### Defining the server-side packages
dslite.server$config(DSLite::defaultDSConfiguration(include=c("dsBase", "dsClusterAnalysis")))
dslite.server$profile()

#### Building the 4 different DSLite Servers with the different datasets
logindata.dslite.data <- data.frame(server = c("Server1", "Server2", "Server3", "Server4"),
                                    url = c("dslite.server", "dslite.server", "dslite.server", "dslite.server"),
                                    user = "",
                                    password = "",
                                    table = c("Data1", "Data2", "Data3", "Data4"),
                                    options = "",
                                    driver = c("DSLiteDriver", "DSLiteDriver", "DSLiteDriver", "DSLiteDriver"))


#### Login to the 4 different DSLite Servers
conns <<- DSI::datashield.login(logindata.dslite.data, assign=TRUE, symbol = "D")

