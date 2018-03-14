#####For test no. 0
library('RUnit')
library(data.table)
library(lme4) # to build linear mixed model
library(lmerTest) # for outputing test results from the mixed model
library(plyr) #for merging data
library(ipw)

source('~/Documents/RStudio projects1/26_12_2017/FEV_7_7_13/FEV_functions.R')

test.suite <- defineTestSuite("Unit Test of FEV Functions",
                              dirs = file.path("~/Documents/RStudio projects1/26_12_2017/FEV_7_7_13/tests"),
                              testFileRegexp = '^\\w+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)

