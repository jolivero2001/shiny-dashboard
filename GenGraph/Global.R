library(shinydashboard)
library(ggvis)
library(shiny)
library(dplyr)
library(ggplot2) 
library(dbConnect)
library(shinyjs)
library(lazyeval)
library(shinyAce)
library(knitr)
library(tidyr)
library(corrplot)


na.rm <- TRUE
args <- commandArgs(TRUE)
argu1 <- args[1]
argu2 <- args[2]
profile <- args[3]

dat.source  <- read.delim("dataSource.csv",  header = TRUE, sep = ";")
#dat.analytics  <- read.delim("analytics.csv",  header = TRUE, sep = ";")
dat.moodle1 <- read.delim("Plots.csv", header = TRUE, sep = ";",na.strings=c(""))
plotDataHis <- list(20)
plotname <- list(20)
datsumm <-list(1)
dat.moodle <- list(1)
dataFiltered <-list(1)
vDataF <- list(20)
vDataT <- list(20)

nSel  <- 0
nSelg <- 0

options(shiny.maxRequestSize=100*1024^2)
options(max.print=1000000)

listTitle <- matrix(NA,nrow=20,ncol=20,byrow=TRUE)


      #conn <- dbConnect(drv = RMySQL::MySQL(),dbname = "moodle",host = "localhost",
                     #username = "root",password = "root")
      #on.exit(dbDisconnect(conn), add = TRUE)
