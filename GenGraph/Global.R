library(shinydashboard)
library(ggvis)
library(shiny)
library(dplyr)
library(ggplot2) 
library(DBI)
library(shinyjs)
library(lazyeval)
library(shinyAce)
library(knitr)
library(tidyr)
library(corrplot)
library(ggraph)
library(data.tree)
library(DiagrammeR)
library(manipulate)
library(ggthemes)
library(psych)
library(multcompView)
library(cowplot)
library(lmtest)
library(zoo)


na.rm <- TRUE
args <- commandArgs(TRUE)
argu1 <- args[1]
argu2 <- args[2]
profile <- args[3]

dat.source  <- read.delim("dataSource.csv",  header = TRUE, sep = ";")
#dat.analytics  <- read.delim("analytics.csv",  header = TRUE, sep = ";")
dat.tables <- read.delim("Competency.csv",  header = TRUE, sep = ";")
dat.moodle1 <- read.delim("Plots.csv", header = TRUE, sep = ";",na.strings=c(""))
Query1="select c.fullname, COUNT(ue.id) AS Enroled FROM mdl_course AS c JOIN mdl_enrol AS en ON en.courseid = c.id JOIN mdl_user_enrolments AS ue ON ue.enrolid = en.id GROUP BY c.id ORDER BY c.fullname"

Query2="SELECT mdl_course.fullname as 'Course',mdl_competency.shortname as 'Competency',mdl_grade_items.itemname as 'Activity',mdl_grade_grades.userid,mdl_grade_grades.finalgrade as 'Grade' FROM mdl_grade_grades,mdl_course,mdl_competency,mdl_competency_coursecomp,mdl_grade_items WHERE  mdl_course.id = mdl_competency_coursecomp.courseid AND mdl_competency.id = mdl_competency_coursecomp.competencyid AND mdl_grade_grades.itemid = mdl_grade_items.id and mdl_grade_items.itemmodule='assign'"

Query3="SELECT survey, answer1 from mdl_survey_answers";

plotDataHis <- list(20)
plotname <- list(20)
datsumm <-list(1)
dat.moodle <- list(1)
dataFiltered <-list(1)
vDataF <- list(999)
vDataT <- list(999)

nSel  <- 0
nSelg <- 0

options(shiny.maxRequestSize=100*1024^2)
options(max.print=1000000)

listTitle <- matrix(NA,nrow=20,ncol=20,byrow=TRUE)


      conn <- dbConnect(drv = RMySQL::MySQL(),dbname = "bitnami_moodle",host = "localhost",
                    username = "root")

      #on.exit(dbDisconnect(conn), add = TRUE)
