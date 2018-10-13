
list=c("tidyverse","stringr","forcats","ggmap","rvest","tm","SnowballC","dplyr","calibrate","doParallel",
       "stringi","ggplot2","maps","httr","rsdmx","devtools","plyr","dplyr","ggplot2","caret","elasticnet",
       "magrittr","broom","glmnet","Hmisc",'knitr',"RSQLite","RANN","lubridate","ggvis","plotly","lars",
       "ggcorrplot","GGally","ROCR","lattice","car","corrgram","ggcorrplot","parallel","readxl","ggmosaic",
       "vcd","Amelia","d3heatmap","ResourceSelection","ROCR","plotROC","DT","aod","mice","Hmisc","data.table",
       "rmarkdown")



list_packages <- list
new.packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

R<-suppressWarnings(suppressMessages(sapply(list, require, character.only = TRUE)))

setwd("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience")

rmarkdown::render("interactiveHeatmap.R")

render("interactiveHeatmap.Rmd", html_document())


rmarkdown::render("rmarkdown1.R")


render("interactiveHeatmap.Rmd", "pdf_document")