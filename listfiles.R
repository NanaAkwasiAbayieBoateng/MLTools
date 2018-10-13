library(readxl)
library(readr)
library(plotly)
library(timetk)
library(lubridate)
library("rvest")
library(tibble)
library(tidyr)
pacman::p_load("rio","tidyverse","stringr","ggthemes","RColorBrewer","viridis","data.table","kableExtra",
               "knitr")




setwd("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/FCA/activeenginemount")

path<-"/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/FCA/activeenginemount"

allfiles<-list.files(path = path)

xlsxfiles<-list.files(path = path,pattern = "*.xlsx")
xlsxfiles


#read_excel("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/FCA/activeenginemount/ActiveEngineMount.xlsx")
#
df.list<-sapply(xlsxfiles,read_excel)
df.list

#df<-bind_rows(df.list,.id="id")
#df<-rbindlist(df.list,idcol="id")
#df
