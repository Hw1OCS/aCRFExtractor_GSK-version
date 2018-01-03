
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(pdftools, stringr, tidyr, xlsx, sqldf)     ## would be handy

# library(devtools)
library(pdftools)
library(stringr)
# library(tidyr)    ## to unlist variable
library(tidyverse)
library(xlsx)     ## obsolete due to its dependency on rJava
library(readxl)     ## to import xls file
library(readr)      ## part of pkg::tidyr, to export xls/csv file
library(sqldf)
# library(tcltk)      ## to select directory for output location

## for choose.folder powered by pkg::bioimagetools
# setRepositories(ind=c(1,2))
# install.packages(c("devtools","tiff","EBImage"))
# devtools::install_github("volkerschmid/bioimagetools")
