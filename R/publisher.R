#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Function for knitting RMarkdowns.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: N/A
#-------- N/A
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package library~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(magrittr)
library(ezknitr)

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/")
rstudioapi::getSourceEditorContext()$path %>%
  as.character() %>%
  gsub("R.*","\\1", .) %>%
  path.expand() %>%
  setwd()

#knitting markdown~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#sets suffix for file name
#generally best if this is 'system date/time'
suffix = format(Sys.time(), '%Y%m%d_%H%M%S')

#section calls script to run, provides filename suffix, and location
#for terminal usage
# ezknit(file = "yolo.rmd", 
#        out_suffix = suffix,
#        out_dir = "../output",
#        fig_dir = "../myfigs")

#for local script usage
#needs to be different than terminal version
ezknit(file = "ODOT_mappr_mkdwn.rmd",
       out_suffix = suffix,
       out_dir = "./output/Rmrkdwn_Reports",
       fig_dir = "./output/figs")


print("R script successfully ran from terminal.")

