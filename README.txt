#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is the README for the ODOT data mapping and filtering project.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


PURPOSE OF THIS DOCUMENT
\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
This document is details the work performed on the ODOT value pricing data work.

HOW TO USE
\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
Everything in this folder is self contained. 

IMPORTANT ITEMS:::::
ODOT_mappr_mkdwn.RMD
|-> sources scripts in R folder in order to 
|-> intended to be lightweight
|-------> sources made map from ODOT_data_mappr.R
|-------> RMD files should pull from this

ODOT_data_mappr.R
|-> performs all mapping operation
|-------> data location mapping
|-------> spatial filtering of points 
|-------> link extraction for kept points
|-> reporting metrics
|-------> currently unused 

adobe_readr_combinr.R
|-> combines the data from all adobe_readr_** files
|-> writes out raw data after combining
|-------> performs outflow aggregation

adobe_readr_**.R
|-> extracts and process pdf data
|-> currently does not extract threeway intersections

General Notes:::::
The data folder should not be touched.
 

