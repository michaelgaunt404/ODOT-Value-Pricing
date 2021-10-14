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
This document details the work performed on the ODOT value pricing data work.

HOW TO USE
\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
Everything in this folder is self contained. 

IMPORTANT ITEMS:::::DATA
\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
Data from external sources are kept in the data/ folder.
Data generated for this work - eg manually sourced gps coordinates or output files - are contained in the output/ folder. 
Raw extracted data for each data source follows extracted_%%%_data.csv convention. 
extracted_local_data_kept.csv is the final data output to be used in validation/calibration model purposes.


IMPORTANT ITEMS:::::SCRIPTS
\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
GENERAL NOTE
|-> Most of the scripts have a utility section towards the top of the script that 
    defines a relative path. If someone wants to replicate this work they may have 
    to redfine a part of this section so that it can work for them locally. In addition, 
    these sections have to commented out if the script is ran sourced by another script or 
    if the markdown is re-knitted. 

ODOT_mappr_mkdwn.RMD
|-> sources scripts in R folder in order to 
|-> intended to be lightweight
|-------> sources made map from ODOT_data_mappr.R
|-------> RMD files should pull from this

ODOT_data_mappr_full.R
|-> performs all mapping operation
|-------> data location mapping
|-------> spatial filtering of points 
|-------> link extraction for kept points
|-> reporting metrics
|-------> currently unused 

mappr_utility.R
mappr_link_extractr.R
mappr_full_data_coverage.R
|-> seprate files broken-out from ODOT_data_mappr_full.R
|-> broken out to make each script smaller
|-> broken out to make each script responsible for single action

adobe_readr_combinr.R
|-> combines the data from all adobe_readr_** files
|-> writes out raw data after combining
|-------> performs outflow aggregation

adobe_readr_**.R
|-> extracts and process pdf data
|-> currently does not extract threeway intersections

General Notes:::::
The data folder should not be touched.
 

