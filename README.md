# LSFE-R
R scripts by members of the Department of Remote Sensing at the University of Wuerzburg


### Folder description:

* ***00_DataDownload***: Functions to download satellite and auxiliary data.

* ***01_SortData***: Functions to store satellite data within a consistent data structure.

* ***02_PreProcess***: Tools for general pre-processing of satellite data (e.g. crop, reproject) following the structure provided by the scripts in 01_SortData.

* ***03_DataAnalysis***: Scripts for generating products and/or statistics based on single time-steps or time series of satellite imagery. Should respect the structure of 01_SortData.

* ***04_Generic***: Generic functions(e.g. estimate UTM zone from Lat-Lon) which might be of use.
