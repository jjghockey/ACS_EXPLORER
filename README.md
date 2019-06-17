# ACS_EXPLORER
Shiny APP to explore the American Community Survey Data

1DataProcessing - This is a preprocessing step that takes the 2015 Person and Household ACS data and prepares it for display in Shiny

001_project_mk_acs.r - Downloads from ACS FTP and process ACS data.  Data is taken from:

  https://www2.census.gov/programs-surveys/acs/data/pums/2015/1-Year/csv_pus.zip AND
  
  https://www2.census.gov/programs-surveys/acs/data/pums/2015/1-Year/csv_hus.zip 
  
002_project_mk_shapefile.r - Process TIGER PUMA shapefiles for mapping.  Please note that the raw data is not directly downloaded via the .r script.  It has to be manually downloaded.  Since github restricts the file size for uploads, this data is NOT available on this repo.  The files can be downloaded manually at: 

  ftp://ftp2.census.gov/geo/tiger/TIGER2015/PUMA/

003_project_mksummary.r - Process the data created in 001 and 002 to create ggplot ready maps and graphs.  This process breaks down the data and produces much smaller sets that can be easily processed in Shiny in order to display the data.  Files that are created are saved to an output folder and zipped.  These are the files grabbed by the Shiny app. 

2Shiny - This is Shiny processing that takes the output of 003 to create the app.

server.r

ui.r
