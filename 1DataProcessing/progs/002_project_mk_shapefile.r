#####################################################################################################
#Engagement		-	UCLA MAS - STAT 405 - Project 													#
#FileName		-	002_project_an_acs_explore.r			  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	4/30/2017									  									#
#																	  								#
#Purpose:		-	Download and prepare shape files that link the map to PUMAS						#
#																									#
#Notes:			- 	Create shape files 																#
#																									#
#																									#
#####################################################################################################

#I. Setup

	#Remove Objects
	rm(list=ls())

	#Clear Memory
	gc(reset=TRUE)

	#Package Install
	library(RCurl) 
	library(rgdal)
	library(rgeos)
	library(maptools)
	library(ggplot2)
	library(dplyr)
	library(data.table)
	library(dtplyr)
	
	#Set Options
	options(scipen=30)

#II. Data Loading

	#A. Get the directory listing
		u <- 'ftp://ftp2.census.gov/geo/tiger/TIGER2015/PUMA/'
		f <- paste0(u, strsplit(getURL(u, ftp.use.epsv = FALSE, ftplistonly = TRUE), 
								'\\s+')[[1]])
							
	#B. Ignore Hawaii/Alaska/Puerto Rico 
	#These make the USA map blow up do not look good when outputed to a graph.
		f <- f[grepl("_78_", f)==FALSE] 
		f <- f[grepl("_02_", f)==FALSE] 
		f <- f[grepl("_15_", f)==FALSE]
												
	#C. Files downloaded directly via FTP. Files located in raw/shape/zip/
	#	Downloading and processing directly from the FTP site caused issues 
	#	and repeated crashes.  Files were downloaded manually and placed into a directory

	setwd("../raw/shape/zip/")

	#D. read in all shps, and prepend shapefile name to IDs
	shps <- lapply(sub('\\.zip', '', basename(f)), function(x) {
	  shp <- readOGR(x, x)
	  shp <- spChFIDs(shp, paste0(x, '_', sapply(slot(shp, "polygons"), slot, "ID")))
	  shp
	})

#III. Data Processing
		
	#A.  rbind to a single object
	state <- do.call(rbind, as.list(shps))

	#B. Add ID / Group variables - These are essential to properly re-contructing the 
	#	map pieces when graphing
	state@data$id <- rownames(state@data)
	statePoints <- fortify(state, region="id")
	stateDF <- inner_join(statePoints, state@data, by="id")
	stateDF$PUMA <- as.integer(levels(stateDF$PUMACE10)[stateDF$PUMACE10])

#IV. Data Output
	
	#A.  write out to USA.shp
	writeOGR(state, '.', 'USA', 'ESRI Shapefile')

	#B.  Save state dataframe 
	stateDF<-as.data.table(stateDF)
	save(file="../../../data/state.rda", stateDF, state)
	
	setwd("../../../progs/")