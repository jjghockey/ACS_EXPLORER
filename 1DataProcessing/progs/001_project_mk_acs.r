#####################################################################################################
#Engagement		-	UCLA MAS - STAT 405 - Project 													#
#FileName		-	001_project_mk_acs.r					  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	5/31/2017									  									#
#																	  								#
#Purpose:		-	Prepare all raw data from ACS													#
#																									#
#Notes:			- 	The data gets very large in memory.  This script was run a machine with 64 GB 	#
#					of memory.  Run using Microsoft R Open 3.4.0									#
#				- 	Data Gathered from https://www2.census.gov/programs-surveys/acs/data/pums/		#
#																									#
#####################################################################################################

#I. Setup

	#Remove Objects
	rm(list=ls())

	#Clear Memory
	gc(reset=TRUE)
	
	#Set Working Directory
	setwd("./")	
	
	#Package Install
	require(gdata)			#Excel processing 
	require(dplyr)			#Better Plyr
	require(ggplot2)		#Graphing Utilities
	require(stringr)		#String Functions
	require(reshape2)		#Data Reshape
	require(readr)			#Better loading tools
	require(tidyverse)		#Tidyverse of packages
	require(data.table)		#Data.table functions
	require(dtplyr)			#Data.table plus dplyr
	require(bit64)			#Numeric precision data.table()
	
	#Options
	options(scipen=30)
	
	#Custom Functions  
	
#II. Data Loading 
	#A. Load each dataset into an R object
	
		#Captures data directly from census FTP site for 2015 only.  
		#Captures 1-year ACS for Personal and Household surveys
	
		#1. Personal Records
		temp <- tempfile()
		download.file("https://www2.census.gov/programs-surveys/acs/data/pums/2015/1-Year/csv_pus.zip", temp)
		acs15a <- fread(unzip(temp, files = "ss15pusa.csv"))
		acs15b <- fread(unzip(temp, files = "ss15pusb.csv"))
		rm(temp)
		
		acs<-rbind(acs15a, acs15b)
		rm(acs15a, acs15b)
		gc(reset=TRUE)
		
		#2. Housing Records		
		temp <- tempfile()
		download.file("https://www2.census.gov/programs-surveys/acs/data/pums/2015/1-Year/csv_hus.zip", temp)
		acs15a <- fread(unzip(temp, files = "ss15husa.csv"))
		acs15b <- fread(unzip(temp, files = "ss15husb.csv"))
		rm(temp)
		
		acs_h<-rbind(acs15a, acs15b)
		rm(acs15a, acs15b)
		gc(reset=TRUE)
		
		save(file="../data/acs_raw.rda", acs_h, acs)

		load("../data/acs_raw.rda")
		
	#B. Housing Level
		#Remove Columns
			acs_h<-as.data.frame(acs_h)
			acs_h<-acs_h[c(1:155)]
			names(acs_h)<-tolower(names(acs_h))
			acs_h<-as.data.table(acs_h)
			
		#Keep Columns of Interest
			acs_h<-acs_h[, .(serialno, division, puma, region, st, np, type, access, fes, partner, ssmc)]
			
		#Clean blank columns
			acs_h<-as.data.frame(acs_h)
			acs_h[acs_h==""]<-NA
			acs_h<-as.data.table(acs_h)
		
	#C. Person Level
		#Remove Columns
			acs<-as.data.frame(acs)
			acs<-acs[c(1:204)]
			names(acs)<-tolower(names(acs))
			acs<-as.data.table(acs)	
			
		#Keep Columns of Interest
			acs<-acs[, .(serialno, sporder, puma, st, adjinc, pwgtp
		  , agep, cit, cow, fer, mar, marhd, marhm, marht, marhw, oip, rac1p
		  , intp, schl, semp, sex, wagp, wkhp, wkw, hicov, indp, esr, sch, schg
		  , msp, nativity, occp, pernp, pincp, socp, waob, mil, mig, wkl, wrk
		  , jwtr, jwmnp, naicsp, migpuma)]
		  
		#Clean blank columns
			acs<-as.data.frame(acs)
			acs[acs==""]<-NA
			acs<-as.data.table(acs)
			
		gc(reset=TRUE)
		
	#D. Match Household to Person.  Only Keep matching records and housing units
		setkey(acs, serialno, puma, st)
		acs[, mx:=1]
		
		setkey(acs_h, serialno, puma, st)
		acs_h[, my:=1]
		
		acs<-as.data.frame(acs)
		acs_h<-as.data.frame(acs_h)		
		acs_all<-full_join(acs, acs_h, by=c("serialno", "puma", "st"))
		acs_all<-as.data.table(acs_all)
		acs_all[, .N, by=list(mx, my)]	

		   # mx my       N
		# 1:  1  1 3147005
		# 2: NA  1  121197 <-Households without people
		
		acs_all<-acs_all[mx==1 & my==1,]
		acs_all[, mx:=NULL]
		acs_all[, my:=NULL]
		
	#E. Geo / PUMA Data	
		
		#This data will supply Census 2010 data linked to PUMA 
	
	#	http://mcdc.missouri.edu/websas/geocorr12.html
		geo12<-fread("../raw/geocorr12.csv")
		geo12<-as.data.frame(geo12)
		names(geo12)[grepl("puma12", names(geo12))==TRUE]<-"puma"
		geo12<-as.data.table(geo12)

		geo12[, puma:=as.character(puma)]
		geo12[, puma:=ifelse(str_length(puma)==3, paste("00", puma, sep=""), 
					ifelse(str_length(puma)==4, paste("0", puma, sep=""),puma))
			]
			
		geo12[, state:=as.character(state)]
		geo12[, state:=ifelse(str_length(state)==1, paste("0", state, sep=""), state)]
		geo12[, GEOID10:=paste(state, puma, sep="")]
		geo12[, GEOID10:=as.character(GEOID10)]
		
	#	http://mcdc.missouri.edu/websas/geocorr12.html
		geo12_sqmi<-fread("../raw/geocorr12_sqmi.csv")
		geo12_sqmi<-as.data.frame(geo12_sqmi)
		names(geo12_sqmi)[grepl("puma12", names(geo12_sqmi))==TRUE]<-"puma"
		geo12_sqmi<-as.data.table(geo12_sqmi)

		geo12_sqmi[, puma:=as.character(puma)]
		geo12_sqmi[, puma:=ifelse(str_length(puma)==3, paste("00", puma, sep=""), 
					ifelse(str_length(puma)==4, paste("0", puma, sep=""),puma))
			]
			
		geo12_sqmi[, state:=as.character(state)]
		geo12_sqmi[, state:=ifelse(str_length(state)==1, paste("0", state, sep=""), state)]
		geo12_sqmi[, GEOID10:=paste(state, puma, sep="")]
		geo12_sqmi[, GEOID10:=as.character(GEOID10)]
		
	#F. OCCP Code	
		#This data links occupation descriptions to occp codes
		#It was derived from ACS documentation
	
		occ<-read.table("../raw/PUMSDataDict15_OCCP.csv", sep="|", header=TRUE) #This file has no delimiter.  Using PIPE to trick the file read in.
		names(occ)<-"occp_full"
		occ$occp_code<-substr(occ$occp_full,1,regexpr(" \\.", occ$occp_full))
		occ$occp_descr<-tolower(substr(occ$occp_full,regexpr(" \\.", occ$occp_full)+2, 255))
		occ$occp_ind<-tolower(substr(occ$occp_descr, 1,regexpr("\\-", occ$occp_descr)-1))
		
		occ<-as.data.table(occ)
		occ[, occ_code:=as.numeric(occp_code)]
		
	#G. State De-code
		#This data links state numerical code into a 2-character alpha code
		#This data was derived from ACS documentation
	
		st<-fread("../raw/PUMSState.csv")
		st[, stab:=toupper(substr(State, regexpr("/", State)+1, 255))]
		st[, State:=substr(State, 1,regexpr("/", State)-1)]
	
		st<-as.data.frame(st)
		names(st)<-c("st", "state", "stab")
		st<-as.data.table(st)
		
#III. Data Processing 
	#A. Add Occupation Codes
		setkey(occ, occ_code)
		occ[, my:=1]

		acs_all[, occp:=as.numeric(occp)]
		setkey(acs_all, occp)
		acs_all[, mx:=1]
		
		acs_all<- occ[acs_all,]
		acs_all[, .N, by=list(mx, my)]		
		   # mx my       N
		# 1:  1 NA 1518296  <-No Occupation Code / Description link.  These are typically blank occp or a designated missing value
		# 2:  1  1 1526700	 
		acs_all[, mx:=NULL]
		acs_all[, my:=NULL]

		acs_all[, occp_descr:=ifelse(is.na(occp_descr)==TRUE, "none", occp_descr)]
		acs_all[, occp_ind:=ifelse(is.na(occp_ind)==TRUE, "none", occp_ind)]
		acs_all[, occp_descr:=ifelse(occp_descr=="", "none", occp_descr)]
		acs_all[, occp_ind:=ifelse(occp_ind=="", "none", occp_ind)]		
		acs_all[, occp_full:=NULL]
		acs_all[, occ_code:=NULL]
		
	#C. State - Link numeric state to alpha-character code.
		setkey(acs_all, st)
		setkey(st, st)
		
		nrow(acs_all)
		acs_all<-st[acs_all, ]
		nrow(acs_all)
			
	#D. Inflation adjusted wages
		acs_all[, adjinc:=adjinc/1000000]
		acs_all[, wagp_infl:=wagp*adjinc]
		acs_all[, pincp_infl:=pincp*adjinc]
		acs_all[, pernp_infl:=pernp*adjinc]
			
	#E. Education - Recode Education from numeric (years of school) into categorical (level acheived)
		acs_all[, educ:=
				ifelse(schl <=15, "Less than Highschool",
				ifelse(schl >15 & schl<=17, "Highschool or Equivalent",
				ifelse(schl >17 & schl<=20, "Some College",
				ifelse(schl ==21, "Bachelors", 
				ifelse(schl ==22, "Masters", 
				ifelse(schl %in% c(23,24), "Professional or PhD", NA))))))
			]
		acs_all[, educ:=as.factor(tolower(educ))]
		acs_all[, educ:=factor(educ, levels=c("less than highschool", "highschool or equivalent", "some college", "bachelors", "masters", "professional or phd"))]

	#F. Part / Full Time - Recode Weekly hours worked into full/part time
		acs_all[, ftpt:=ifelse(wkhp>=35 & wkhp<50, "Full Time (35 to 50 Hours)", 
						ifelse(wkhp>=50 & wkhp<=60, "Full Time (50 to 60 Hours)",
						ifelse(wkhp>60, "Full Time (Greater than 60 Hours)",
						ifelse(wkhp<35, "Part Time (Less than 35 Hours)", NA))))
				]
		acs_all[, ftpt:=as.factor(tolower(ftpt))]

	#G. Industry / Job - Recode naicsp code into an industry  description.  These codes were 
	#					 derived from ACS documentation
		acs_all[, ind:=
				ifelse(substr(naicsp,1,1)=="1", "Agriculture"
			,	ifelse(substr(naicsp,1,2)=="21", "Extraction"
			,	ifelse(substr(naicsp,1,2)=="22", "Utility"
			,	ifelse(substr(naicsp,1,2)=="23", "Construction"
			,	ifelse(substr(naicsp,1,1)=="3", "Manufacturing"
			,	ifelse(substr(naicsp,1,2)=="42", "Wholesale"
			,	ifelse(substr(naicsp,1,2) %in% c("44", "45"), "Retail"
			,	ifelse(substr(naicsp,1,2) %in% c("4m"), "Retail"
			,	ifelse(substr(naicsp,1,2) %in% c("48", "49"), "Transportation"
			,	ifelse(substr(naicsp,1,2)=="51", "Information"
			,	ifelse(substr(naicsp,1,2) %in% c("52", "53"), "Finance"	
			,	ifelse(substr(naicsp,1,2) %in% c("54", "55", "56"), "Professional"	
			,	ifelse(substr(naicsp,1,2)=="61", "Education"
			,	ifelse(substr(naicsp,1,3)=="624", "Community Services"
			,	ifelse(substr(naicsp,1,2)=="62", "Medical"
			,	ifelse(substr(naicsp,1,1)=="7", "Entertainment"						
			,	ifelse(substr(naicsp,1,1)=="8", "Personal Service"
			,	ifelse(substr(naicsp,1,3)=="928", "Military"
			,	ifelse(substr(naicsp,1,3)=="92m", "Government"						
			,	ifelse(substr(naicsp,1,2)=="92", "Government"
			,	ifelse(substr(naicsp,1,1)=="2", "Extraction", "None")))))))))))))))))))))						
			]
		acs_all[, ind:=as.factor(ind)]
		acs_all[, naicsp:=as.factor(naicsp)]
					
	#H. Occupation Codes
		acs_all[, occp_descr:=as.character(occp_descr)]
		acs_all[, occp_descr:=substr(occp_descr, 5,255)]	
		acs_all[, occp_descr:=ifelse(occp_descr=="", "none", occp_descr)]

		acs_all[, occp_code:=as.factor(occp_code)]
		acs_all[, occp_descr:=as.factor(occp_descr)]		
		acs_all[, occp_ind:=as.factor(occp_ind)]		
					
	#I. Save dataset
		acs<-acs_all
		save(file="../data/acs.rda", acs, geo12, geom12_sqmi)
		
