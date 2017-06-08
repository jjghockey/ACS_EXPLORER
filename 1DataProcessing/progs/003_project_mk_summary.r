#####################################################################################################
#Engagement		-	UCLA MAS - STAT 405 - Project 													#
#FileName		-	003_project_mk_summary.r				  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	5/30/2017									  									#
#																	  								#
#Purpose:		-	Make summary tables from ACS data for use in Shiny App							#
#																									#
#Notes:			- 	These tables will be outputted and then processed by Shiny 						#
#					The data is too large to allow for native or direct processing in Shiny			#
#																									#
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
	require(ggmap)			#Google Maps
	require(maps)			#Maps
	require(dplyr)			#Better Plyr
	require(ggplot2)		#Graphing Utilities
	require(stringr)		#String Functions
	require(reshape2)		#Data Reshape
	require(readr)			#Better loading tools
	require(tidyverse)		#Tidyverse of packages
	require(data.table)		#Data.table functions
	require(dtplyr)			#Data.table plus dplyr
	require(utils)			#Utilities
	
	require(forcats)		#Factor functions
	
	#Set Options
	options(scipen=30)
	
#II. Data Loading 
	#A. Load each dataset into an R object
		load("../data/acs.rda")	
		
		acs[, puma:=as.character(puma)]
		acs[, puma:=ifelse(str_length(puma)==3, paste("00", puma, sep=""), 
					ifelse(str_length(puma)==4, paste("0", puma, sep=""),puma))
			]
			
		acs[, st:=as.character(st)]
		acs[, st:=ifelse(str_length(st)==1, paste("0", st, sep=""), st)]
		acs[, GEOID10:=paste(st, puma, sep="")]
		acs[, GEOID10:=as.character(GEOID10)]
	
	#B. Mapping 
		load("../data/state.rda") #Shape file and dataset for mapping with PUMA
		stateDF<-stateDF[long<0,]
		stateDF<-stateDF[lat>22,]
		stateDF[, GEOID10:=as.character(GEOID10)]
		stateDF[, my:=1]
		
		state<-unique(acs[, .(stab, GEOID10)], by=c("stab", "GEOID10"))
		
		setkey(stateDF, GEOID10)
		setkey(state, GEOID10)
		nrow(stateDF) #6251300
		stateDF<-state[stateDF,]
		nrow(stateDF) #6251300
		
		stateDF<-stateDF[, .(stab, GEOID10, long, lat, group, PUMA, my)]
		
		gc(reset=TRUE)
		
	#C. Geo 
		geo12<-geo12[, list(pop10=sum(pop10)), by=list(stab, puma, PUMAname, GEOID10)]
		setkey(acs, puma, stab, GEOID10)
		setkey(geo12, puma, stab, GEOID10)
		
		nrow(acs)
		acs<-geo12[acs, ]
		nrow(acs)
		
	#D. Geo - SQMI
		geo12_sqmi<-geo12_sqmi[state!="FIPS state", list(sqmi=sum(as.numeric(LandSQMI))), by=list(stab, puma, PUMAname, GEOID10)]
		setkey(acs, puma, stab, GEOID10)
		setkey(geo12_sqmi, puma, stab, GEOID10)
		
		nrow(acs)
		acs<-geo12_sqmi[acs, ]
		nrow(acs)
		
#III. Data Analysis	
		
	#A. Population Density (United States Heatmap)
		pop<-acs[, list(pop=sum(pwgtp)), by=list(GEOID10, sqmi)]
		pop<-pop[, pop:=pop/sqmi]		
		pop[, zpop:=(pop-mean(pop))/sd(pop)]

		#Bin Population Density
		pop[, pop:=ifelse(pop>0 & pop<1, "0-0.9",
				   ifelse(pop>=1 & pop<20, "1-19.9",
				   ifelse(pop>=20 & pop<90, "20-89.9",
				   ifelse(pop>=90 & pop<500, "90-499.9",
				   ifelse(pop>=500 & pop<2000, "500-1999.9",
				   ifelse(pop>=2000, ">=2000", NA)
				   )))))
		]
		pop[, pop:=factor(pop, levels=c("0-0.9", "1-19.9", "20-89.9", "90-499.9", "500-1999.9", ">=2000"))]
		
		setkey(pop, GEOID10)
		setkey(stateDF, GEOID10)
		
		pop<-as.data.frame(pop)
		pop$mx<-1
		stateDF<-as.data.frame(stateDF)

		stateDF<-full_join(stateDF, pop, by=c("GEOID10"))
		stateDF<-as.data.table(stateDF)
		stateDF[, .N, by=list(mx, my)]
		
		stateDF<-stateDF[mx==my | (my==1 & is.na(mx)==TRUE),]
		stateDF[, mx:=NULL]
		
		pop<-acs[, list(pop=sum(pwgtp)), by=list(GEOID10, sqmi, PUMAname)]
		pop<-pop[, pop:=pop/sqmi]		
		pop[, zpop:=(pop-mean(pop))/sd(pop)]
		pop[, sqmi:=NULL]
				
	#B. Income Heatmap (United States Heatmap)
		#Pct
		inc<-acs[, list(tot_inc=sum(as.numeric(pincp_infl)*as.numeric(pwgtp), na.rm=TRUE)), by=list(GEOID10)]
		inc[, tot:=sum(tot_inc)][, pct:=tot_inc/tot]
		inc[, ztot_inc:=(tot_inc-mean(tot_inc))/sd(tot_inc)]
		
		setkey(inc, GEOID10)
		setkey(stateDF, GEOID10)
		
		inc<-as.data.frame(inc)
		inc$mx<-1
		stateDF<-as.data.frame(stateDF)

		stateDF<-full_join(stateDF, inc, by=c("GEOID10"))
		stateDF<-as.data.table(stateDF)
		stateDF[, .N, by=list(mx, my)]
		
		stateDF<-stateDF[mx==my | (my==1 & is.na(mx)==TRUE),]
		stateDF[, mx:=NULL]		
		
		inc<-acs[, list(tot_inc=sum(as.numeric(pincp_infl)*as.numeric(pwgtp), na.rm=TRUE)), by=list(GEOID10, stab, puma, PUMAname)]
				
		#AVG
		inc2<-acs[, list(avg_inc=weighted.mean(as.numeric(pincp_infl), w=pwgtp, na.rm=TRUE)), by=list(GEOID10)]
		inc2[, zavg_inc:=(avg_inc-mean(avg_inc))/sd(avg_inc)]
		
		setkey(inc2, GEOID10)
		setkey(stateDF, GEOID10)
		
		inc2<-as.data.frame(inc2)
		inc2$mx<-1
		stateDF<-as.data.frame(stateDF)

		stateDF<-full_join(stateDF, inc2, by=c("GEOID10"))
		stateDF<-as.data.table(stateDF)
		stateDF[, .N, by=list(mx, my)]
		stateDF<-stateDF[mx==my | (my==1 & is.na(mx)==TRUE),]	

		stateDF<-stateDF[mx==my | (my==1 & is.na(mx)==TRUE),]
		stateDF[, mx:=NULL]	
		
		inc2<-acs[, list(avg_inc=weighted.mean(as.numeric(pincp_infl), w=pwgtp, na.rm=TRUE),
						 tot_inc=sum(as.numeric(pincp_infl)*as.numeric(pwgtp), na.rm=TRUE)
						)
					, by=list(GEOID10, stab, puma, PUMAname)]		
		
		inc<-inc2[, .(GEOID10, stab, puma, PUMAname, avg_inc, tot_inc)]
		inc[, zinc:=(tot_inc-mean(tot_inc))/sd(tot_inc)]
		inc[, zavg_inc:=(avg_inc-mean(avg_inc))/sd(avg_inc)]
				
	#C. Change in Population (United States Heatmap)
		pop_acs<-acs[, list(tot_pop=sum(pwgtp)), by=GEOID10]
		pop_cen<-geo12[, list(cen_pop=sum(pop10)), by=GEOID10]
		
		setkey(pop_acs, GEOID10)
		setkey(pop_cen, GEOID10)
		
		nrow(pop_acs)
		pop2<-pop_cen[pop_acs, ]
		nrow(pop_acs)
		
		pop2[, pop_grwth:=(tot_pop-cen_pop)/cen_pop,]
		pop2[, cen_pop:=ifelse(pop_grwth>0.50,as.numeric(tot_pop),cen_pop)] 
		pop2[, cen_pop:=ifelse(is.na(pop_grwth)==TRUE,as.numeric(tot_pop),cen_pop)] 
		pop2[, pop_grwth:=(tot_pop-cen_pop)/cen_pop,]	
		pop2[, zpop_grwth:=(pop_grwth-mean(pop_grwth))/sd(pop_grwth)]
			
		setkey(pop2, GEOID10)
		setkey(stateDF, GEOID10)
		
		pop2<-as.data.frame(pop2)
		pop2$mx<-1
		stateDF<-as.data.frame(stateDF)

		stateDF<-full_join(stateDF, pop2, by=c("GEOID10"))
		stateDF<-as.data.table(stateDF)
		stateDF[, .N, by=list(mx, my)]
	
		stateDF<-stateDF[mx==my | (my==1 & is.na(mx)==TRUE),]
		stateDF[, mx:=NULL]	
		
		pop_acs<-acs[, list(tot_pop=sum(pwgtp)
						), by=list(GEOID10, stab, puma, PUMAname, sqmi)]
		pop_acs[, pop_den:=tot_pop/sqmi]
		pop_cen<-geo12[, list(cen_pop=sum(pop10)), by=list(GEOID10, stab, puma, PUMAname)]
		
		setkey(pop_acs, GEOID10, stab, puma, PUMAname)
		setkey(pop_cen, GEOID10, stab, puma, PUMAname)
		
		pop2<-pop_cen[pop_acs,]
		pop2[, pop_grwth:=(tot_pop-cen_pop)/cen_pop,]
		pop2[, cen_pop:=ifelse(pop_grwth>0.50,as.numeric(tot_pop),cen_pop)] 
		pop2[, cen_pop:=ifelse(is.na(pop_grwth)==TRUE,as.numeric(tot_pop),cen_pop)] 
		pop2[, pop_grwth:=(tot_pop-cen_pop)/cen_pop,]	
		
		pop<-pop2[, .(GEOID10, stab, puma, PUMAname, tot_pop, pop_grwth, cen_pop, pop_den)]
		pop[, zpop:=(tot_pop-mean(tot_pop))/sd(tot_pop)]
		pop[, zpop_grwth:=(pop_grwth-mean(pop_grwth))/sd(pop_grwth)]
		pop[, zpop_den:=(pop_den-mean(pop_den))/sd(pop_den)]
		
	#D. Unemployment
		acs[, unemp_flg:=	ifelse(esr==3, 1, 								#Unemployed
							ifelse(is.na(esr)==TRUE | is.na(cow), NA,  		#Underage (<16)
							ifelse(esr==6, NA, 								#Not in Labor Force
							ifelse(cow==9, 1, 0))))							#Unemployed
			]
		
		ur<-acs[, list(
				unemp=sum(ifelse(unemp_flg==1,1,0)*pwgtp, na.rm=TRUE)
			,	emp=sum(ifelse(unemp_flg==0,1,0)*pwgtp, na.rm=TRUE)
			,	nlf=sum(ifelse(is.na(unemp_flg)==TRUE,1,0)*pwgtp)
				), by=list(GEOID10, sqmi)]
		ur[, unemp_den:=unemp/sqmi]
				
		#Bin unemp_denulation Density
		ur[, unemp_den:=ifelse(unemp_den>0 & unemp_den<1, "0-0.9",
				   ifelse(unemp_den>=1 & unemp_den<20, "1-19.9",
				   ifelse(unemp_den>=20 & unemp_den<90, "20-89.9",
				   ifelse(unemp_den>=90 & unemp_den<500, "90-499.9",
				   ifelse(unemp_den>=500 & unemp_den<2000, "500-1999.9",
				   ifelse(unemp_den>=2000, ">=2000", NA)
				   )))))
		]
		ur[, unemp_den:=factor(unemp_den, levels=c("0-0.9", "1-19.9", "20-89.9", "90-499.9", "500-1999.9", ">=2000"))]
											
		ur[, ur:= unemp/(emp+unemp),]
		ur[, zur:=(ur-mean(ur))/sd(ur)]
		ur[, zunemp:=(unemp-mean(unemp))/sd(unemp)]
		ur[, zunemp_den:=(unemp_den-mean(unemp_den))/sd(unemp_den)]	
		ur[, sqmi:=NULL]
		
		setkey(ur, GEOID10)
		setkey(stateDF, GEOID10)
		
		ur<-as.data.frame(ur)
		ur$mx<-1
		stateDF<-as.data.frame(stateDF)

		stateDF<-full_join(stateDF, ur, by=c("GEOID10"))
		stateDF<-as.data.table(stateDF)
		stateDF[, .N, by=list(mx, my)]
		
		stateDF<-stateDF[mx==my | (my==1 & is.na(mx)==TRUE),]
		stateDF[, mx:=NULL]	

		ur<-acs[, list(
				unemp=sum(ifelse(unemp_flg==1,1,0)*pwgtp, na.rm=TRUE)
			,	emp=sum(ifelse(unemp_flg==0,1,0)*pwgtp, na.rm=TRUE)
			,	nlf=sum(ifelse(is.na(unemp_flg)==TRUE,1,0)*pwgtp)
				), by=list(GEOID10, stab, puma, PUMAname, sqmi)]
		ur[, unemp_den:=unemp/sqmi]
		
		#Bin unemp_denulation Density
		ur[, unemp_den:=ifelse(unemp_den>0 & unemp_den<1, "0-0.9",
				   ifelse(unemp_den>=1 & unemp_den<20, "1-19.9",
				   ifelse(unemp_den>=20 & unemp_den<90, "20-89.9",
				   ifelse(unemp_den>=90 & unemp_den<500, "90-499.9",
				   ifelse(unemp_den>=500 & unemp_den<2000, "500-1999.9",
				   ifelse(unemp_den>=2000, ">=2000", NA)
				   )))))
		]
		ur[, unemp_den:=factor(unemp_den, levels=c("0-0.9", "1-19.9", "20-89.9", "90-499.9", "500-1999.9", ">=2000"))]
						
		ur[, ur:= unemp/(emp+unemp),]	
		ur<-ur[, .(GEOID10, stab, puma, PUMAname,ur,unemp,sqmi,unemp_den)]
		
		ur[, zur:=(ur-mean(ur))/sd(ur)]	
		ur[, zunemp:=(unemp-mean(unemp))/sd(unemp)]	
		ur[, sqmi:=NULL]
		
	#D. Age Histogram (Pyramid Bar Chart)
		acs[, age_cat:=	ifelse(agep<=5, "05", 
						ifelse(agep>5 & agep<=10, "10",
						ifelse(agep>10 & agep<=15, "15",
						ifelse(agep>15 & agep<=20, "20",
						ifelse(agep>20 & agep<=25, "25",
						ifelse(agep>25 & agep<=30, "30",
						ifelse(agep>30 & agep<=35, "35",
						ifelse(agep>35 & agep<=40, "40",
						ifelse(agep>40 & agep<=45, "45",
						ifelse(agep>45 & agep<=50, "50",
						ifelse(agep>50 & agep<=55, "55",
						ifelse(agep>55 & agep<=60, "60",
						ifelse(agep>60 & agep<=65, "65",
						ifelse(agep>65 & agep<=70, "70",							
						ifelse(agep>70 & agep<=75, "75",	
						ifelse(agep>75 & agep<=80, "80",														
						ifelse(agep>80, "80+",	NA)))))))))))))))))
			]
			
		male<-acs[sex==1, list(male=sum(pwgtp)), by=age_cat][order(age_cat)]
		female<-acs[sex==2, list(female= sum(pwgtp)), by=age_cat][order(age_cat)]
		
		setkey(male, age_cat)
		setkey(female, age_cat)
		age_hist<-male[female, nomatch=0]
		age_hist<-melt(age_hist, 1)
		age_hist[, age_cat:=as.factor(age_cat)]

		male_st<-acs[sex==1, list(male=sum(pwgtp)), by=list(age_cat, stab)][order(age_cat)]
		female_st<-acs[sex==2, list(female= sum(pwgtp)), by=list(age_cat, stab)][order(stab, age_cat)]
		
		setkey(male_st, age_cat, stab)
		setkey(female_st, age_cat, stab)
		age_hist_st<-male_st[female_st, nomatch=0]
		age_hist_st<-melt(age_hist_st, 1:2)
		age_hist_st[, age_cat:=as.factor(age_cat)]			
	
	#E. Unemployment / Age / Unemployed (Pyramid Chart)	

		#US
		male<-acs[sex==1 & unemp_flg %in% c(0,1), list(male=sum(pwgtp)), by=list(age_cat, educ, unemp_flg)][order(age_cat)]
		female<-acs[sex==2 & unemp_flg  %in% c(0,1), list(female= sum(pwgtp)), by=list(age_cat, educ, unemp_flg)][order(age_cat)]	
	
		setkey(male, age_cat, educ, unemp_flg)
		setkey(female, age_cat, educ, unemp_flg)
		age_ur_educ_hist<-male[female, nomatch=0]
		age_ur_educ_hist<-melt(age_ur_educ_hist, 1:3)
		age_ur_educ_hist[, age_cat:=as.factor(age_cat)]
		
		age_ur_educ_hist[, unemp_cat:=ifelse(unemp_flg==0, "Employed", "Unemployed")]
		age_ur_educ_hist[, tot:=sum(value), by=list(educ, age_cat)]
		age_ur_educ_hist[, ur:=value/tot,] #Unemployment Rate by education, gender and age
		age_ur_educ_hist<-age_ur_educ_hist[unemp_cat=="Unemployed",]
		
		#By State
		male_st<-acs[sex==1 & unemp_flg %in% c(0,1), list(male=sum(pwgtp)), by=list(age_cat, educ, unemp_flg, stab)][order(age_cat)]
		female_st<-acs[sex==2 & unemp_flg  %in% c(0,1), list(female= sum(pwgtp)), by=list(age_cat, educ, unemp_flg, stab)][order(age_cat)]	
	
		setkey(male_st, age_cat, educ, unemp_flg, stab)
		setkey(female_st, age_cat, educ, unemp_flg, stab)
		age_ur_educ_hist_st<-male_st[female_st, nomatch=0]
		age_ur_educ_hist_st<-melt(age_ur_educ_hist_st, 1:4)
		age_ur_educ_hist_st[, age_cat:=as.factor(age_cat)]
		
		age_ur_educ_hist_st[, unemp_cat:=ifelse(unemp_flg==0, "Employed", "Unemployed")]
		age_ur_educ_hist_st[, tot:=sum(value), by=list(educ, stab, age_cat)]
		age_ur_educ_hist_st[, ur:=value/tot,] #Unemployment Rate by education, gender and age
		age_ur_educ_hist_st<-age_ur_educ_hist_st[unemp_cat=="Unemployed",]

	#F. Top 10 Industry
		top10ind<-head(acs[unemp_flg==0 & is.na(ind)==FALSE & ind!="None", list(value=sum(pwgtp)), by=ind][order(-value)],10)
		top10ind[, ord:=1:nrow(top10ind)]
		top10ind[, stab:="All"]
		top10ind<-top10ind[, .(ind, stab, value, ord)]
		top10ind[, tp:="Total"]

		top10ind_st<-acs[unemp_flg==0 & is.na(ind)==FALSE & ind!="None", list(value=sum(pwgtp)), by=list(ind, stab)][order(stab, -value)]
		top10ind_st<-top10ind_st[, ord:=1]
		top10ind_st<-top10ind_st[, ord:=cumsum(ord), by=stab]
		top10ind_st<-top10ind_st[ord<=10, ][order(stab, -value)]
		top10ind_st[, tp:="Total"]
		
		top10ind_wage<-head(acs[unemp_flg==0 & is.na(ind)==FALSE & ind!="None", list(tot_wage=sum(pwgtp*wagp_infl), value=weighted.mean(wagp_infl, w=pwgtp)), by=ind][order(-value)][, tot_wage:=NULL],10)
		top10ind_wage[, ord:=1:nrow(top10ind_wage)]
		top10ind_wage[, stab:="All"]
		top10ind_wage<-top10ind_wage[, .(ind, stab, value, ord)]
		top10ind_wage[, tp:="Wage"]

		top10ind_st_wage<-acs[unemp_flg==0 & is.na(ind)==FALSE & ind!="None", list(tot_wage=sum(pwgtp*wagp_infl), value=weighted.mean(wagp_infl, w=pwgtp)), by=list(ind, stab)][order(stab, -value)]
		top10ind_st_wage<-top10ind_st_wage[, ord:=1]
		top10ind_st_wage<-top10ind_st_wage[, ord:=cumsum(ord), by=stab]
		top10ind_st_wage<-top10ind_st_wage[ord<=10, ][order(stab, -value)][, tot_wage:=NULL]
		top10ind_st_wage[, tp:="Wage"]

		top10ind<-rbind(top10ind, top10ind_st, top10ind_wage, top10ind_st_wage)
		
	#G. Top 10 Occupation
		acs[, occp_descr:=str_trim(gsub(", including funeral service managers and postmasters and mail superintendents", "", occp_descr))]
	
		top10occp_descr<-head(acs[unemp_flg==0 & is.na(occp_descr)==FALSE & occp_descr!="None" & occp_descr!="miscellaneous managers", list(value=sum(pwgtp)), by=occp_descr][order(-value)],10)
		top10occp_descr[, ord:=1:nrow(top10occp_descr)]
		top10occp_descr[, stab:="All"]
		top10occp_descr<-top10occp_descr[, .(occp_descr, stab, value, ord)]
		top10occp_descr[, tp:="Total"]
	
		top10occp_descr_st<-acs[unemp_flg==0 & is.na(occp_descr)==FALSE & occp_descr!="None" & occp_descr!="miscellaneous managers", list(value=sum(pwgtp)), by=list(occp_descr, stab)][order(stab, -value)]
		top10occp_descr_st<-top10occp_descr_st[, ord:=1]
		top10occp_descr_st<-top10occp_descr_st[, ord:=cumsum(ord), by=stab]
		top10occp_descr_st<-top10occp_descr_st[ord<=10, ][order(stab, -value)]
		top10occp_descr_st[, tp:="Total"]
		
		top10occp_descr_wage<-head(acs[unemp_flg==0 & is.na(occp_descr)==FALSE & occp_descr!="None" & occp_descr!="miscellaneous managers", list(tot_wage=sum(pwgtp*wagp_infl), value=weighted.mean(wagp_infl, w=pwgtp)), by=occp_descr][order(-value)][,tot_wage:=NULL],10)
		top10occp_descr_wage[, ord:=1:nrow(top10occp_descr_wage)]
		top10occp_descr_wage[, stab:="All"]
		top10occp_descr_wage<-top10occp_descr_wage[, .(occp_descr, stab, value, ord)]
		top10occp_descr_wage[, tp:="Wage"]
				
		top10occp_descr_st_wage<-acs[unemp_flg==0 & is.na(occp_descr)==FALSE & occp_descr!="None" & occp_descr!="miscellaneous managers", list(tot_wage=sum(pwgtp*wagp_infl), value=weighted.mean(wagp_infl, w=pwgtp)), by=list(occp_descr, stab)][order(stab, -value)]
		top10occp_descr_st_wage<-top10occp_descr_st_wage[, ord:=1]
		top10occp_descr_st_wage<-top10occp_descr_st_wage[, ord:=cumsum(ord), by=stab]
		top10occp_descr_st_wage<-top10occp_descr_st_wage[ord<=10, ][order(stab, -value)][, tot_wage:=NULL]
		top10occp_descr_st_wage[, tp:="Wage"]
		
		top10occp<-rbind(top10occp_descr, top10occp_descr_st, top10occp_descr_wage, top10occp_descr_st_wage)
		
	#H. Distribution of Wages (Overall and by state) 
		wage_dist<-acs[unemp_flg==0 & grepl("full time", ftpt)==TRUE,] 
		wage_dist[, wage:=ifelse(wagp_infl==0, pincp_infl, wagp_infl)]  #Wagp is earnings from wages, Pincp is all earnings
	
		wage_dist<-wage_dist[wage>1000,]
		wage_dist<-wage_dist[, logwage:=log(wage)]
		wage_dist<-wage_dist[, .(stab, logwage, sex)]
						
#IV. Output

	#Save out MAP object for Shiny Import (Shiny Maps)
#	stateDF<-as.data.table(stateDF)
#	stateDF<-stateDF[, .(stab, long, lat, group, pop, zpop, zavg_inc,zpop_grwth, unemp_den, zur)]
#	write_csv(stateDF,"../output/stateDF.csv")  #Single File
	
	#Split into two pieces
	stateDF1<-stateDF[as.numeric(row.names(stateDF))<obs]
	stateDF2<-stateDF[as.numeric(row.names(stateDF>=obs & as.numeric(row.names(stateDF)<(2*obs)]	
	stateDF3<-stateDF[as.numeric(row.names(stateDF))>=(2*obs)]

	write_csv(stateDF1,"../output/stateDF1.csv")  #Single File
	write_csv(stateDF2,"../output/stateDF2.csv")  #Single File
	write_csv(stateDF3,"../output/stateDF3.csv")  #Single File
	
	#Standardize and Save out table object for Shiny Import (Shiny Map summary tables)
 	setkey(pop, GEOID10, stab, puma, PUMAname)
	setkey(inc, GEOID10, stab, puma, PUMAname)
	setkey(ur, GEOID10, stab, puma, PUMAname)
	nrow(pop)
	nrow(ur)
	nrow(inc)
	
	tblDF<-pop[inc, ]
	tblDF<-tblDF[ur, ]
	nrow(tblDF)
	
	tblDF<-as.data.table(tblDF)
	write_csv(tblDF, "../output/tblDF.csv")
	
	#Standardize and Save Objects for Other Graphics
	
	#Age Hist
	age_hist[, stab:="All"]
	age_hist<-rbind(age_hist[, .(age_cat, stab, variable, value)], age_hist_st[, .(age_cat, stab, variable, value)])
	
	ageDF<-as.data.frame(age_hist)
	write_csv(ageDF, "../output/ageDF.csv")
	
	#Age Hist / Unemployment
	age_ur_educ_hist[, stab:="All"]
	age_ur_educ_hist<-rbind(age_ur_educ_hist[, .(age_cat, stab, variable, value, educ, unemp_cat, tot, ur)], age_ur_educ_hist_st[, .(age_cat, stab, educ, variable, value, unemp_cat, tot, ur)])

	ageureducDF<-as.data.table(age_ur_educ_hist)
	write_csv(ageureducDF, "../output/ageureducDF.csv")
	
	#Occupation and Industry
	top10ind<-as.data.table(top10ind)
	write_csv(top10ind, "../output/top10indDF.csv")

	top10occp<-as.data.table(top10occp)
	write_csv(top10occp, "../output/top10occpDF.csv")
	
	#Wage Distribution 
	wage_dist<-as.data.table(wage_dist)
	write_csv(wage_dist, "../output/wagedistDF.csv")
