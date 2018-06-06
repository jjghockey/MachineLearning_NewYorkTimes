#####################################################################################################											#
#FileName		-	007_an_lime.r							  										#
#By				- 	Jeremy Guinta												#
#																	  								#
#Last Update Date:	5/31/2017									  									#
#																	  								#
#Purpose:		-	Lime Review																		#											
#####################################################################################################



#I. Setup -------------------------------------------------------------------------------------------

	#Remove Objects
	rm(list=ls())

	#Clear Memory
	gc(reset=TRUE)
	
	#Set Working Directory
	#setwd("C:/Users/jguinta/Desktop/Working/005_GradSchool/003_Course/STAT412/FINALPROJ/")
	setwd("//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/")

	#Package Install
	require(grid)			#Plotting utilities
	require(gridExtra)		#Plotting utilities	
	require(tidyverse)		#All things tidy 
	require(data.table)		#Data table is better
	require(dtplyr)			#Make sure Data table and dplyr work together
	require(ggplot2)		#Graphing Utilities
	require(stringr)		#String Functions
	require(reshape2)		#Data Reshape
	require(GGally)			#Correlation
	require(h2o)			#Auto ML
	require(lime)			#Open the black box

	#Set Options
	options(scipen=20)
	
	#Graphic Themes
		out_theme <- theme_bw() + 
		  theme(panel.grid.major=element_line(color="white"), 
				text=element_text(family="ArialMT"), 
				legend.position="bottom",
				plot.title = element_text(size = rel(1.0)),
				axis.text.x = element_text(size= rel(1.0)),
				axis.text.y = element_text(size= rel(1.0)))
				
		color_scheme <- c("#6495ED", "#C90E17", "#001933", "#691b14", "#08519c", "#778899", "#B0C4DE", 
							  "#999999", "#000000",  "#800000", "#B23232")   

							  
#II.  Data Loading ---------------------------------------------------------------------------------
trn<-as.data.table(readRDS("./trn.rds"))
trn<-trn[is.na(byline)==FALSE]  #These are comments that do not have article information
trn[, log_rec:=log(ifelse(recommendations==0, 1, recommendations))]

#III. Data Processing -----------------------------------------------------------------------------
#A. Prepare the data for h2o

	setwd("C:/h2o/")  	#The network pathways are too long.  Setting directory to local C:/h2o
						#All h2o objects will be saved here	

	write.csv(file="./trn.csv", trn)	
	h2o.init(nthreads=6, min_mem_size="24G")
		
	#Load into h2o
	trn<-h2o.importFile("./trn.csv")
	xnames <- names(trn[grepl("log_rec|picURL|inReplyTo|parentID|parentUserDisplayName|createDate_ts|C1|approveDate|
								   permID|createDate|commentTitle|commentSequence|commentBody|approveDate_ts|userTitle|
								   approveDate|element_id|type|articleID|recommendedFlag|pubDate_dt|
								   status|sharing|updateDate|userDisplayName|userID|userLocation|
								   userTitle|userURL|byline|printPage|reportAbuseFlag|typeOfMaterial", names(trn))==FALSE])

	trn<-as.data.frame(trn)
	trn<-trn[, c(xnames)]
	
	write.csv(file="./trn.csv", trn)	
	trn<-h2o.importFile("./trn.csv")
	
#B. Perform Lime Review
	load(file="C:/h2o/006_model_paths.h2o")
	rf3_best<-h2o.loadModel(rf3_best_save)
	gbm3_best<-h2o.loadModel(gbm3_best_save)
		
    # Check explainer
	trn<-as.data.table(trn)
	trn[, C1:=NULL]
	trn[, permID:=NULL]
	trn[, commentID:=NULL]
	trn[, recommendations:=NULL]
	trn<-as.data.frame(trn)
	names(trn)
	
	explainer <- lime(as.data.frame(trn), model = gbm3_best)
   
    # Check explanation
	trn<-as.data.table(trn)
			
	#Editor Selection == TRUE
    explanation <- lime::explain(as.data.frame(trn[152341,]), explainer, n_features = 20, kernel_width = 0.5) #Actual recommendations: 3
	lime1<-plot_features(explanation)
	
	tbl1<-data.frame(max(explanation$model_intercept),
				sum(explanation$feature_weight),
				max(explanation$model_prediction),
				val=3,
				ed=TRUE
				)
	tbl
	
setwd("//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/")
ggsave("./006_lime1.png", lime1, height=8, width=11)

	
