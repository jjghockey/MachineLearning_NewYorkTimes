#####################################################################################################
#Engagement		-	UCLA MAS - STAT 412 - Final Project												#
#FileName		-	004_an_model.r							  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	5/13/2017									  									#
#																	  								#
#Purpose:		-	Final Modeling 																	#											
#				- 	GBM, RF																			#	
#				- 																					#
#					Final modeling that uses "Best" parameters.										#
#					h2o with randomized grid searching of 8 hours per algorithm						#
#					uses best parameters from prior GBM and RF models to adjust the tuning 			#
#					parameters for the final modeling 												#
#				-	Training / Test derived from initial data using a 70/30 split 					#
#				-	5-fold CV used on all models.													#
#				-	All modeling performed on Training set, Test set used to evaluate MAE 			#
#					before prediction on final submission set.										#
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
trn<-readRDS("./trn.rds")
trn<-trn[is.na(byline)==FALSE]  #These are comments that do not have article information
trn[, log_rec:=log(ifelse(recommendations==0, 1, recommendations))]

tst<-readRDS("./tst.rds")
tst<-tst[is.na(byline)==FALSE]  #These are comments that do not have article information
tst[, log_rec:=log(ifelse(recommendations==0, 1, recommendations))]

#trn<-rbind(trn,tst) #Recombining sets for full training

tst_sub<-readRDS("./tst_submission.rds") #True Submission Test set

#III. Data Processing -----------------------------------------------------------------------------
#A. Prepare the data for h2o

	setwd("C:/h2o/")  	#The network pathways are too long.  Setting directory to local C:/h2o
						#All h2o objects will be saved here	

	write.csv(file="./trn.csv", trn)
	write.csv(file="./tst.csv", tst)
	write.csv(file="./tst_sub.csv", tst_sub)

	setwd("C:/h2o/")  	#The network pathways are too long.  Setting directory to local C:/h2o
						#All h2o objects will be saved here	
	
	h2o.init(nthreads=6, min_mem_size="24G")
		
	#Load into h2o
	trn<-h2o.importFile("./trn.csv")
	tst<-h2o.importFile("./tst.csv")
	
#B. Set up Grid Search 
		xnames <- names(trn[grepl("log_rec|picURL|inReplyTo|parentID|parentUserDisplayName|createDate_ts|C1|approveDate|
								   permID|createDate|commentTitle|commentSequence|commentBody|approveDate_ts|userTitle|
								   approveDate|element_id|type|articleID|commentID|recommendedFlag|pubDate_dt|
								   status|sharing|updateDate|userDisplayName|userID|userLocation|
								   userTitle|userURL|byline|recommendations|printPage|reportAbuseFlag|typeOfMaterial", names(trn))==FALSE])
			
		#1. Gradient Boosted Model - GBM
		hyper_params_gbm <- list(
		  ntrees = c(50,100,150,200), 
		  max_depth = c(10,15,20), 
		  min_rows = c(5,10,15,20), 
		  learn_rate = c(.07,.08,.09,.1,.11),
		  sample_rate = c(.975,.99,.995,1),
		  col_sample_rate = c(.3,.4,.5,.6,.7),
		  col_sample_rate_per_tree = c(.6,.7,.8,.9,1),
		  nbins_cats = c(32,64,128,256),
		  learn_rate_annealing=c(0.25,0.5,0.75, 1)	
		  
		)
		
		#2. Random Forest
		hyper_params_rf <- list(
		  ntrees = c(25,50,75,100), 
		  max_depth = c(10,15,20), 
		  min_rows = c(5,10,30,70,100), 
		  sample_rate = c(.975,.99,.995,1),
		  col_sample_rate_per_tree = c(.6,.7,.8,.9,1),
		  nbins=c(10,15,20),
		  mtries=c(-1,5,10,15,20,25),
		  nbins_cats = c(512,1024,1536)
		)		
		
		#3. Search Criteria
		search_criteria <- list(
		  strategy = "RandomDiscrete",
		  max_runtime_secs = 28800,  #8 hours per model
		  max_models = 500
		)	
										
#C. Generate the model		
	#1. Gradient Boosted Machine
		gbm3 <- h2o.grid(algorithm = "gbm", 
		                x = xnames, y = "recommendations", 
		                training_frame = trn,
		                hyper_params = hyper_params_gbm,
		                search_criteria = search_criteria,
		                stopping_metric = "MAE", stopping_tolerance = 1e-3, 
		                stopping_rounds = 3,
		                seed = -1,
		                nfolds = 5, fold_assignment = "Modulo",
						distribution = "poisson",
		                keep_cross_validation_predictions = TRUE
		)
		
		gbm3_sort <- h2o.getGrid(grid_id = gbm3@grid_id, sort_by = "MAE", decreasing = FALSE)
		gbm3_sort
		
		gbm3_best <- h2o.getModel(gbm3_sort@model_ids[[1]])
		summary(gbm3_best)	
		
		#Prediction
		pred_gbm3 <- h2o.predict(gbm3_best, newdata = tst, type = "probs")
		pref_gbm3<-h2o.performance(gbm3_best, newdata=tst)		
		
	#2. Random Forest
		rf3 <- h2o.grid(algorithm = "randomForest", 
		                x = xnames, y = "recommendations", 
		                training_frame = trn, 
		                hyper_params = hyper_params_rf,
		                search_criteria = search_criteria,
		                stopping_metric = "MAE", stopping_tolerance = 1e-3, 
		                stopping_rounds = 3,
		                seed = -1,
		                nfolds = 5, fold_assignment = "Modulo", 
						distribution = "poisson",
		                keep_cross_validation_predictions = TRUE
		)
		
		rf3_sort <- h2o.getGrid(grid_id = rf3@grid_id, sort_by = "MAE", decreasing = FALSE)
		rf3_sort
		
		rf3_best <- h2o.getModel(rf3_sort@model_ids[[1]])
		summary(rf3_best)	
		
		#Prediction
		pred_rf3 <- h2o.predict(rf3_best, newdata = tst, type = "probs")
		pref_rf3<-h2o.performance(rf3_best, newdata=tst)		
		
#IV. Output
	
#A. Save Models
		gbm3_best_save <- h2o.saveModel(
		  object = gbm3_best,
		  path = "//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/gbm3.h2o", 
		  force =TRUE
		)
		
		rf3_best_save <- h2o.saveModel(
		  object = rf3_best,
		  path = "//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/rf3.h2o", 
		  force =TRUE
		)		
		save(file="./006_model_paths.h2o", gbm3_best_save, rf3_best_save)
		
#B. Perform Prediction for Kaggle Submission
	tst_sub<-h2o.importFile("C:/h2o/tst_sub.csv")

	load(file="C:/h2o/006_model_paths.h2o")
	rf3_best<-h2o.loadModel(rf3_best_save)
	gbm3_best<-h2o.loadModel(gbm3_best_save)
	
	#RF
	sub_rf3 <- h2o.predict(rf3_best, newdata = tst_sub, type = "probs")
	sub_rf3<-as.data.table(sub_rf3)
	sub_rf3<-sub_rf3[, pred_recs:=round((predict),0)]
	sub_rf3<-sub_rf3[pred_recs<0, pred_recs:=0,]
	
	tst_sub_rf3<-as.data.table(tst_sub)
	tst_sub_rf3<-tst_sub_rf3[, .(commentID)]
		
	submission_rf3<-cbind(tst_sub_rf3, sub_rf3)
	submission_rf3[, predict:=NULL]
	submission_rf3[, commentID:=as.double(commentID)]
	submission_rf3[, commentID:=as.character(as.double(commentID))]
	
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission4a.csv", as.data.frame(submission_rf3), row.names=FALSE)
	
	#GBM
	sub_gbm3 <- h2o.predict(gbm3_best, newdata = tst_sub, type = "probs")
	sub_gbm3<-as.data.table(sub_gbm3)
	sub_gbm3<-sub_gbm3[, pred_recs:=round((predict),0)]
	sub_gbm3<-sub_gbm3[pred_recs<0, pred_recs:=0,]
	
	tst_sub_gbm3<-as.data.table(tst_sub)
	tst_sub_gbm3<-tst_sub_gbm3[, .(commentID)]

	submission_gbm3<-cbind(tst_sub_gbm3, sub_gbm3)
	submission_gbm3[, predict:=NULL]
	submission_gbm3[, commentID:=as.double(commentID)]
	submission_gbm3[, commentID:=as.character(as.double(commentID))]
	
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission4b.csv", as.data.frame(submission_gbm3), row.names=FALSE)
	
	#Simple Ensemble (rf3, gbm3)
	submission_ens<-cbind(submission_gbm3[, .(commentID, gbm3=pred_recs)], submission_rf3[, .(rf3=pred_recs)])
	submission_ens[, pred_recs:=round((gbm3+rf3)/2,0)][, gbm3:=NULL][, rf3:=NULL]
	submission_ens[, commentID:=as.double(commentID)]
	submission_ens[, commentID:=as.character(as.double(commentID))]
	
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission4c.csv", as.data.frame(submission_ens), row.names=FALSE)
	