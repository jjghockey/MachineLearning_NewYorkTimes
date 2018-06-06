#####################################################################################################											#
#FileName		-	002_an_model.r							  										#
#By				- 	Jeremy Guinta												#
#																	  								#
#Last Update Date:	5/13/2017									  									#
#																	  								#
#Purpose:		-	Initial Modeling 																#											
#				- 	GLM, GBM, RF																	#	
#				- 																					#
#					Initial modeling that uses base parameters.										#
#					h2o with randomized grid searching of 4 hours per algorithm						#
#					use baseline best models from this process to determine likely best 			#
#					candidate model and likely candidate parameters									#
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
	
	h2o.init(nthreads=1, min_mem_size="16G")
		
	#Load into h2o
	trn<-h2o.importFile("./trn.csv")
	tst<-h2o.importFile("./tst.csv")
	
#B. Set up Grid Search 
		xnames <- names(trn[grepl("log_rec|picURL|inReplyTo|parentID|parentUserDisplayName|createDate_ts|C1|approveDate|
								   permID|createDate|commentTitle|commentSequence|commentBody|approveDate_ts|userTitle|
								   approveDate|element_id|type|articleID|commentID|recommendedFlag|pubDate_dt|
								   status|sharing|updateDate|userDisplayName|userID|userLocation|
								   userTitle|userURL|byline|recommendations|printPage|reportAbuseFlag|typeOfMaterial", names(trn))==FALSE])
			
		#1. Generalize Linear Models - LM
		hyper_params_glm <- list(
		  alpha = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
		)
    
		#2. Gradient Boosted Model - GBM
		hyper_params_gbm <- list(
		  ntrees = c(5, 10, 25, 50, 100, 250, 500), 
		  max_depth = 5:25, 
		  min_rows = c(5, 10,30,70,100), 
		  learn_rate = c(.01,.03,.05,.08,.1),
		  sample_rate = c(.975,.99,.995,1),
		  col_sample_rate = c(.4,.7,1,1),
		  col_sample_rate_per_tree = c(.7,1,1),
		  nbins_cats = c(64,256,1024)
		)
		
		#3. Random Forest
		hyper_params_rf <- list(
		  ntrees = c(5, 10, 25, 50, 100, 250, 500), 
		  max_depth = 5:25, 
		  min_rows = c(1,5,10,30,70,100), 
		  sample_rate = c(.975,.99,.995,1),
		  col_sample_rate_per_tree = c(.7,1,1),
		  nbins=c(5,10,15,20,25),
		  mtries=c(-1,5,10,15),
		  nbins_cats = c(64,256,1024)
		)				
				
		#4. Search Criteria
		search_criteria <- list(
		  strategy = "RandomDiscrete",
		  max_runtime_secs = 28800,  #4 hours per run
		  max_models = 500
		)	
										
#C. Generate the model		
	#1. Generalize Linear Models
		glm2 <- h2o.grid(algorithm = "glm", 
							x = xnames, y = "recommendations", 
							training_frame = trn,
							hyper_params = hyper_params_glm,
							search_criteria = search_criteria,
							stopping_metric = "mae", stopping_tolerance = 1e-3, 
							stopping_rounds = 3,
							seed = 1,
							nfolds = 5, fold_assignment = "Modulo", 
							keep_cross_validation_predictions = TRUE,
							lambda_search=TRUE
							)
		
		glm2_sort <- h2o.getGrid(grid_id = glm2@grid_id, sort_by = "MAE", decreasing = FALSE)
		glm2_sort
		
		glm2_best <- h2o.getModel(glm2_sort@model_ids[[1]])
		summary(glm2_best)	
		
		#Prediction
		pred_glm2 <- h2o.predict(glm2_best, newdata = tst, type = "probs")
		pref_glm2<-h2o.performance(glm2_best, newdata=tst)
		
		#Manual Performance
		man_pred_glm2<-as.data.table(pred_glm2)
		man_pred_glm2<-man_pred_glm2[, .(pred_recs=round(predict,0))]
		man_tst<-as.data.table(tst)
		man_tst<-man_tst[, .(recommendations)]
		
		man<-cbind(man_pred_glm2, man_tst)
		man[, sum(abs(pred_recs-recommendations), na.rm=TRUE)]/nrow(man) #MAE	21.91653		
		
	#2. Gradient Boosted Machine
		gbm2 <- h2o.grid(algorithm = "gbm", 
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
		
		gbm2_sort <- h2o.getGrid(grid_id = gbm2@grid_id, sort_by = "MAE", decreasing = FALSE)
		gbm2_sort
		
		gbm2_best <- h2o.getModel(gbm2_sort@model_ids[[1]])
		summary(gbm2_best)	
		
		#Prediction
		pred_gbm2 <- h2o.predict(gbm2_best, newdata = tst, type = "probs")
		pref_gbm2<-h2o.performance(gbm2_best, newdata=tst)		
		
		#Manual Performance
		man_pred_gbm2<-as.data.table(pred_gbm2)
		man_pred_gbm2<-man_pred_gbm2[, .(pred_recs=round(predict,0))]
		man_tst<-as.data.table(tst)
		#man_tst<-man_tst[, .(recommendations)]
		
		man<-cbind(man_pred_gbm2, man_tst)
		man[, sum(abs(pred_recs-recommendations), na.rm=TRUE)]/nrow(man) #MAE	14.77185					
		
	#3. Random Forest
		rf2 <- h2o.grid(algorithm = "randomForest", 
		                x = xnames, y = "recommendations", 
		                training_frame = tst, #using the smaller data due to RF memory issues
		                hyper_params = hyper_params_rf,
		                search_criteria = search_criteria,
		                stopping_metric = "MAE", stopping_tolerance = 1e-3, 
		                stopping_rounds = 3,
		                seed = -1,
		                nfolds = 5, fold_assignment = "Modulo", 
						distribution = "poisson",
		                keep_cross_validation_predictions = TRUE
		)
		
		rf2_sort <- h2o.getGrid(grid_id = rf2@grid_id, sort_by = "MAE", decreasing = FALSE)
		rf2_sort
		
		rf2_best <- h2o.getModel(rf2_sort@model_ids[[1]])
		summary(rf2_best)	
		
		#Prediction
		pred_rf2 <- h2o.predict(rf2_best, newdata = trn, type = "probs")
		pref_rf2<-h2o.performance(rf2_best, newdata=trn)		
		
#IV. Output
	
#A. Save Models
		glm2_best_save <- h2o.saveModel(
		  object = glm2_best,
		  path = "//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/glm2.h2o", 
		  force =TRUE
		)
	
		gbm2_best_save <- h2o.saveModel(
		  object = gbm2_best,
		  path = "//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/gbm2.h2o", 
		  force =TRUE
		)
		
		rf2_best_save <- h2o.saveModel(
		  object = rf2_best,
		  path = "//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/rf2.h2o", 
		  force =TRUE
		)		
		
		save(file="./004_model_paths.h2o", gbm2_best_save, rf2_best_save, glm2_best_save)
		
#B. Perform Prediction for Kaggle Submission
	tst_sub<-h2o.importFile("C:/h2o/tst_sub.csv")

	load(file="C:/h2o/004_model_paths.h2o")
	rf2_best<-h2o.loadModel(rf2_best_save)
	gbm2_best<-h2o.loadModel(gbm2_best_save)
	
	#RF
	sub_rf2 <- h2o.predict(rf2_best, newdata = tst_sub, type = "probs")
	sub_rf2<-as.data.table(sub_rf2)
	sub_rf2<-sub_rf2[, pred_recs:=round((predict),0)]
	sub_rf2<-sub_rf2[pred_recs<0, pred_recs:=0,]
	
	tst_sub_rf2<-as.data.table(tst_sub)
	tst_sub_rf2<-tst_sub_rf2[, .(commentID)]
		
	submission_rf2<-cbind(tst_sub_rf2, sub_rf2)
	submission_rf2[, predict:=NULL]
	submission_rf2[, commentID:=as.double(commentID)]
	submission_rf2[, commentID:=as.character(as.double(commentID))]
	
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission2a.csv", as.data.frame(submission_rf2), row.names=FALSE)
	
	#GBM
	sub_gbm2 <- h2o.predict(gbm2_best, newdata = tst_sub, type = "probs")
	sub_gbm2<-as.data.table(sub_gbm2)
	sub_gbm2<-sub_gbm2[, pred_recs:=round((predict),0)]
	sub_gbm2<-sub_gbm2[pred_recs<0, pred_recs:=0,]
	
	tst_sub_gbm2<-as.data.table(tst_sub)
	tst_sub_gbm2<-tst_sub_gbm2[, .(commentID)]

	submission_gbm2<-cbind(tst_sub_gbm2, sub_gbm2)
	submission_gbm2[, predict:=NULL]
	submission_gbm2[, commentID:=as.double(commentID)]
	submission_gbm2[, commentID:=as.character(as.double(commentID))]
	
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission2b.csv", as.data.frame(submission_gbm2), row.names=FALSE) 	#5/18/2018 - Current Leader with ~14 MAE
	
	#Simple Ensemble (rf2, gbm2)
	submission_ens<-cbind(submission_gbm2[, .(commentID, gbm2=pred_recs)], submission_rf2[, .(rf2=pred_recs)])
	submission_ens[, pred_recs:=round((gbm2+rf2)/2,0)][, gbm2:=NULL][, rf2:=NULL]
	submission_ens[, commentID:=as.double(commentID)]
	submission_ens[, commentID:=as.character(as.double(commentID))]
	
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission2c.csv", as.data.frame(submission_ens), row.names=FALSE)
	
