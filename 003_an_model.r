#####################################################################################################											#
#FileName		-	003_an_model.r							  										#
#By				- 	Jeremy Guinta													#
#																	  								#
#Last Update Date:	5/13/2017									  									#
#																	  								#
#				- 	AutoML, NN																		#	
#				- 																					#
#					Initial modeling that uses base parameters.										#
#					h2o with randomized grid searching of 4 hours per algorithm						#
#					use baseline best models from this process to determine likely best 			#
#					candidate model and likely candidate parameters									#		
#				- AutoML runs the following: 														#
#					1) RF																			#
#					2) Deep RF 																		#
#					3) GBM																			#
#					4) GLM																			#
#					5) NN																			#
#					6) Ensemble of all models														#
#					7) Ensemble of the best (based on 1-5) models									#
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
	
	h2o.init(nthreads=6, min_mem_size="16G")
		
	#Load into h2o
	trn<-h2o.importFile("./trn.csv")
	tst<-h2o.importFile("./tst.csv")
	

#B. Set up Grid Search 
		xnames <- names(trn[grepl("log_rec|picURL|inReplyTo|parentID|parentUserDisplayName|createDate_ts|C1|approveDate|
								   permID|createDate|commentTitle|commentSequence|commentBody|approveDate_ts|userTitle|
								   approveDate|element_id|type|articleID|commentID|recommendedFlag|pubDate_dt|
								   status|sharing|updateDate|userDisplayName|userID|userLocation|
								   userTitle|userURL|byline|recommendations|printPage|reportAbuseFlag|typeOfMaterial", names(trn))==FALSE])
		
		#1.  Deep Learning - Neural Net - NN
		hyper_params_nn <- list(
		  epochs=20,
		  overwrite_with_best_model=FALSE,
		  hidden=list(c(32,32,32),c(64,64), c(128,128,128)),
		  max_w2=10,
		  score_duty_cycle=0.025,
		  activation=c("Rectifier","Tanh","TanhWithDropout"),
		  input_dropout_ratio=c(0,0.05),
		  score_validation_samples=10000,
		  l1=c(.00001,.000001,.0000001),
		  l2=c(.00001,.000001,.0000001),
		  rho = c(.99,.975,1,0.95),
		  rate=c(.005,.0005,.00005),
		  rate_annealing=c(.00000001,.0000001,.000001),
		  momentum_start=c(.5,.1,.01,.05,.005),
		  momentum_stable=c(0.1, 0.2, 0.3, 0.4,0.5), 
		  momentum_ramp=c(1000000,100000)
		)
				
		#2. GLM/GBM/NN Search Criteria
		search_criteria <- list(
		  strategy = "RandomDiscrete",
		  max_runtime_secs = 28800,  #4 hours per run
		  max_models = 500
		)	
										
#C. Generate the model	
	#1. AutoML
		ml2<-h2o.automl(x=xnames, y="recommendations",
					training_frame=trn,
					stopping_metric="MAE",
					stopping_tolerance=1e-3,
					stopping_rounds=3,
					seed=1,
					nfolds=5,
					max_models =500,
					exclude_algos = c("GLM"),
					max_runtime_secs = 28800  #4 hours
		)
	
		ml2_best <- ml2@leader
		
		#Prediction
		pred_ml2 <- h2o.predict(ml2_best, newdata = tst, type = "probs")
		pref_ml2<-h2o.performance(ml2_best, newdata=tst)
		
		#Manual Performance
		man_pred_ml2<-as.data.table(pred_ml2)
		man_pred_ml2<-man_pred_ml2[, .(pred_recs=round(predict,0))]
		man_tst<-as.data.table(tst)
		man_tst<-man_tst[, .(recommendations)]
		
		man<-cbind(man_pred_ml2, man_tst)
		man[, sum(abs(pred_recs-recommendations), na.rm=TRUE)]/nrow(man) #MAE	14.77185			

	#2. Neural Net	
		nn2 <- h2o.grid(algorithm = "deeplearning", 
		                x = xnames, y = "recommendations", 
		                training_frame = trn,
		                hyper_params = hyper_params_nn,
		                search_criteria = search_criteria,
		                stopping_metric = "MAE", stopping_tolerance = 1e-3, 
		                stopping_rounds = 3,
		                seed = 1,
		                nfolds = 5, fold_assignment = "Modulo", 
						distribution = "poisson",
		                keep_cross_validation_predictions = TRUE
		)
		
		nn2_sort <- h2o.getGrid(grid_id = nn2@grid_id, sort_by = "MAE", decreasing = FALSE)
		nn2_sort
		
		nn2_best <- h2o.getModel(nn2_sort@model_ids[[1]])
		summary(nn2_best)	
		
		#Prediction
		pred_nn2 <- h2o.predict(nn2_best, newdata = tst, type = "probs")
		pref_nn2<-h2o.performance(nn2_best, newdata=tst)
		
		#Manual Performance
		man_pred_nn2<-as.data.table(pred_nn2)
		man_pred_nn2<-man_pred_nn2[, .(pred_recs=round(predict,0))]
		man_tst<-as.data.table(tst)
		man_tst<-man_tst[, .(recommendations)]
		
		man<-cbind(man_pred_nn2, man_tst)
		man[, sum(abs(pred_recs-recommendations), na.rm=TRUE)]/nrow(man) #MAE			

#IV. Output
	
#A. Save Models		
		nn2_best_save <- h2o.saveModel(
		  object = nn2_best,
		  path = "//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/nn2.h2o", 
		  force =TRUE
		)		
		ml2_best_save <- h2o.saveModel(
		  object = ml2_best,
		  path = "//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/ml2.h2o", 
		  force =TRUE
		)		
		
		save(file="./005_model_paths.h2o", nn2_best_save, ml2_best_save)
		
#B. Perform Prediction for Kaggle Submission
	tst_sub<-h2o.importFile("C:/h2o/tst_sub.csv")

	load(file="C:/h2o/005_model_paths.h2o")
	nn2_best<-h2o.loadModel(nn2_best_save)
	ml2_best<-h2o.loadModel(ml2_best_save)
	
	#Auto ML
	sub_ml2 <- h2o.predict(ml2_best, newdata = tst_sub, type = "probs")
	sub_ml2<-as.data.table(sub_ml2)
	sub_ml2<-sub_ml2[, pred_recs:=round((predict),0)]
	sub_ml2<-sub_ml2[pred_recs<0, pred_recs:=0,]
	
	tst_sub_ml2<-as.data.table(tst_sub)
	tst_sub_ml2<-tst_sub_ml2[, .(commentID)]
		
	submission_ml2<-cbind(tst_sub_ml2, sub_ml2)
	submission_ml2[, predict:=NULL]
	submission_ml2[, commentID:=as.double(commentID)]
	submission_ml2[, commentID:=as.character(as.double(commentID))]
	
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission3a.csv", as.data.frame(submission_ml2), row.names=FALSE)
	
	#NN
	sub_nn2 <- h2o.predict(nn2_best, newdata = tst_sub, type = "probs")
	sub_nn2<-as.data.table(sub_nn2)
	sub_nn2<-sub_nn2[, pred_recs:=round((predict),0)]
	sub_nn2<-sub_nn2[pred_recs<0, pred_recs:=0,]
	
	tst_sub_nn2<-as.data.table(tst_sub)
	tst_sub_nn2<-tst_sub_nn2[, .(commentID)]

	submission_nn2<-cbind(tst_sub_nn2, sub_nn2)
	submission_nn2[, predict:=NULL]
	submission_nn2[, commentID:=as.double(commentID)]
	submission_nn2[, commentID:=as.character(as.double(commentID))]
	
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission3b.csv", as.data.frame(submission_nn2), row.names=FALSE) 	
	
	#Simple Ensemble (ml2, nn2)
	submission_ens<-cbind(submission_nn2[, .(commentID, nn2=pred_recs)], submission_ml2[, .(ml2=pred_recs)])
	submission_ens[, pred_recs:=round((nn2+ml2)/2,0)][, nn2:=NULL][, ml2:=NULL]
	submission_ens[, commentID:=as.double(commentID)]
	submission_ens[, commentID:=as.character(as.double(commentID))]
	
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission3c.csv", as.data.frame(submission_ens), row.names=FALSE)
	
