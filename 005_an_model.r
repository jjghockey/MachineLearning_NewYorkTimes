#####################################################################################################
#Engagement		-	UCLA MAS - STAT 412 - Final Project												#
#FileName		-	004_an_model.r							  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	5/13/2017									  									#
#																	  								#
#Purpose:		-	Average Ensemble																#											
#				- 	Determine lowest possible validation MAE via an average 						#
#					ensemble of all models predicts (GLM, GBM, RF, NN, autoML)						#
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
	
#A. Load Models

load(file="C:/h2o/006_model_paths.h2o")
load(file="C:/h2o/005_model_paths.h2o")
load(file="C:/h2o/004_model_paths.h2o")

	rf3_best<-h2o.loadModel(rf3_best_save)
	gbm3_best<-h2o.loadModel(gbm3_best_save)

	gbm2_best<-h2o.loadModel(gbm2_best_save)
	rf2_best<-h2o.loadModel(rf2_best_save)

	ml2_best<-h2o.loadModel(ml2_best_save)
	glm2_best<-h2o.loadModel(glm2_best_save)
	nn2_best<-h2o.loadModel(nn2_best_save)

#B. Prediction on Test
	pred_nn2 <- as.data.table(h2o.predict(nn2_best, newdata = tst, type = "probs"))
	setnames(pred_nn2, "pred_nn2")
	
	pred_ml2 <- as.data.table(h2o.predict(ml2_best, newdata = tst, type = "probs"))
	setnames(pred_ml2, "pred_ml2")
	pred_ml2[pred_ml2<0, pred_ml2:=0]
	
	pred_rf2 <- as.data.table(h2o.predict(rf2_best, newdata = tst, type = "probs"))
	setnames(pred_rf2, "pred_rf2")
	
	pred_gbm2 <- as.data.table(h2o.predict(gbm2_best, newdata = tst, type = "probs"))
	setnames(pred_gbm2, "pred_gbm2")
	
	pred_glm2 <- as.data.table(h2o.predict(glm2_best, newdata = tst, type = "probs"))
	setnames(pred_glm2, "pred_glm2")
	pred_glm2[pred_glm2<0, pred_glm2:=0]
	
	pred_rf3 <- as.data.table(h2o.predict(rf3_best, newdata = tst, type = "probs"))
	setnames(pred_rf3, "pred_rf3")
	
	pred_gbm3 <- as.data.table(h2o.predict(gbm3_best, newdata = tst, type = "probs"))
	setnames(pred_gbm3, "pred_gbm3")
	
	final_pred<-cbind(as.data.table(tst), pred_nn2, pred_ml2, pred_rf2, pred_gbm2, pred_glm2, pred_rf3, pred_gbm3)
		
	final_pred[, final_pred:=round((pred_nn2+pred_ml2+(3*pred_rf2)+(2*pred_gbm2)+(0*pred_glm2)+pred_rf3+(2*pred_gbm3))/10,0)]
	final_pred[, mean( abs(final_pred-recommendations) , na.rm=TRUE)] #12.01023
	
#C. Function to find minimized combination of ensembled models

	for (i in 1:10000) {
	
		chk<-i %% 1000
		if (chk==0) {
			print(i)
		}
		
		nums<-round(runif(7, 0,10),0)
		tot_nums<-sum(nums)
		add_nums<-length(nums[nums>1])
		final_pred[, final_pred:=round( ( ( (nums[1]*pred_nn2)  + 
										  (nums[2]*pred_ml2)  + 
										  (nums[3]*pred_rf2)  + 
										  (nums[4]*pred_gbm2) + 
										  (nums[5]*pred_glm2) + 
										  (nums[6]*pred_rf3)  + 
										  (nums[7]*pred_gbm3)) / (tot_nums)),0)] 
		final_pred[editorsSelection==TRUE, final_pred:=final_pred]
		MAE<-final_pred[, mean( abs(final_pred-recommendations) , na.rm=TRUE)] 
		MAE<-as.data.table(cbind(MAE, nums))
		val<-nrow(MAE)
		MAE[, ord:=1:val]
		MAE<-reshape(MAE, idvar=c("MAE"), timevar=c("ord"), direction="wide", sep="")	
		MAE<-cbind(MAE, random_to_add)
		MAE<-cbind(MAE, tot_nums)
		if (i==1) {
			out<-MAE
		}
		else {
			out<-rbind(MAE, out)
		}
	}
	val<-nrow(out)
	out[,ord:=1:val ]
	out[min(MAE)==MAE, ]
		
        # MAE nums1 nums2 nums3 nums4 nums5 nums6 nums7  ord
# 1: 10.85752     1     1     7     1     0     0     3 7403	
	
#IV. Output
		
#A. Perform Prediction for Kaggle Submission
	tst_sub<-h2o.importFile("C:/h2o/tst_sub.csv")

	#Full Ensemble
	pred_nn2 <- as.data.table(h2o.predict(nn2_best, newdata = tst_sub, type = "probs"))
	setnames(pred_nn2, "pred_nn2")
	
	pred_ml2 <- as.data.table(h2o.predict(ml2_best, newdata = tst_sub, type = "probs"))
	setnames(pred_ml2, "pred_ml2")
	pred_ml2[pred_ml2<0, pred_ml2:=0]
	
	pred_rf2 <- as.data.table(h2o.predict(rf2_best, newdata = tst_sub, type = "probs"))
	setnames(pred_rf2, "pred_rf2")
	
	pred_gbm2 <- as.data.table(h2o.predict(gbm2_best, newdata = tst_sub, type = "probs"))
	setnames(pred_gbm2, "pred_gbm2")
	
	pred_glm2 <- as.data.table(h2o.predict(glm2_best, newdata = tst_sub, type = "probs"))
	setnames(pred_glm2, "pred_glm2")
	pred_glm2[pred_glm2<0, pred_glm2:=0]
	
	pred_rf3 <- as.data.table(h2o.predict(rf3_best, newdata = tst_sub, type = "probs"))
	setnames(pred_rf3, "pred_rf3")
	
	pred_gbm3 <- as.data.table(h2o.predict(gbm3_best, newdata = tst_sub, type = "probs"))
	setnames(pred_gbm3, "pred_gbm3")	
	
	final_pred<-cbind(as.data.table(tst_sub), pred_nn2, pred_ml2, pred_rf2, pred_gbm2, pred_glm2, pred_rf3, pred_gbm3)
	
	#Ensemble 1
	final_pred[, final_pred:=round((pred_nn2+pred_ml2+(3*pred_rf2)+(2*pred_gbm2)+(0*pred_glm2)+pred_rf3+(2*pred_gbm3))/10,0)]	
	sub_ens<-final_pred[, .(commentID, pred_recs=final_pred)]
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission5a.csv", as.data.frame(sub_ens), row.names=FALSE)
	
	#Ensemble 2
	
        # MAE nums1 nums2 nums3 nums4 nums5 nums6 nums7  ord
# 1: 10.85752     1     1     7     1     0     0     3 7403	
	
	final_pred[, final_pred:=round(( (1*pred_nn2) +(1*pred_ml2)+(7*pred_rf2)+(1*pred_gbm2)+(0*pred_glm2)+(0*pred_rf3)+(3*pred_gbm3))/13,0)]	
	sub_ens<-final_pred[, .(commentID, pred_recs=final_pred)]
	write.csv(file="//chi1fls02/tsp/LosAngeles/Admin/001_Users/jjg/STAT412/FINALPROJ/submission5b.csv", as.data.frame(sub_ens), row.names=FALSE)
	