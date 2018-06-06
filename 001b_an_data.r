#####################################################################################################
#Engagement		-	UCLA MAS - STAT 412 - Final Project												#
#FileName		-	001b_an_data.r							  										#
#By				- 	Jeremy Guinta (ID 604882679)													#
#																	  								#
#Last Update Date:	5/11/2017									  									#
#																	  								#
#Purpose:		-	Initial Data Exploration 														#	
#				- 	Proposed Theory - Well written,timely comments on popular articles will 		#
#					have more comment recommendations												#
#				- 	Use features developed in 001a_an_data.r										#
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
	require(sentimentr)		#Sentiment Analysis
	require(quanteda)		#Readability Scores
	require(lubridate)
	
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

	#Custom Functions
	source("./000_mk_functions.r")  #Loads Text to Columns
							  
#II.  Data Loading ---------------------------------------------------------------------------------
#A. Load Training Set

	trn<-readRDS(file="./trn.rds") 
	tst<-readRDS(file="./tst.rds") 
	
#III. Data Processing -----------------------------------------------------------------------------

	trn1<-nrow(trn) # 465777
	trn1_u<-nrow(unique(trn[, .(articleID)])) #
	trn<-trn[is.na(pubDate_dt)==FALSE,] #Remove comments that did not match to articles
	rem1<-nrow(trn) # 448556
	rem1_u<-nrow(unique(trn[, .(articleID)])) #3407	

	tst1<-nrow(tst) # 465777
	tst1_u<-nrow(unique(tst[, .(articleID)])) #
	tst<-tst[is.na(pubDate_dt)==FALSE,] #Remove comments that did not match to articles
	rem2<-nrow(tst) # 448556
	rem2_u<-nrow(unique(tst[, .(articleID)])) #3407		
	
	orig<-trn1+tst1
	orig_u<-trn1_u+tst1_u

	rem<-rem1+rem2
	rem_u<-rem1_u+rem2_u

	
#IV. Data Analysis ---------------------------------------------------------------------------------
	
#A. Build Visuals to compare recommendations to key features

	#1. Recommendations by Editor Selection
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE)), by=list(articleID, editorsSelection, commentID)]
	setkey(tbl, editorsSelection, recs, commentID)
	tbl[, commentID:=as.factor(commentID)]
	ord<-tbl[order(editorsSelection, -recs)][, .(commentID)]
	ord<-as.matrix(ord)
	tbl[, commentID:=factor(commentID, levels=c(ord))]

	p<-ggplot(tbl, aes(x=commentID, y=recs, fill=editorsSelection))+geom_bar(stat="identity")
	p<-p+out_theme
	p<-p+scale_fill_manual(values=color_scheme)
	p<-p+labs(title="Recommendations by Editor Selection", x="commentID", y="Recommendations")
	p<-p+theme(axis.text.x=element_blank())
	graph1<-p

	#2. Recommendations by Editor Selection (Recommendations over time)
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(articleID, editorsSelection)]
	tbl[, avg_recs:=recs/num_com]
	tbl[, ord:=1]
	tbl[, ord:=cumsum(ord), articleID]
	tbl[, maxord:=max(ord), articleID]
	tbl[maxord==2, descr:="The Article has Editor Selected Comments"]
	tbl[maxord==1, descr:="The Article does not have Editor Selected Comments"]

	setkey(tbl, editorsSelection, avg_recs, articleID)
	tbl<-tbl[order(editorsSelection, -avg_recs, articleID)]
	tbl[, articleID:=as.factor(articleID)]

	p<-ggplot(tbl, aes(x=articleID, y=avg_recs, fill=editorsSelection))+geom_bar(stat="identity")
	p<-p+out_theme
	p<-p+facet_wrap(~descr)
	p<-p+scale_fill_manual(values=color_scheme)
	p<-p+labs(title="Average Recommendations by Number of Comments and Editor Selection", subtitle="For Articles when Articles have at Least One Editor Selected Comments", x="articleID", y="Average Recommendations")
	p<-p+theme(axis.text.x=element_blank())
	graph2<-p

	#3. Key Word Rank by Recommendations
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID), kwr=max(kwr)), by=list(articleID, editorsSelection)]
	tbl[, avg_recs:=recs/num_com]

	p<-ggplot(tbl, aes(x=kwr, y=avg_recs, color=editorsSelection)) + geom_point(position="jitter", alpha=0.5, shape=".") + geom_density2d()
	p<-p+stat_density_2d(geom="raster", aes(fill=..density..), contour=FALSE, alpha=0.1, show.legend = FALSE)
	p<-p+out_theme
	p<-p+scale_color_manual(values=color_scheme)
	p<-p+scale_fill_gradient(low="white", high="grey")
	p<-p+labs(title=c("Density of Average Recommendations by Number of Comments, Keyword Rank, and Editor Selection"), subtitle=c("All Articles"))
	p<-p+labs(x="Key Word Rank (Lower is Better)", y="Average Recommendations")
	p<-p+theme(legend.position="bottom")	
	p<-p+xlim(-100,3000)
	p<-p+ylim(-100,500)
	graph3<-p

	#4. Topic Analysis
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(topic)]
	tbl[, topic:=gsub("[^A-Za-z]", "", topic)]
	tbl[, avg_recs:=recs/num_com]

	setkey(tbl, avg_recs, topic)
	tbl<-tbl[order(-avg_recs, topic)]
	tbl[, topic:=as.factor(topic)]
	ord<-tbl[order(-avg_recs)][, .(topic)]
	ord<-as.matrix(ord)
	tbl[, topic:=factor(topic, levels=c(ord))]

	p<-ggplot(tbl[is.na(topic)==FALSE,], aes(x=topic, y=avg_recs))+geom_bar(stat="identity")
	p<-p+out_theme
	p<-p+scale_fill_manual(values=color_scheme)
	p<-p+labs(title="Average Recommendations by Number of Comments and Topic", subtitle="All Articles", x="Topic", y="Average Recommendations")
	p<-p+theme(axis.text.x = element_text(size= rel(0.50)))
	graph4<-p

	#5. Specific Topic (Highest Ranking Key Word) 
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(specific)]
	tbl[, specific:=gsub("[^A-Za-z]", "", specific)]
	tbl[, avg_recs:=recs/num_com]

	setkey(tbl, avg_recs, specific)
	tbl<-tbl[order(-avg_recs, specific)]
	tbl[, specific:=as.factor(specific)]
	ord<-tbl[order(-avg_recs)][, .(specific)]
	ord<-as.matrix(ord)
	tbl[, specific:=factor(specific, levels=c(ord))]

	p<-ggplot(tbl[is.na(specific)==FALSE,], aes(x=specific, y=avg_recs))+geom_bar(stat="identity")
	p<-p+out_theme
	p<-p+scale_fill_manual(values=color_scheme)
	p<-p+labs(title="Average Recommendations by Number of Comments and Specific Topics", subtitle="All Articles", x="Specific Topic", y="Average Recommendations")
	p<-p+theme(axis.text.x=element_blank())
	graph5<-p

	#6. Key Word Rank (Top 3) 
	#1
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(kw1)]
	tbl[, avg_recs:=recs/num_com]

	setkey(tbl, avg_recs, kw1)
	tbl<-tbl[order(-avg_recs, kw1)]
	tbl[, kw1:=as.factor(kw1)]
	ord<-tbl[order(-avg_recs)][, .(kw1)]
	ord<-unique(as.matrix(ord))
	tbl[, kw1:=factor(kw1, levels=c(ord))]
	tbl[1:10,]

	p<-ggplot(tbl[is.na(kw1)==FALSE,], aes(x=kw1, y=avg_recs))+geom_bar(stat="identity")
	p<-p+out_theme
	p<-p+scale_fill_manual(values=color_scheme)
	p<-p+labs(title="Average Recommendations by Number of Comments and Top Key Word", subtitle="All Articles", x="Top Keyword", y="Average Recommendations")
	p<-p+theme(axis.text.x=element_blank())
	graph6<-p

	#2
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(kw2)]
	tbl[, avg_recs:=recs/num_com]

	setkey(tbl, avg_recs, kw2)
	tbl<-tbl[order(-avg_recs, kw2)]
	tbl[, kw2:=as.factor(kw2)]
	ord<-tbl[order(-avg_recs)][, .(kw2)]
	ord<-unique(as.matrix(ord))
	tbl[, kw2:=factor(kw2, levels=c(ord))]
	tbl[1:10,]

	p<-ggplot(tbl[is.na(kw2)==FALSE,], aes(x=kw2, y=avg_recs))+geom_bar(stat="identity")
	p<-p+out_theme
	p<-p+scale_fill_manual(values=color_scheme)
	p<-p+labs(title="Average Recommendations by Number of Comments and Top Key Word", subtitle="All Articles", x="2nd Highest Keyword", y="Average Recommendations")
	p<-p+theme(axis.text.x=element_blank())
	graph7<-p

	#3
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(kw3)]
	tbl[, avg_recs:=recs/num_com]

	setkey(tbl, avg_recs, kw3)
	tbl<-tbl[order(-avg_recs, kw3)]
	tbl[, kw3:=as.factor(kw3)]
	ord<-tbl[order(-avg_recs)][, .(kw3)]
	ord<-unique(as.matrix(ord))
	tbl[, kw3:=factor(kw3, levels=c(ord))]
	tbl[1:10,]

	p<-ggplot(tbl[is.na(kw3)==FALSE,], aes(x=kw3, y=avg_recs))+geom_bar(stat="identity")
	p<-p+out_theme
	p<-p+scale_fill_manual(values=color_scheme)
	p<-p+labs(title="Average Recommendations by Number of Comments and Top Key Word", subtitle="All Articles", x="3rd Highest Keyword", y="Average Recommendations")
	p<-p+theme(axis.text.x=element_blank())
	graph8<-p

	#7. DOW / timeofday (Heatmap)
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(timeofday, dow)]
	tbl[, avg_recs:=recs/num_com]
	tbl[, rescale:=scale(avg_recs)]
	tbl[, dow:=factor(dow, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))]
	tbl[, timeofday:=factor(timeofday, levels=c("Morning", "Afternoon", "Evening", "Late Night"))]

	p<-ggplot(tbl, aes(x=dow, y=as.factor(timeofday)))+geom_tile(aes(fill=rescale), colour="white")
	p<-p+scale_fill_gradient(low="white", high="#001933")
	p<-p+labs(title="Density of Average Recommendations by Number of Comments", subtitle="All Articles", x="Day of Week",  y="Time of Day")
	p<-p+out_theme
	p<-p+theme(legend.title=element_blank())
	p<-p+theme(legend.position = "none")
	graph9<-p		

	#8. DOW / timeofday (Heatmap)
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(timeofday, dow)]
	tbl[, rescale:=scale(recs)]
	tbl[, dow:=factor(dow, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))]
	tbl[, timeofday:=factor(timeofday, levels=c("Morning", "Afternoon", "Evening", "Late Night"))]

	p<-ggplot(tbl, aes(x=dow, y=as.factor(timeofday)))+geom_tile(aes(fill=rescale), colour="white")
	p<-p+scale_fill_gradient(low="white", high="#001933")
	p<-p+labs(title="Density of Recommendations", subtitle="All Articles", x="Day of Week",  y="Time of Day")
	p<-p+out_theme
	p<-p+theme(legend.title=element_blank())
	p<-p+theme(legend.position = "none")
	graph10<-p	

	#9. Sentiment / Grade level All Recommendations
	p<-ggplot(trn[readCL>0 & readCL<=16], aes(readCL, recommendations, color=com_cat))+geom_point(alpha=0.25)
	p<-p+labs(title="Scatter of Grade Reading Level, Recommendations, and Comment Sentiment", subtitle="All Comments", x="Grade Level",  y="Recommendations")
	p<-p+out_theme
	p<-p+scale_color_manual(values=color_scheme)
	p<-p+theme(legend.title=element_blank())
	p<-p+theme(legend.position = "bottom")
	graph11<-p	

	#10. Sentiment / Grade level (Average Recommendations)
	tbl<-trn[readCL>0 & readCL<=16, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(readCL, com_cat)]
	tbl[, avg_recs:=recs/num_com]

	p<-ggplot(tbl, aes(readCL, avg_recs, color=com_cat))+geom_point(alpha=0.25)
	p<-p+labs(title="Scatter of Grade Reading Level, Avereage Recommendations by Number of Comments, and Comment Sentiment", subtitle="All Articles", x="Grade Level",  y="Average Recommendations")
	p<-p+out_theme
	p<-p+scale_color_manual(values=color_scheme)
	p<-p+theme(legend.title=element_blank())
	p<-p+theme(legend.position = "bottom")
	graph12<-p	

	#11. Sentiment / Grade level (Heatmap)
	tbl<-trn[readCL>0 & readCL<=16, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(readCL, com_cat)]
	tbl[, rescale:=scale(recs)]
	tbl[, readCL:=as.factor(readCL)]

	p<-ggplot(tbl, aes(x=readCL, y=as.factor(com_cat)))+geom_tile(aes(fill=rescale), colour="white")
	p<-p+scale_fill_gradient(low="white", high="#001933")
	p<-p+labs(title="Density of Recommendations", subtitle="All Articles", x="Grade Level",  y="Comment Sentiment")
	p<-p+out_theme
	p<-p+theme(legend.title=element_blank())
	p<-p+theme(legend.position = "none")
	graph13<-p	

	#12. Time to Post Cat / timeofday (Heatmap)
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(timeofday, time_to_post_cat)]
	tbl[, rescale:=scale(recs)]
	tbl[, time_to_post_cat:=factor(time_to_post_cat, levels=c("Within 15 minutes", "15 to 29 minutes", "30 to 44 minutes", "45 minutes to 1 Hour", "1 to 2 Hours", "2 to 4 Hours", "4 to 12 Hours", ">12 Hours"))]
	tbl[, timeofday:=factor(timeofday, levels=c("Morning", "Afternoon", "Evening", "Late Night"))]

	p<-ggplot(tbl, aes(x=time_to_post_cat, y=as.factor(timeofday)))+geom_tile(aes(fill=rescale), colour="white")
	p<-p+scale_fill_gradient(low="white", high="#001933")
	p<-p+labs(title="Density of Recommendations", subtitle="All Articles", x="Time to Post Category",  y="Time of Day")
	p<-p+out_theme
	p<-p+theme(legend.title=element_blank())
	p<-p+theme(legend.position = "none")
	p<-p+theme(axis.text.x = element_text(size= rel(0.50)))
	graph14<-p	

	#13. Time to Post Cat / timeofday (Heatmap)
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(timeofday, time_to_post_cat)]
	tbl[, avg_recs:=recs/num_com]
	tbl[, rescale:=scale(avg_recs)]
	tbl[, time_to_post_cat:=factor(time_to_post_cat, levels=c("Within 15 minutes", "15 to 29 minutes", "30 to 44 minutes", "45 minutes to 1 Hour", "1 to 2 Hours", "2 to 4 Hours", "4 to 12 Hours", ">12 Hours"))]
	tbl[, timeofday:=factor(timeofday, levels=c("Morning", "Afternoon", "Evening", "Late Night"))]

	p<-ggplot(tbl, aes(x=time_to_post_cat, y=as.factor(timeofday)))+geom_tile(aes(fill=rescale), colour="white")
	p<-p+scale_fill_gradient(low="white", high="#001933")
	p<-p+labs(title="Density of Average Recommendations", subtitle="All Articles", x="Time to Post Category",  y="Time of Day")
	p<-p+out_theme
	p<-p+theme(legend.title=element_blank())
	p<-p+theme(legend.position = "none")
	p<-p+theme(axis.text.x = element_text(size= rel(0.50)))
	graph15<-p	
	
	#14. Comment Order 
	tbl<-trn[, list(recs=sum(recommendations, na.rm=TRUE), num_com=n_distinct(commentID)), by=list(com_pos_cat, articleID)]
	tbl[, avg_recs:=recs/num_com]
	tbl[, com_pos_cat:=factor(com_pos_cat, levels=c("Top 10", "11 to 50", "51 to 100", "101 to 200", ">200"))]
	
	p<-ggplot(tbl, aes(x=com_pos_cat, y=avg_recs))+geom_boxplot()
	p<-p+scale_fill_gradient(low="white", high="#001933")
	p<-p+labs(title="Boxplot of Average Recommendations", subtitle="All Articles", x="Position Category", y="Average Recommendations")
	p<-p+out_theme
	p<-p+theme(legend.title=element_blank())
	p<-p+theme(legend.position = "none")
	graph16<-p		