#######################/#############################################################################
#								# 
#FileName		-	001_mk_data.r							  										# 
#By				- 	Jeremy Guinta												# 
#																	  								# 
#Last Update Date:	5/11/2017									  									# 
#																	  								# 
#Purpose:		-	Build data and features															# 		
#				- 	Proposed theory: Well written, timely comments on popular articles will have 	# 
#					more recommendations.  Build features to capture these effects					# 
#																									# 
#				-	Article Features:                                                               # 
#					1. Key	Words - Parse key word string and reorg into a key word ranking. Rank   # 
#					each article by a key word rank that will separate popular articles away from   # 
#					less popular articles on a continuous scale.                                    # 
#					2. Topic Analysis - Group articles in general and specific topics               # 
#					3. Date / Time / DOW / Time of Day of the article based on publish date         # 
#					4. Sentiment Analysis on article snippet                                        # 
#                                                                                                   # 
#				- 	Comment Features																# 
#					1. Number of unique users posting on an article                                 # 
#					2. Rank order of the comment                                                    # 
#					3. Date / Time / Time to Post (from publish date) of a comment                  # 
#					4. Sentiment Analysis (comment positive or negative)                            # 
#					5. Comment length                                                               # 
#					6. Reading Grade level of the comment                                           # 
#													                                                # 
#				- 	Useful Existing Features 														# 
#					1. Editors Selection 															#
#					2. replyCount																	#
#					3. newDesk																		#
#					4. articleWordCount																#
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
trn_art<-fread("./train_articles.csv")
trn_art[, type:="trn"]
trn_com<-fread("./train_comments.csv")
trn_com[, type:="trn"]
tst_art<-fread("./test_articles.csv")
tst_art[, type:="tst"]
tst_com<-fread("./test_comments.csv")
tst_com[, type:="tst"]

#III. Data Processing -----------------------------------------------------------------------------
#A. Combine the Data - For convenience of feature building
	
	#Articles
	art<-bind_rows(trn_art, tst_art)
	art[, .N, by=type]
	
	   # type    N
	# 1:  trn 3445
	# 2:  tst 1324	
	
	#Comments
	trn_com[, editorsSelection:=ifelse(editorsSelection=="0" | editorsSelection=="False", FALSE,
								ifelse(editorsSelection=="1" | editorsSelection=="True", TRUE, NA))
		]
	com<-bind_rows(trn_com, tst_com)	
	com[, .N, by=type]	
	   # type      N
	# 1:  trn 665396
	# 2:  tst 264924	

#B. Article Features 

	#1. Keywords

		#a. Split the Keywords
		art[, keywords:=gsub("\\[", "", keywords)]
		art[, keywords:=gsub("\\]", "", keywords)]
		art[, keywords:=gsub("',", "|", keywords)]
		art[, keywords:=gsub("'", "", keywords)]
		
		art<-text_to_columns(as.data.frame(art), column="keywords", delimiters=c("|"))
		art<-as.data.table(art)
		
		#b. Loop through the data, combine the keywords and sort them
		tot<-nrow(art)
		art[, ord:=1:nrow(art)]
		for (i in 1:tot) {
			print(i)
			tmp<-art[ord==i, c(1, 7:30)]
			for (j in 1:24) {  #There are 24 "new" values created from the text_to_columns
					val<-paste("new", j, sep="")
					chk<-tmp[, .(articleID, new=get(val))]
					chk[, new:=as.character(new)]
				if (j ==1) {
					out<-chk
				}
				else {
					out<-rbind(out, chk)
				}
			}
			out<-out[is.na(new)==FALSE,]
			out<-out[order(new)]
			if (i==1) {
				out2<-out
			}
			else {
				out2<-bind_rows(out,out2)
			}			
		}
		
		#c. Combine and build rankings
		tot_out<-out2[, .N, by=list(new)]
		out3<-as.data.table(inner_join(out2, tot_out, by=c("new"="new")))
		out3<-out3[order(-N, new, articleID)]
		out3[new!=lag(new), keyword_rank:=1]
		out3[is.na(keyword_rank)==TRUE,keyword_rank:=0]
		out3[, keyword_rank:=cumsum(keyword_rank)]
		out3[, keyword_rank:=keyword_rank+1]
		out3<-out3[order(articleID, -N, keyword_rank)]
		out3[, ord2:=1]
		out3[, ord2:=cumsum(ord2), by=list(articleID)]
		
		#d. Keywords will be matched back to the article data
		keywords<-reshape(out3[, .(new, articleID, keyword_rank, ord2)], idvar=c("articleID"), timevar=c("ord2"), direction="wide", sep="")
		
	#2. Date / Time 
		art[, pubDate_dt:=gsub("T", "", pubDate)]
		art[, pubDate_dt:=gsub("Z", "", pubDate_dt)]
		art[, pubDate_dt:=ymd_hms(pubDate_dt)]
		
		#Weekday
		art[, dow:=weekdays(pubDate_dt)]
		art[, wkdy:=ifelse(dow %in% c("Sunday", "Saturday"), "Weekend", "Weekend")]
		
		#Time of Day (Morning / Afternoon / Night)
		art[, timeofday:=substr(as.character(pubDate_dt),12,255)]
		art[, hr:=substr(timeofday,1,2)]

		art[, timeofday:=ifelse(as.numeric(hr)>=8 & as.numeric(hr)<12, "Morning",
						 ifelse(as.numeric(hr)>=12 & as.numeric(hr)<18, "Afternoon",
						 ifelse(as.numeric(hr)>=18 & as.numeric(hr)<24, "Evening", 
						 ifelse(as.numeric(hr)>=0 & as.numeric(hr)<8, "Late Night", NA))))
		]
		
	#3. General Topics - Condense the keywords into specific generalized topics
		#(Politics, Sports, International, Entertainment, etc...)
		out3[, topic:=ifelse(grepl("Politics|Trump|Republ|Democr|Government|Election|Presidential|Senate|House of Rep|Law and Legislation", new)==TRUE, "1. Politics", 
					  ifelse(grepl("Income|Infrastructure|Economic|Economy|Trade|Tariffs|Labor and Jobs|Wages and Salaries|Real Estate|Regulation", new)==TRUE, "2. Economy",
					  ifelse(grepl("State Legislatures|States (US)|Drug Abuse and Traffic|Florida|Federal Bureau of Investigation|Mueller, Robert S III|News and News Media|Firearms|Zuckerberg|Gun|Shooting|United States|Social Media|Education|Colleges and Universities|Discrimination|#MeToo|Blacks|Sexual|Ethnicity|Demonstration|Sex Crimes|Justice Department|Executive Changes|California|States (US)|Health Insurance|Murders|Ethics", new)==TRUE, "3. National",
					  ifelse(grepl("Palestinians|Terrorism|Espionage|International|Immigration|Russia|China|Iran|North Korea|Kim Jong-un|Great Britain|South Korea|Syria|Olympic|Israel|Vietnam|Canada", new)==TRUE, "4. International",
					  ifelse(grepl("Research|Global Warming|Science|Computer|Apple|PC|Data-Mining|Database|Facebook|Analy|Environmental", new)==TRUE, "5. Science and Technology",
					  ifelse(grepl("Actors and Actresses|Music|Fallon|Colbert|Television|Entertainment|Theater|Theatre|Movies|Art|Books", new)==TRUE, "5. Entertainment",
					  ifelse(grepl("Restaurants|Food|Cooking|Chef",new)==TRUE, "6. Food",
					  ifelse(grepl("Writing and Writers|Exercise|Women and Girls|Men and Boys|Weight|Nutrition|Fashion|Parenting|Vacations|Photography|Crossword|Children|Family", new)==TRUE, "7. Lifestyle",
					  ifelse(grepl("Manhattan|NYC|New York City|New York Times|New York State", new)==TRUE, "8. Local","9. Other")))))))))
					  ]
		out3[, topic:=min(topic, na.rm=TRUE), by=articleID]
		gen_topic<-out3[, list(topic=max(topic)), by=list(articleID)]
		
	#4. Keyword Rank
		kwr<-out3[, list(kwr=sum(N, na.rm=TRUE),
						 minkwr=min(keyword_rank, na.rm=TRUE)
						), by=articleID]

	#5. Specific Topics
		out3[, minkwr:=min(keyword_rank, na.rm=TRUE), by=articleID]
		spec_topic<-out3[minkwr==keyword_rank, .(specific=new, articleID)]
	
	#6. Article Sentiment
		art[, element_id:=1:nrow(art)]
		snippet<-get_sentences(art[, .(snippet)])
		sent1<-sentiment(snippet, neutral.nonverb.like = FALSE, missing_value=NULL)
		sent1<-as.data.table(sent1)
		sent1<-sent1[, list(snip_sent=sum(sentiment, na.rm=TRUE)), by=element_id]
		sent1[, snip_cat:=ifelse(snip_sent<= -0.50, "Very Negative",
						  ifelse(snip_sent> -0.50 & snip_sent<= -0.10, "Negative", 
						  ifelse(snip_sent> -0.10 & snip_sent< 0.10, "Neutral",
						  ifelse(snip_sent>= 0.10 & snip_sent< 0.50, "Positive", 
						  ifelse(snip_sent>= 0.50, "Very Positive", NA
						  )))))
		]
		
	#7. Match back together
		art<-as.data.table(left_join(art, keywords[, .(articleID, kw1=new1, kwr1=keyword_rank1, kw2=new2, kwr2=keyword_rank2, kw3=new3, kwr3=keyword_rank3)] , by=c("articleID"="articleID")))
		art<-as.data.table(left_join(art, gen_topic, by=c("articleID"="articleID")))
		art<-as.data.table(left_join(art, spec_topic, by=c("articleID"="articleID")))
		art<-as.data.table(left_join(art, kwr, by=c("articleID"="articleID")))
		art<-as.data.table(left_join(art, sent1, by=c("element_id"="element_id")))
								
		saveRDS(file="./art.rds", art)
			
#C. Comment Features
	#1. Number of Posters by Article
		num_post<-unique(com[, .(articleID, userID)], by=c("articleID", "userID"))
		num_post<-num_post[, .N, by=articleID]
		num_post<-num_post[, .(articleID, num_posts=N)]
		
	#2. Comment Order 
		com<-com[order(articleID, commentSequence)]
		com[, com_ord:=1]
		com[, com_ord:=cumsum(com_ord), by=articleID]
		
		com[, com_pos_cat:=ifelse(com_ord<=10, "Top 10", 
						   ifelse(com_ord>10 & com_ord<=50, "11 to 50", 
						   ifelse(com_ord>50 & com_ord<=100, "51 to 100", 
						   ifelse(com_ord>100 & com_ord<=200, "101 to 200",
						   ifelse(com_ord>200, ">200", NA)))))
		]
		
	
	#3. Time - Number of seconds from 1970-01-01
		#Convert to ts
		com[, createDate_ts:=ymd_hms(as.character(as.POSIXct(createDate, origin="1970-01-01 00:00:00")))] #Convert to datetime
		com[, approveDate_ts:=ymd_hms(as.character(as.POSIXct(approveDate, origin="1970-01-01 00:00:00")))] #Convert to datetime
		
		#Add on Article Date
		art_dt<-art[, .(pubDate_dt, articleID)]
		com<-as.data.table(left_join(com, art_dt, by=c("articleID"="articleID")))
		
		com[, time_to_post:=as.numeric(round(difftime(approveDate_ts, pubDate_dt, units="mins"),0))] #Time in minutes to posting
		com[, time_to_post:=ifelse(time_to_post<0, 0, time_to_post)] #Assumption since we do not know why posts could occur before publication dates
																	 #Maybe timezone issues
		com[, time_to_post_cat:=ifelse(time_to_post>=0 & time_to_post<15, "Within 15 minutes",
								ifelse(time_to_post>=15 & time_to_post<30, "15 to 29 minutes",
								ifelse(time_to_post>=30 & time_to_post<45, "30 to 44 minutes",
								ifelse(time_to_post>=45 & time_to_post<=60, "45 minutes to 1 Hour", 
								ifelse(time_to_post>60 & time_to_post<=120, "1 to 2 Hours",
								ifelse(time_to_post>120 & time_to_post<=240,"2 to 4 Hours", 
								ifelse(time_to_post>240 & time_to_post<=720,"4 to 12 Hours", 
								ifelse(time_to_post>720, ">12 Hours", 
								ifelse(is.na(pubDate_dt)==TRUE, "No Article", NA
								)))))))))
		]
		
	#4. Positive / Negative comment (need text analysis)
		com[, element_id:=1:nrow(com)]
		commentBody<-get_sentences(com[, .(commentBody)])
		sent2<-sentiment(commentBody, neutral.nonverb.like = FALSE, missing_value=NULL)
		sent2<-as.data.table(sent2)
		sent2<-sent2[, list(com_sent=sum(sentiment, na.rm=TRUE)), by=element_id]
		
		sent2[, com_cat:=ifelse(com_sent<= -0.50, "Very Negative",
						  ifelse(com_sent> -0.50 & com_sent<= -0.10, "Negative", 
						  ifelse(com_sent> -0.10 & com_sent< 0.10, "Neutral",
						  ifelse(com_sent>= 0.10 & com_sent< 0.50, "Positive", 
						  ifelse(com_sent>= 0.50, "Very Positive", NA
						  )))))
		]		
	
	#5. Comment Length 
		com[, comment_length:=str_length(commentBody)]
	
	#6. Readability
		read<-as.data.frame(com[, .(commentBody)])
		read<-textstat_readability(read$commentBody, measure = c("Flesch.Kincaid", "Coleman.Liau.grade"))
		read<-as.data.table(read)
		read[, element_id:=1:nrow(read)]
		read[, readFR:=round(Flesch.Kincaid,0)]
		read[, readCL:=round(Coleman.Liau.grade,0)]
		read<-read[, .(element_id, readFR, readCL)]

	
	#7. Match it all back together
		com<-as.data.frame(com)
		com<-as.data.table(com)
		com[, element_id:=1:nrow(com)]
		com<-left_join(com, read, by=c("element_id"="element_id"))
		com<-left_join(com, sent2, by=c("element_id"="element_id"))
		com<-as.data.table(com)
		
		saveRDS(file="./com.rds", com)
	
#D. Add Article Features to Comment Features
	art<-readRDS("./art.rds")
	com<-readRDS("./com.rds")

	#1. Reduce Article Data
		art_red<-art[, .(type, articleID, byline, kw1, kw2, kw3, kwr1, kwr2, kwr3, timeofday, wkdy, dow,
						 topic, specific, kwr, minkwr, snip_sent, snip_cat)]

	#2. Match Article Features to Comments
		com_final<-left_join(com, art_red, by=c("articleID"="articleID", "type"="type"))
		com_final<-as.data.table(com_final)
	
#IV. Data Output ------------------------------------------------------------------------------
#A. Split the data into test and train sets
	com_trn<-com_final[type=="trn", ]
	com_tst<-com_final[type=="tst", ]
	com_tst[, recommendations:=NULL]  #Removing this columns as it is NA for all values and this is what we are predicting

	stopifnot(nrow(com_trn)==nrow(trn_com)) #trn_com is the original
	stopifnot(nrow(com_tst)==nrow(tst_com)) #tst_com is the original
	
#B. Split the Train set into Train / Test sets (original test set is for submission)
	set.seed(19790324)
	samp<-as.data.table(sample_n(com_trn, round(nrow(com_trn)*0.70,0)))
	samp<-as.data.table(samp[, .(commentID)])
	samp[, samp:="trn"]
	
	trn_trn<-as.data.table(left_join(com_trn, samp, by=c("commentID"="commentID")))
	trn_trn[is.na(samp)==TRUE, samp:="tst"]
	trn_tst<-trn_trn[samp=="tst"][, samp:=NULL]
	trn_trn<-trn_trn[samp=="trn"][, samp:=NULL]

	stopifnot(round(nrow(trn_trn)/(nrow(trn_trn)+nrow(trn_tst)),2)==0.70)
	stopifnot(round(nrow(trn_tst)/(nrow(trn_trn)+nrow(trn_tst)),2)==0.30)
	
#C. Save objects
	saveRDS(file="./tst_submission.rds", com_tst)  #Final submission set
	saveRDS(file="./trn.rds", trn_trn)			   #Training set
	saveRDS(file="./tst.rds", trn_tst)			   #Initial validation of the Training set to use before submission
	

