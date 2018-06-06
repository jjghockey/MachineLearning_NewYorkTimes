require(stringr)

text_to_columns <- function(data= NA, column = NA, delimiters = NA, trim = TRUE,
                                put_at_end = FALSE, keep_orig = TRUE, new_name_conv="new"){
  
  
  #eventually we convert column argument to numeric representation of location
  #we'll want to store the name if it's provided to keep output naming consistent
  cname <- column
  
  #First, set up the regex for delimiters:
  rules <- c(".","^","$","|","?","+",":","*","(",")")
  if(any(delimiters %in% rules)){
    specials_id <- which(delimiters %in% rules)
    specials <- paste0("\\",delimiters[specials_id])
    delimiters <- paste0(c(specials,delimiters[-specials_id]),collapse = "|") #the bars are interpreted "or" for regex
  }else{
    delimiters <- paste0(delimiters,collapse = "|") #the bars are interpreted "or" for regex
  }
  
  #It gets mad if you try more than one column at a time
  if(length(column)>1){
    stop("use on one column at a time")
  }
      
  #simplest case: fed a vector
  if(length(dim(data)) != 2 || dim(data)[2]==1 || is.vector(data)){
    #overwrite necessary defaults:
    put_at_end = TRUE
    
    if(length(dim(data))==2){
      data <- as.vector(data[,1])
    }
    #split along delimiters
    splits <- strsplit(data, split = delimiters)
    
    
    if(trim==TRUE){
      splits = lapply(splits,FUN = trimws)
    }
    
    
    #finds maximum number of "new columns" given the various splits in x
    split_lengths <- unlist(lapply(splits,FUN = length))
    new_col_count <- max(split_lengths)
    
    
    #now, not all elements are guaranteed to have the same number of splits, so we need a fix:
    #which elements do not comply:
    short_splits <- which(split_lengths < new_col_count)
    #for non-compliers, add NA as end elements
    #NOTE! THIS DOES NOT PUT THEM IN THE PROPER COLUMN IF AN EARLY DELIMITER IS ABSENT
    # THIS IS THE SAME FUNCTIONALITY AS IN EXCEL
    for(i in short_splits){
      splits[[i]] <- c(splits[[i]], rep(NA, new_col_count - length(splits[[i]]) ) )
    }
    
    #make splits a matrix:
    splits <- matrix(unlist(splits),nrow=length(split_lengths),ncol=new_col_count, byrow = TRUE)
    
    #take the max "new columns" and initialize NA columns as storage for inserts
    #blank_cols <- matrix(NA,ncol=new_col_count, nrow=length(data))
    
    #make a new dataset w/ or w/o originals
    if(keep_orig == TRUE){
      data <- as.data.frame(cbind(data, splits),stringsAsFactors=FALSE)
      column = 1
      if(is.null(names(data))|is.na(cname)){
        new_names <- c("original",paste0(new_name_conv,1:new_col_count))
      }else{
        new_names <- c(cname, paste0(new_name_conv, 1:new_col_count))
      }
    }else{
      data <- as.data.frame(splits,stringsAsFactors=FALSE)
      column = 0
      new_names <- c(paste0(new_name_conv,1:new_col_count))
    }
    
    names(data) <- new_names
    
    ###################
    ###################
    ###################
    
    #More complicated: fed a matrix (we can add columns NEXT TO, or just tack on at far right)
  }else{
    
    #overwrite necessary defaults:
    if(is.character(column)){
      column = which(names(data) == column)
    }
    
    #split along delimiters
    splits <- strsplit(data[,column], split = delimiters)
    
    if(trim==TRUE){
      splits<-lapply(splits,FUN = trimws)
    }
    
    #finds maximum number of "new columns" given the various splits in x
    split_lengths <- unlist(lapply(splits,FUN = length))
    new_col_count <- max(split_lengths)
    
    #now, not all elements are guaranteed to have the same number of splits, so we need a fix:
    #which elements do not comply:
    short_splits <- which(split_lengths < new_col_count)
    #for non-compliers, add NA as end elements
    #NOTE! THIS DOES NOT PUT THEM IN THE PROPER COLUMN IF AN EARLY DELIMITER IS ABSENT
    # THIS IS THE SAME FUNCTIONALITY AS IN EXCEL
    for(i in short_splits){
      splits[[i]] <- c(splits[[i]], rep(NA, new_col_count - length(splits[[i]])))
    }
    
    #take the max "new columns" and initialize NA columns as storage for inserts
    #blank_cols <- matrix(NA,ncol=new_col_count, nrow=nrow(data))
    
    #make splits a matrix to add in
    splits <- matrix(unlist(splits),nrow=nrow(data),ncol=new_col_count, byrow = TRUE)
    
    data_names <- names(data)
    
    #to keep track of how many columns we have
    end <- ncol(data)
    
    #make a new dataset w/ or w/o originals
    if(keep_orig == TRUE){
      #can make columns to the right of specified column, or tack on to far right of data:
      
      #put on far right:
      if(put_at_end== TRUE | column==ncol(data)){
        data <- as.data.frame(cbind(data, splits), stringsAsFactors=F)
        names(data) <- c(data_names[1:end], paste0(new_name_conv,1:new_col_count))
      }else{ #stick in next to original
        data <- as.data.frame(cbind(data[,1:column], splits, data[,(column+1):ncol(data)] ),stringsAsFactors = F)
        names(data) <- c(data_names[1:column], paste0(new_name_conv,1:new_col_count), data_names[(column+1):length(data_names)])
      }
      
    }else{ #keep_orig = FALSE
      #removes original text, and just puts split-text in place
      if(put_at_end==FALSE){
        #need to check for being in the final column (avoid over-indexing)
        if(column==1){
          data <- as.data.frame(cbind(splits, data[,-column]), stringsAsFactors = F)
          names(data)<- c(paste0(new_name_conv, 1:new_col_count), data_names[-column])
        }
        if(column<end & column > 1){
          data <- as.data.frame(cbind(data[,1:(column-1)], splits, data[,(column+1):ncol(data)] ),stringsAsFactors = F)
          names(data) <- c(data_names[1:(column-1)], paste0(new_name_conv,1:new_col_count), data_names[(column+1):length(data_names)])
        }
        if(column == end & end != 1){
          data <- as.data.frame(cbind(data[,1:(column-1)], splits),stringsAsFactors = F)
          names(data) <- c(data_names[1:end], paste0(new_name_conv,1:new_col_count))
        }
        
      }else{#put at end and remove original:
        #needs to check if provided data is nx1
        if(dim(data)[2]==1){
          data <- as.data.frame(matrix(unlist(splits),nrow=nrow(data),ncol=new_col_count, byrow = TRUE), stringsAsFactors=FALSE)
          names(data) <- paste0(new_name_conv, 1:new_col_count)
        }else{
          data <- as.data.frame(cbind(data[,-column], splits), stringsAsFactors = FALSE)
          names(data) <- c(data_names[-column], paste0(new_name_conv, 1:new_col_count))
        }
        
      }
      
    }
    
  }
  
  data
}