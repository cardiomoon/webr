#' Extract categorical variables
#' @param df a data.frame
#' @param max.ylev maximal length of unique values of catergorical variables
#' @export
GroupVar=function(df,max.ylev=20){
    result=c()
    for(i in 1:ncol(df)){
       if(length(unique(df[[i]]))<=max.ylev) result=c(result,colnames(df)[i])      
   }    
   result
}

#' Extract continuous variables
#' @param df a data.frame
#' @export
ContinuousVar=function(df){
    result=c()
    for(i in 1:ncol(df)){
        if(is.numeric(df[[i]])) result=c(result,colnames(df)[i])      
    }    
    result
}

#' Extract bivariate variables
#' @param df a data.frame
#' @export
BiVar=function(df){
    result=c()
    for(i in 1:ncol(df)){
        if(length(unique(df[[i]]))==2) result=c(result,colnames(df)[i])      
    }    
    result
}
