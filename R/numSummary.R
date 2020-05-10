#' replace vector with labels
#' @param x A vector
#' @importFrom sjlabelled get_labels
#' @export
replaceWithLabels=function(x){
    (labels=get_labels(x,attr.only=TRUE))
    if(!is.null(labels)) {
        x=labels[x]
    }
    x
}

#' Numerical Summary
#' @param x A numeric vector or a data.frame or a grouped_df
#' @param digits integer indicating the number of decimal places
#' @param lang Language. choices are one of c("en","kor")
#' @param ... further arguments to be passed
#' @importFrom purrr map_dfc
#' @export
#' @examples
#' require(moonBook)
#' require(magrittr)
#' require(dplyr)
#' require(rrtable)
#' require(webr)
#' require(tibble)
#' numSummary(acs)
#' numSummary(acs$age)
#' numSummary(acs,age,EF)
#' acs %>% group_by(sex) %>% numSummary(age,BMI)
#' acs %>% group_by(sex) %>% select(age) %>% numSummary
#' acs %>% group_by(sex) %>% select(age,EF) %>% numSummary
#' acs %>% group_by(sex,Dx) %>% select(age,EF) %>% numSummary
#' acs %>% group_by(sex,Dx) %>% select(age) %>% numSummary
#' #acs %>% group_by(sex,Dx) %>% numSummary(age,EF,lang="kor")
numSummary <- function(x,...,digits=2,lang="en") {
     if("grouped_df" %in% class(x)) {

         result=numSummary2(x,...,digits=digits,lang=lang)
     } else{

         result=numSummary1(x,...,digits=digits,lang=lang)
     }
     map_dfc(result,replaceWithLabels)

}

#'@describeIn numSummary Numerical Summary of a data.frame or a vector
#'@importFrom psych describe
#'@importFrom tibble as_tibble
#'@importFrom dplyr enexprs
numSummary1 <- function(x,...,digits=2,lang="en"){

       if('data.frame' %in% class(x)) {
           vars=enexprs(...)
           if(length(vars)>0) x<-x %>% select(...)
           select=sapply(x,is.numeric)
           x=x[select]
           if(ncol(x)==1) x=x[[1]]
        }

        result=psych::describe(x)
        if('data.frame' %in% class(x)) {
            result$vars=rownames(result)
        } else{
            result$vars=NULL
        }
        if(digits!=2) result=print(result,digits=digits)

        if(lang=="kor"){
            if('data.frame' %in% class(x)) {
                colnames(result)=c(langchoice1(21,lang=lang),"n",langchoice1(7:17,lang=lang))
            } else{
                colnames(result)=c("n",langchoice1(7:17,lang=lang))
            }
        }
        as_tibble(result)
}


#' @describeIn numSummary Numerical Summary of a grouped_df
#' @importFrom rlang quos
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom dplyr mutate select
numSummary2 <- function(x,...,digits=2,lang="en") {
    temp="mutate(x,summary=map(data,numSummary1,...,digits=digits,lang=lang))"
    x<-x %>% nest()
    eval(parse(text=temp)) %>%
        select(-c('data')) %>%
        unnest(cols=c(summary))
}


#' Make a table showing numerical summary
#' @param x A grouped_df or a data.frame or a vector
#' @param ... further argument to be passed
#' @param lang Language. choices are one of c("en","kor")
#' @param vanilla Logical. Whether make vanilla table or not
#' @param add.rownames Logical. Whether or not add rownames
#' @export
#' @examples
#' require(moonBook)
#' require(dplyr)
#' numSummaryTable(acs)
#' numSummaryTable(acs$age)
#' acs %>% group_by(sex) %>% select(age) %>% numSummaryTable
#' acs %>% group_by(sex) %>% select(age,EF) %>% numSummaryTable
#' acs %>% group_by(sex,Dx) %>% select(age,EF) %>% numSummaryTable(vanilla=FALSE)
#' acs %>% group_by(sex,Dx) %>% numSummaryTable(age,EF,add.rownames=FALSE)
numSummaryTable <- function(x,...,lang=getOption("numSummaryTable.lang","en"),vanilla=FALSE,add.rownames=NULL){

    result=numSummary(x,lang=lang,...)
    if(is.null(add.rownames)){
        add.rownames=FALSE
        if("data.frame" %in% class(x)) add.rownames=TRUE
        if("tibble" %in% class(x)) add.rownames=TRUE
        if("grouped_df" %in% class(x)) add.rownames=FALSE
    }
    df2flextable(result,add.rownames=add.rownames,vanilla=vanilla)
}
