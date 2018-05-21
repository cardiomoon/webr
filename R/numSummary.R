#' Numerical Summary
#' @param x A numeric vector or a data.frame or a grouped_df
#' @param digits integer indicating the number of decimal places
#' @param lang Language. choices are one of c("en","kor")
#' @param ... further arguments to be passed
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
#' acs %>% group_by(sex) %>% select(age) %>% numSummary
#' acs %>% group_by(sex) %>% select(age,EF) %>% numSummary
#' acs %>% group_by(sex,Dx) %>% select(age,EF) %>% numSummary
#' acs %>% group_by(sex,Dx) %>% select(age) %>% numSummary
#' acs %>% group_by(sex,Dx) %>% numSummary(age,EF,lang="kor")
numSummary <- function(x,...,digits=2,lang="en") {
     if("grouped_df" %in% class(x)) {

         numSummary2(x,...,digits=digits,lang=lang)
     } else{

         numSummary1(x,...,digits=digits,lang=lang)
     }

}

#'@describeIn numSummary Numerical Summary of a data.frame or a vector
#'@importFrom tibble as.tibble
numSummary1 <- function(x,...,digits=2,lang="en"){


       if('data.frame' %in% class(x)) {
           select=sapply(x,is.numeric)
           x=x[select]
           if(ncol(x)==1) x=x[[1]]
        }
        result=psych::describe(x,...)
        result$vars=NULL
        if(digits!=2) result=print(result,digits=digits)

        if(lang=="kor"){
            colnames(result)=c("n",langchoice1(7:17,lang=lang))
        }
        as.tibble(result)
}

# #'@describeIn numSummary Numerical Summary of a grouped_df
# #'@importFrom psych describeBy print.psych
# numSummary2 <- function(x,digits=2,lang="en",...){
#
#
#     vars=attr(x,"vars")
#     x1=x[-(1:length(vars))]
#     if(ncol(x1)==1) x1=x1[[1]]
#
#     select=sapply(x1,is.numeric)
#     x1=x1[select]
#     result=psych::describeBy(x1,x[vars],mat=TRUE,...)
#
#     if(digits!=2){
#
#         result=print(result,digits=digits)
#
#     }
#     if(lang=="kor"){
#         group=paste0(langchoice1(19,lang=lang),1:length(vars))
#         group
#         colnames(result)=c(langchoice1(20,lang=lang),group,
#                            langchoice1(21,lang=lang),"n",langchoice1(7:17,lang=lang))
#     }
#     as.tibble(result)
# }

#' @describeIn numSummary Numerical Summary of a grouped_df
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
numSummary2 <- function(x,...,digits=2,lang="en") {
    vars <- tidyselect::vars_select(names(x), !!! rlang::quos(...))

    numSummary_group_impl(x, vars,digits=digits,lang=lang)
}
#
# x<-acs %>% group_by(sex)
# numSummary2(x,age,EF)

#' @importFrom tibble is.tibble tibble
#' @importFrom purrr map_df
#' @importFrom tidyr unnest
numSummary_group_impl <- function(df, vars,digits=digits,lang=lang ) {

    if (length(vars) == 0) vars <- names(df)

    if (length(vars) == 1 & !is.tibble(df)) df <- as.tibble(df)

    idx_numeric <- sapply(df[, vars], is.numeric)

    num_summary <- function(x, .data, vars) {

        result <- numSummary(.data[x + 1, vars],digits=digits,lang=lang )

        as.tibble(result)

    }

    call_summary <- function(vars) {
        statistic <- purrr::map_df(attr(df, "indices"),
                                   num_summary, df, vars)

        dplyr::bind_cols(tibble(variable = rep(vars, nrow(statistic))),
                         as.tibble(attr(df, "labels")), statistic)
    }

    statistic <- lapply(vars[idx_numeric], function(x) call_summary(x))

    result <- tibble(statistic) %>%
        tidyr::unnest()

    result
}


#' Make a table showing numerical summary
#' @param x A grouped_df or a data.frame or a vector
#' @param ... further argument to be passed
#' @export
#' @examples
#' require(moonBook)
#' require(magrittr)
#' require(dplyr)
#' require(rrtable)
#' require(webr)
#' numSummaryTable(acs)
#' numSummaryTable(acs$age)
#' acs %>% group_by(sex) %>% select(age) %>% numSummaryTable
#' acs %>% group_by(sex) %>% select(age,EF) %>% numSummaryTable
#' acs %>% group_by(sex,Dx) %>% select(age,EF) %>% numSummaryTable
#' acs %>% group_by(sex,Dx) %>% numSummaryTable(age,EF,lang="kor")
numSummaryTable <- function(x,...){

    result=numSummary(x,...)
    df2flextable(result,add.rownames=TRUE)
}
