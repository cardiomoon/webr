#' Renew dictionary
renew_dic=function(){
    #' Renew dictionary

    # dicTable=rio::import("./R/sysdata.rda")
    # result=editData::editData(dicTable)
    # result
    # dicTable<-result
    #
    # devtools::use_data(dicTable,internal=TRUE,overwrite=TRUE)
}


#' Select word
#' @param id data id
#' @param lang language. Possible choices are c("en","kor")
#' @export
langchoice1=function(id,lang="en"){
    temp=dicTable[as.numeric(dicTable$id) %in% id,]
    if(lang=="en"){
        result=temp$en
    } else{
        result=temp$kor
    }
    result
}

