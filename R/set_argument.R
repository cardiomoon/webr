# require(editData)
# result=editData(sampleData2)
# sampleData3<-result
# sampleData3
#
#
#data2pdf(sampleData3)
# data2HTML(sampleData3,vanilla=FALSE)
#data2HTML(sampleData3)
# data2docx(sampleData3)
# data2docx(sampleData3,vanilla=TRUE)


#' set argument of a function
#' @param code string of function call
#' @param argument argument of function to be set
#' @param value value to be set
#' @export
#' @importFrom stringr str_detect str_flatten
#' @importFrom purrr map_chr
#' @examples
#' code="df2flextable( ) "
#' code="df2flextable(vanilla=TRUE,head(iris[1:10,]))"
#' code="df2flextable(mtcars)"
#' code="df2flextable(head(iris[1:10,]),vanilla=TRUE)"
#' set_argument(code,"vanilla",FALSE)
set_argument=function(code,argument,value=TRUE){
    argument=as.character(substitute(argument))
    if(str_detect(code,argument)){
        temp<-unlist(strsplit(code,"\\("))
        for(i in seq_along(temp)){
            temp1=unlist(strsplit(temp[i],","))
            temp1=map_chr(temp1,replace_argument,argument,value)
            temp[i]=str_flatten(temp1,",")
        }
        result=str_flatten(temp,"(")
    } else{
        result=insert_argument(code,argument,value)
    }
    result
}

#' replace argument of a function
#' @param substring string of function call
#' @param argument argument of function to be set
#' @param value value to be set
#' @importFrom stringr str_replace
replace_argument=function(substring,argument,value){
    if(str_detect(substring,argument)){
        substring=str_replace(substring,"[[:alnum:]=]+",paste0(argument,"=",value))
    }
    substring
}

#' replace argument of a function
#' @param code string of function call
#' @param argument argument of function to be set
#' @param value value to be set
#' @importFrom stringr str_replace_all
insert_argument=function(code,argument,value){
    code=str_replace_all(code," ","")
    code=substr(code,1,nchar(code)-1)
    paste0(code,ifelse(substr(code,nchar(code),nchar(code))=="(","",","),
           argument,"=",value,")")
}


