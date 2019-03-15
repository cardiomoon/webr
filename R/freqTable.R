#' Make table summarizing frequency
#' @param x A vector
#' @param digits integer indicating the number of decimal places
#' @param lang Language. choices are one of c("en","kor")
#' @export
#' @importFrom sjlabelled get_labels
#' @importFrom stats addmargins
#' @examples
#' require(moonBook)
#' freqSummary(acs$Dx)
#' freqSummary(acs$smoking,lang="kor")
freqSummary=function(x,digits=1,lang="en"){
        if(sum(is.na(x))==0){
                # (x=to_label(x))
                (res=table(x))
                 (labels=get_labels(x,attr.only=TRUE))

                if(!is.null(labels)) {

                        #str(labels)
                        if(length(rownames(res))==length(labels)) {
                                if(!all(rownames(res) %in% labels)){
                                   rownames(res)=labels
                                }
                        } else{
                                for(i in 1:length(rownames(res))){
                                        rownames(res)[i]=labels[as.numeric(rownames(res)[i])]
                                }
                        }

                }
                res
                (res1=prop.table(res)*100)
                (result=cbind(res,res1,res1))
                (result=addmargins(result,1))
                res2=cumsum(res1)
                res2=c(res2,NA)
                (result=cbind(result,res2))

                result2=result[,1]
                for(i in (2:4)) {
                        format=paste0("%0.",digits,"f")
                        temp=sprintf(format,result[,i])
                        result2=cbind(result2,temp)
                }
                result2[result2=="NA"]=""

        } else {
                res=table(x)
                res
                # if(!is.null(names(attr(x,"labels")))) rownames(res)=names(attr(x,"labels"))
                # if(!is.null(names(attr(x,"value.labels")))) rownames(res)=names(attr(x,"value.labels"))
                (labels=get_labels(x,attr.only=TRUE))
                if(!is.null(labels)) {

                        rownames(res)
                        #str(labels)
                        if(length(rownames(res))==length(labels)) {
                                rownames(res)=labels
                        } else{
                                for(i in 1:length(rownames(res))){
                                        rownames(res)[i]=labels[as.numeric(rownames(res)[i])]
                                }
                        }

                }
                res
                res1=c(res,sum(is.na(x)))
                res1
                names(res1)[length(res1)]=langchoice1(1,lang)

                res2=prop.table(res1)*100
                result=cbind(res1,res2)
                result=addmargins(result,1)

                res3=prop.table(table(x))*100
                res3=c(res3,NA,sum(res3))
                res4=cumsum(res3)
                result=cbind(result,res3,res4)
                result2=result[,1]
                for(i in (2:4)) {
                        format=paste0("%0.",digits,"f")
                        temp=sprintf(format,result[,i])
                        result2=cbind(result2,temp)
                }
                result2[result2=="NA"]=""
                rownames(result2)[nrow(result2)-1]<-langchoice1(1,lang)
                result2
        }

        colnames(result2)=langchoice1(2:5,lang=lang)
        rownames(result2)[nrow(result2)]=langchoice1(6,lang=lang)
        result2

}


#' Make flextable summarizing frequency
#' @param x A vector
#' @param digits integer indicating the number of decimal places
#' @param lang Language. choices are one of c("en","kor")
#' @param vanilla Logical. Whether make vanilla table or not
#' @param ... Further arguments to paseed to the df2flextable function
#' @return An object of clss flextable
#' @export
#' @importFrom rrtable df2flextable
#' @importFrom flextable color autofit
#' @importFrom magrittr '%>%'
#' @examples
#' require(moonBook)
#' freqTable(acs$Dx)
#' freqTable(acs$smoking,lang="kor",vanilla=TRUE,fontsize=12)
freqTable=function(x,digits=1,lang=getOption("freqTable.lang","en"),vanilla=FALSE,...){

    res=freqSummary(x,digits=digits,lang=lang)
    tempname=colnames(res)
    res=data.frame(res,stringsAsFactors=FALSE)
    colnames(res)=tempname

    result<-rrtable::df2flextable(res,add.rownames=TRUE,vanilla=vanilla,...) %>% autofit()

    if(vanilla) {
        result<- result %>% color(i=1,j=1,color="white",part="header")
    } else {
        result<- result %>% color(i=1,j=1,color="#007FA6",part="header")
    }
    result
}


