#' My chisquare test
#' @param x a table
#' @importFrom stats chisq.test fisher.test
mychisq.test=function(x){
    result=tryCatch(chisq.test(x),warning=function(w) return("warnings present"))
    if(class(result)!="htest"){
        result1=tryCatch(fisher.test(x),
                         error=function(e) return("error present"),
                         warning=function(w) return("warning present"),
                         message = function(c) "message")
        if(class(result1)=="htest") result<-result1
    }
    result
}

#' Extract labels
#' @param x a vector
extractLabels=function(x){
    result=NULL
    if(!is.null(names(attr(x,"labels")))) result=names(attr(x,"labels"))
    if(!is.null(names(attr(x,"value.labels")))) result=names(attr(x,"value.labels"))
    #if(is.null(result)) result=as.character(unique(x))
    result
}

#' Extract x2 statistical result
#' @param x a table
#' @importFrom vcd assocstats
x2result=function(x){
    warning=0
    (result=mychisq.test(x))
    (result1=chisq.test(x))
    if(class(result)!="htest") {
        result=result1
        warning=1
    }
    cramer=vcd::assocstats(x)$cramer
    statresult=paste0("Chi-squared=",round(result1$statistic,3),", df=",result1$parameter)
    statresult=paste0(statresult,", Cramer\'s V=",round(cramer,3))
    if(substr(result$method,1,6)=="Fisher") {
        statresult=paste0(statresult,", Fisher's p")
    } else {
        statresult=paste0(statresult,", Chi-squared p")
    }
    pvalue=ifelse(result$p.value>=0.001,paste0("=",round(result$p.value,4)),"< 0.001")
    statresult=paste0(statresult,pvalue)
    if(warning==1) statresult=paste0(statresult,"; approximation may be incorrect")
    statresult
}

#' Summarize chisquare result
#' @param data A data.frame
#' @param x  a column name
#' @param y a column name
#' @param a a vector
#' @param b a vector
#' @param margin numeric If 1 row percent, if 2 col percent
#' @param show.percent logical
#' @param show.label logical
#' @export
#' @examples
#' require(moonBook)
#' x2summary(acs,sex,DM)
x2summary=function(data=NULL,x=NULL,y=NULL,a,b,margin=1,show.percent=TRUE,show.label=TRUE){
    # data=acs;x=sex;y=DM
    # margin=1;show.percent=TRUE;show.label=TRUE
    if(!is.null(data)){
        x=substitute(x)
        y=as.character(substitute(y))

        a=data[[x]]
        b=data[[y]]
    }
    x=table(a,b)

    # str(x)
    # (labela=attr(a,"label"))
    # (labelb=attr(b,"label"))
    labela=sjlabelled::get_label(a)
    labelb=sjlabelled::get_label(b)
    ncolb=ncol(x)




    x=addmargins(x,margin)
    x
    x1=scales::percent(round(t(apply(x,margin,function(a) a/sum(a))),3))

    if(margin==2) x1=matrix(x1,nrow=nrow(x),byrow=TRUE)

    if(show.percent) {
        x3=paste0(x,"\n(",x1,")")
    } else{
        x3<-x
    }

    if(margin==1){
        x2=matrix(x3,ncol=ncol(x))
        rowcol=rowSums(x)
        if(show.percent) rowcol=paste0(rowcol,"\n(100 %)")
        x2=cbind(x2,Total=rowcol)
    } else{
        x2=matrix(x3,nrow=nrow(x))
        colrow=colSums(x)
        if(show.percent) colrow=paste0(colrow,"\n(100 %)")
        x2=rbind(x2,Total=colrow)
    }

    x2
    x
    colnames(x)
    labels=extractLabels(b)
    labels

    if(!is.null(labels)) {

        colnames(x)
        #str(labels)
        if(length(colnames(x))==length(labels)+1) {
            colnames(x)=c(labels,"total")
        } else{
            if(margin==2){
                for(i in 1:(length(colnames(x))-1)){
                    colnames(x)[i]=labels[as.numeric(colnames(x)[i])]
                }
            } else{
                for(i in 1:(length(colnames(x)))){
                    colnames(x)[i]=labels[as.numeric(colnames(x)[i])]
                }

            }
        }

    }
    if(is.null(labels)) {
        if(margin==1) {
            colnames(x2)=c(colnames(x),"Total")
        } else{
            colnames(x2)=c(colnames(x)[1:(ncol(x)-1)],"Total")
        }

    } else if(ncol(x2)==length(labels)+1){
        colnames(x2)=c(labels,"Total")
    } else{
        if(margin==1) {
            colnames(x2)=c(colnames(x),"Total")
        } else{
            colnames(x2)=c(colnames(x)[1:(ncol(x)-1)],"Total")
        }
    }
    x2
    labels=extractLabels(a)

    rownames(x2)
    nrow(x2)
    rownames(x)
    if(!is.null(labels)) {

        colnames(x)
        #str(labels)
        if(length(rownames(x))==length(labels)+1) {
            rownames(x)=c(labels,"total")
        } else{
            if(margin==1){
                for(i in 1:(length(rownames(x))-1)){
                    rownames(x)[i]=labels[as.numeric(rownames(x)[i])]
                }
            } else{
                for(i in 1:(length(colnames(x)))){
                    rownames(x)[i]=labels[as.numeric(rownames(x)[i])]
                }

            }
        }

    }
    if(is.null(labels)) {
        if(margin==2){
            rownames(x2)=c(rownames(x),"Total")
        } else{
            rownames(x2)=c(rownames(x)[1:(nrow(x)-1)],"Total")
        }

    } else if(nrow(x2)==length(labels)+1){
        rownames(x2)=c(labels,"Total")
    } else{
        if(margin==2) {
            rownames(x2)=c(rownames(x),"Total")
        } else{
            rownames(x2)=c(rownames(x)[1:(nrow(x)-1)],"Total")
        }
    }
    # else {
    #         rownames(x2)=c(temp,"Total")
    # }
    x2
    as.data.frame(x2)
}


#' Make a chisquare result table
#' @param data A data.frame
#' @param x  a column name
#' @param y a column name
#' @param margin numeric If 1 row percent, if 2 col percent
#' @param show.percent logical
#' @param show.label logical
#' @param show.stat logical
#' @param vanilla logical whether or not make vanilla table
#' @param fontsize A numeric
#' @param ... Further arguments to be passed to df2flextable()
#' @importFrom rrtable df2flextable
#' @importFrom flextable align add_footer merge_at italic autofit
#' @export
#' @examples
#' require(moonBook)
#' x2Table(acs,sex,DM)
x2Table=function(data,x,y,margin=1,show.percent=TRUE,show.label=TRUE,
                 show.stat=TRUE,vanilla=FALSE,fontsize=12,...){
    # data=acs;x=sex;y=DM;margin=1;show.percent=TRUE;show.label=TRUE;
    # show.stat=TRUE;vanilla=FALSE;fontsize=12

    x=substitute(x)
    y=as.character(substitute(y))

    a=data[[x]]
    b=data[[y]]

    x2=x2summary(a=a,b=b,margin=margin,show.percent=show.percent,show.label=show.label)

    MyTable=rrtable::df2flextable(x2,add.rownames = TRUE,vanilla=vanilla,fontsize=fontsize,digits=0,...) %>%
         flextable::align(j=1,align='center',part='body') %>%
         flextable::align(i=1,align='center',part='header')

    if(show.stat){
    x=table(a,b)
    statresult=x2result(x)
    color=ifelse(vanilla,"white","#5B7778")
       MyTable<- MyTable %>%
           flextable::add_footer(rowname=statresult) %>%
           flextable::merge_at(j=1:(ncol(x2)+1),part="footer") %>%
           flextable::italic(j=1:(ncol(x2)+1),part="footer") %>%
           flextable::align(align='right',part='footer') %>%
           flextable::color(color=color,i=1,j=1,part='header') %>%
           flextable::autofit()

    }
    MyTable$header$dataset[1]<-y
    MyTable

}

