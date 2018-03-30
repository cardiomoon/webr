#' Convert mytable object to flextable
#'
#' @param result An object of class "mytable"
#' @param vanilla A Logical.
#' @param fontname Font name
#' @param fontsize font size
#' @importFrom flextable bg merge_at merge_v
#' @importFrom moonBook mycsv
#' @importFrom utils read.csv
#' @export
#' @examples
#' require(moonBook)
#' require(flextable)
#' require(officer)
#' result=mytable(smoking+Dx~.,data=acs)
#' mytable2flextable(result)
#' mytable2flextable(result,vanilla=FALSE)
#' result=mytable(Dx~.,data=acs)
#' mytable2flextable(result)
#' mytable2flextable(result,vanilla=FALSE)
mytable2flextable=function(result,vanilla=TRUE,fontname=NULL,fontsize=12){

     mycsv(result,"test.csv",row.names = FALSE)
     test=read.csv("test.csv",colClasses = "character")
     file.remove("test.csv")

     if("cbind.mytable" %in% class(result)){
          (pcolumns=seq(from=(ncol(test)-1)%/%length(result)+1,to=ncol(test),(ncol(test)-1)/length(result)))

          for(i in 1:length(pcolumns)) test[[pcolumns[i]]][test[[pcolumns[i]]]=="0.000"]="< 0.001"

          tableNo=length(attr(result,"caption"))
          colPerTable=(length(test)-1)/tableNo
          select=c(1,seq(2,length(test),by=colPerTable))
          deselect=setdiff(1:ncol(test),select)
          df1=data.frame(
               col_keys=colnames(test),
               type=colnames(test),
               group=as.vector(as.matrix(test[1,])),
               n=as.vector(as.matrix(test[2,])),
               stringsAsFactors = FALSE
          )
          df1[deselect,2]=""
          TableName=names(test)[select]
          colspan=c(1,rep(colPerTable,tableNo))
          start=pcolumns-(colPerTable-1)
          test=test[-c(1:2),]
          big_border=fp_border(color="black",width=2)
          std_border=fp_border(color="black",width=1)

          ft<- flextable(test) %>%
               set_header_df(mapping=df1,key="col_keys") %>%
               border_remove() %>%
               hline_top(part="all",border=big_border) %>%
               hline_bottom(part="all",border=big_border) %>%
               hline(i=1,part="header",border=std_border) %>%
               merge_v(part="header")
          #ft <-ft %>% empty_blanks()

          rowno=c()
          current=1
          test[[ncol(test)]]
          for(i in 1:nrow(test)){

               if(test[[ncol(test)]][i]=="") {
                    rowno=c(rowno,current)
               } else{
                    rowno=c(rowno,!current)
                    current=!current
               }
          }
          change<-c()
          for(i in 1:(length(rowno)-1)){
               if(rowno[i]!=rowno[i+1]){
                    change=c(change,i)
               } else{
                    stop=c(stop,i)
               }
          }
          change=c(change,i+1)
          if(!vanilla){
               ft=bg(ft,rowno==1,j=1:ncol(test),bg="#EFEFEF")
          }
          start=pcolumns-(colPerTable-1)
          for(k in 1:length(start)){
               ft=merge_at(ft,i=1,j=start[k]:pcolumns[k],part="header")
          }
          if(length(change)>1) for(i in 1:(length(change)-1)){
               if(change[i+1]>change[i]+1){
                     for(j in 1:length(pcolumns)) {
                         ft=merge_at(ft,i=(change[i]+1):(change[i+1]),j=pcolumns[j])
                     }
               }
          }
     } else{

          df1=data.frame(
               col_keys=colnames(test),
               type=colnames(test),
               n=as.vector(as.matrix(test[1,])),
               stringsAsFactors = FALSE
          )
          df1[1,ncol(df1)]=df1[1,1]
          df1[nrow(df1),ncol(df1)]=df1[nrow(df1),1]
          test=test[-1,]
          #str(test)
          pcolumn=ncol(test)
          test[[pcolumn]][test[[pcolumn]]=="0.000"]="< 0.001"
          big_border=fp_border(color="black",width=2)
          ft=flextable(test) %>%
               set_header_df(mapping=df1,key="col_keys") %>%
               border_remove() %>%
               hline_top(part="all",border=big_border) %>%
               hline_bottom(part="all",border=big_border) %>%
               merge_v(part="header") %>%
               align(j=1,align="left",part="body") %>%
               align(align="center",part="header") %>%
               padding(padding.left=5,padding.right=5,
                       padding.top=2,padding.bottom=2,part="all") %>%
               autofit()

          rowno=c()
          current=1
          for(i in 1:nrow(test)){
               if(test[[ncol(test)]][i]=="") {
                    rowno=c(rowno,current)
               } else{
                    rowno=c(rowno,!current)
                    current=!current
               }
          }
          change<-c()
          for(i in 1:(length(rowno)-1)){
               if(rowno[i]!=rowno[i+1]){
                    change=c(change,i)
               } else{
                    stop=c(stop,i)
               }
          }
          change=c(change,i+1)
          if(!vanilla){
               ft=bg(ft,rowno==1,j=1:ncol(test),bg="#EFEFEF")
          }
          if(length(change)>1) for(i in 1:(length(change)-1)){
               if(change[i+1]>change[i]+1){
                    ft=merge_at(ft,i=change[i]+1,j=2:(ncol(test)-1))
                    ft=merge_at(ft,j=ncol(test),i=(change[i]+1):(change[i+1]))
               }
          }
     }
     if(!is.null(fontname)) ft<-ft %>% font(fontname=fontname,part="all")
     ft <- ft %>%
          align(j=1,align="left",part="body") %>%
          align(align="center",part="header") %>%
          fontsize(size=fontsize+1,part="header") %>%
          fontsize(size=fontsize,part="body") %>%
          padding(padding.left=5,padding.right=5,
                          padding.top=2,padding.bottom=2,part="all") %>%
          autofit()
     ft

}

