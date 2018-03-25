#' Convert numeric columns of data.frame to character
#' @param df A data.frame
#' @param digits integer indicating the number of decimal places
#' @export
#' @examples
#' roundDf(iris,digits=c(1,2,3,4))
#' roundDf(mtcars,digits=2)
roundDf=function(df,digits=2){
     if(length(digits)==1){
          digits<-rep(digits,ncol(df))
     }
     else if(length(digits)!=ncol(df)) {
          digits<-c(digits,rep(0,ncol(df)-length(digits)))
     }
     df[]<-lapply(1:ncol(df),function(i){
          if(is.integer(df[[i]])) {
               df[[i]]<-df[[i]]
          } else if(is.numeric(df[[i]])) {
               fmt=paste0("%0.",sprintf("%d",digits[i]),"f")
               df[[i]]=sprintf(fmt,df[[i]])
          } else{
               df[[i]]<-df[[i]]
          }

     })
     df
}






#' Convert data.frame to flextable
#'
#' @param df A data.frame
#' @param vanilla A Logical
#' @param fontname Font name
#' @param fontsize font size
#' @param even_header background color of even_header
#' @param odd_header background color of even_header
#' @param even_body background color of even_body
#' @param odd_body background color of even_body
#' @param vlines Logical. Whether or not draw vertical lines
#' @param colorheader Logical. Whether or not use color in header
#' @param digits integer indicating the number of decimal places
#' @param align_header alignment of header. Expected value is one of 'left', 'right', 'center', 'justify'.
#' @param align_body alignment of body. Expected value is one of 'left', 'right', 'center', 'justify'.
#' @param ... further argumants to be passed to flextable
#' @importFrom flextable regulartable set_formatter_type set_header_df theme_zebra vline vline_left align autofit padding hline hline_top hline_bottom border_remove font fontsize color
#' @importFrom officer fp_border
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' require(flextable)
#' df2flextable(head(iris),vanilla=TRUE,colorheader=TRUE)
#' df2flextable(head(iris),vanilla=TRUE,digits=c(1,2,3,4))
#' df2flextable(head(iris),vanilla=FALSE)
#' df2flextable(head(iris),vanilla=FALSE,vlines=FALSE,fontsize=14)
#' df2flextable(head(mtcars),vanilla=FALSE)
df2flextable=function(df,vanilla=FALSE,fontname=NULL,fontsize=12,
                      even_header="transparent",odd_header="#5B7778",
                      even_body="#EFEFEF",odd_body="transparent",
                      vlines=TRUE,colorheader=FALSE,digits=2,
                      align_header="center",align_body="right",...){

     df<-roundDf(df,digits)
     if(!colorheader){
          headercolor=ifelse(vanilla,"black","white")
     } else{
          headercolor=ifelse(vanilla,"#007FA6","white")
     }
     big_border=fp_border(color=headercolor,width=2)
     header_border=fp_border(color="black",width=1)
     std_color="#EDBD3E"
     if(vanilla) std_color="black"
     std_border=fp_border(color=std_color,width=1)

     fmt_double=paste0("%0.0",sprintf("%df",digits))

     ft <- regulartable(df,...) %>% set_formatter_type(fmt_double=fmt_double)
     odd_header=ifelse(vanilla,"transparent","#5B7778")
     if(!vanilla)
          ft<- ft %>% theme_zebra(even_body=even_body,odd_body=odd_body,
                                  odd_header = odd_header)
     ft <- ft %>% border_remove()
     if(!is.null(fontname)) ft<-ft %>% font(fontname=fontname,part="all")
     ft<-ft %>%
          fontsize(size=fontsize+1,part="header") %>%
          fontsize(size=fontsize,part="body") %>%
          color(color=headercolor,part="header") %>%
          color(color="black",part="body")

     if(vanilla){
          ft <- ft %>% hline_top(part="all",border=big_border) %>%
               hline_bottom(part="all",border=big_border)

     } else{
     ft <- ft %>% hline_top(part="all",border=header_border) %>%
          hline_bottom(part="all",border=std_border)
     }
     if(!vanilla){
          ft <- ft %>% hline(part="body",border=std_border)
     }
     if((!vanilla)&(vlines)){
          ft <-ft %>% vline(part="body",border=std_border) %>%
               vline_left(part="body",border=std_border) %>%
               vline(part="header",border=header_border) %>%
               vline_left(part="header",border=header_border)
     }
     ft <- ft %>% align(align=align_body,part="body") %>%
          align(align=align_header,part="header") %>%
          padding(padding.left=5,padding.right=5,
                          padding.top=2,padding.bottom=2,part="all") %>%
          autofit()
     ft
}
