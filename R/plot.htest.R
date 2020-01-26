require(ggplot2)

#' Plotting distribution of statistic for object "htest"
#'
#' @param x object of class "htest"
#' @param ... further arguments to ggplot
#' @importFrom ggplot2 ggplot geom_point geom_line theme_bw annotate aes aes_string geom_area labs theme element_text
#' @importFrom stringr str_pad str_c
#' @export
#' @return a ggplot or NULL
#'
#' @examples
#'
#' require(moonBook)
#' require(webr)
#' ## chi-square test
#' x=chisq.test(table(mtcars$am,mtcars$cyl))
#' plot(x)
#'
#' #Welch Two Sample t-test
#' x=t.test(mpg~am,data=mtcars)
#' plot(x)
#'\donttest{
#' x=t.test(BMI~sex,data=acs)
#' plot(x)
#'
#' # F test to compare two variances
#' x=var.test(age~sex,data=acs,alternative="less")
#' plot(x)
#'
#' # Paired t-test
#' x=t.test(iris$Sepal.Length,iris$Sepal.Width,paired=TRUE)
#' plot(x)
#'
#' # One sample t-test
#' plot(t.test(acs$age,mu=63))
#'
#' # Two sample t-test
#' x=t.test(age~sex, data=acs,conf.level=0.99,alternative="greater",var.equal=TRUE)
#' plot(x)
#' }
plot.htest=function(x,...){

     tests=c("Welch Two Sample t-test","Pearson's Chi-squared test"," Two Sample t-test","Pearson's Chi-squared test with Yates' continuity correction",
             "One Sample t-test","F test to compare two variances","Paired t-test")
     if(!(x$method %in% tests)) {
          cat("Currently, ",x$method," is not supported")
          return(invisible(0))
     }
     (degf=x[[2]])
     statName=tolower(attr(x$statistic,"names"))
     statName
     if(statName=="x-squared") {
          statName<-"chisq"
          alternative<-"greater"
          alpha<-0.05
     } else{
          if(statName=="w") {
               statName<-"wilcox"
               alpha=0.05
          } else{
               alpha=1-attr(x$conf.int,"conf.level")
          }
          alternative<-x$alternative


     }
     (newalpha=alpha)
     if(alternative=="two.sided") newalpha=alpha/2

     qF=function(...){
          eval(parse(text=paste0("q",statName,"(...)")))
     }
     dF=function(...){

               eval(parse(text=paste0("d",statName,"(...)")))
     }

     if(statName=="chisq"){
          x0 <- seq(0,qF(p=0.999,df=degf),length=100)
          y0=dF(x0,df=degf)
          x1=seq(qF(p=1-newalpha,df=degf),qF(p=0.999,df=degf),length=50)
          y1=dF(x1,df=degf)
     } else if(statName %in% c("f","wilcox")){
          x0 <- seq(qF(p=0.0001,degf[1],degf[2]),qF(p=0.9999,degf[1],degf[2]),length=100)

          y0=dF(x0,degf[1],degf[2])
          x2=seq(qF(p=0.0001,degf[1],degf[2]),qF(p=newalpha,degf[1],degf[2]),length=50)
          y2=dF(x2,degf[1],degf[2])
          x1=seq(qF(p=1-newalpha,degf[1],degf[2]),qF(p=0.9999,degf[1],degf[2]),length=50)
          y1=dF(x1,degf[1],degf[2])
     } else{
          x0 <- seq(-4,4,length=100)
          if(x[[1]]>4) {
               x0=c(x0,x[[1]])
          } else if(x[[1]] < -4) {
               x0=c(x[[1]],x0)
          }
          x0
          y0=dF(x0,df=degf)
          y0
          x1=seq(qF(p=1-newalpha,df=degf),4,length=50)
          y1=dF(x1,df=degf)
          x2=seq(-4,qF(p=newalpha,df=degf),length=50)
          y2=dF(x2,df=degf)
     }

     data=data.frame(x=x0,y=y0)
     data
     data1=data.frame(x=x1,y1=y1)
     if(statName!="chisq") data2=data.frame(x=x2,y1=y2)
     x
     label=paste0(sprintf("%9s",attr(x$statistic,"names"))," = ",
                   sprintf("%.03f",x$statistic))
     if(length(degf)==2) {
          label=c(label,paste0("num df=",degf[1],", denom df=",degf[2]))
     } else {
          label=c(label,paste0(sprintf("%9s","df")," = ",sprintf("%.2f",degf)))
     }

     if(x[[3]]>=0.00001) {
          label=c(label,paste0(sprintf("%9s","p")," = ",sprintf("%.5f",x[[3]])))
     } else {
          label=c(label,paste0(sprintf("%9s","p")," < 0.00001"))
     }
     label=stringr::str_pad(label,19,side="right")

     label=stringr::str_c(label,collapse="\n")
     label
     p2<-ggplot(data,aes_string(x="x",y="y"))+geom_line()+theme_bw()
     if(alternative!="less")  p2<-p2+geom_area(data=data1,aes(x1,y1),fill="red",alpha=0.5)
     if(alternative!="greater")  p2<-p2+ geom_area(data=data2,aes(x2,y2),fill="red",alpha=0.5)
     p2
     if(abs(x$statistic)>4) {
          hjust=1
     } else if(x$statistic>0) {
          hjust=-0.1
     } else hjust=0.1

     if(statName %in% c("f","wilcox")) {
          ypoint=dF(x$statistic,degf[1],degf[2])
          xpoint=qF(p=1-newalpha,degf[1],degf[2])
          xpoint2=qF(p=newalpha,degf[1],degf[2])
     } else {
          ypoint=dF(x$statistic,df=degf)
          ypoint
          xpoint=qF(p=1-newalpha,df=degf)
          xpoint2=qF(p=newalpha,df=degf)
     }
     p2<-p2+geom_point(x=x[[1]],y=ypoint,color="blue")
     p2<-p2+ annotate(geom="label",x=Inf,y=Inf,label=label,vjust=1.1,hjust=1.1)
         # geom_text(x=xpoint2,y=0.38,label=label)+
         #
     p2 <-p2+ annotate(geom="text",x=ifelse(alternative=="less",xpoint2,xpoint),y=0,
                   label=paste0("p < ",alpha),vjust=1.5,color="red")
     p2<-p2+labs(title=x$method,x=paste0(statName," statistic"),y="Probability Density")+theme(plot.title=element_text(hjust=0.5))
     sub=makeSub(x)
     if(sub!="") p2<-p2+labs(subtitle=sub)
     p2
}




#' Make subtitle
#'
#' @param x An object of class "htest"
#'
makeSub=function(x){
     sub=""
     if (!is.null(x$alternative)) {
          sub="alternative hypothesis: "
          if (!is.null(x$null.value)) {
               if (length(x$null.value) == 1L) {
                    alt.char <- switch(x$alternative, two.sided = "not equal to",
                                       less = "less than", greater = "greater than")
                    sub=paste(sub,"true ", names(x$null.value), " is ", alt.char,
                              " ", x$null.value, sep = "")
               }
               else {
                    sub=paste(sub,x$alternative, "\nnull values:\n", sep = "")
                    sub=paste0(sub,x$null.value)
               }
          }

     }
     sub
}
