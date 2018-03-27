#' Add a flextable or mytable object into a document object
#' @param mydoc A document object
#' @param ftable A flextable or mytable object
#' @param title An character string as a plot title
#' @importFrom officer add_slide ph_with_text  body_add_par
#' @importFrom flextable body_add_flextable ph_with_flextable
#' @return a document object
#' @export
#' @examples
#' require(webr)
#' require(moonBook)
#' require(officer)
#' require(magrittr)
#' ftable=mytable(Dx~.,data=acs)
#' title="mytable Example"
#' ft=df2flextable(head(iris))
#' title2="df2flextable Example"
#' doc=read_docx()
#' doc %>% add_flextable(ftable,title) %>%
#'         add_flextable(ft,title2) %>%
#'         print(target="mytable.docx")
#' read_pptx() %>%
#'        add_flextable(ftable,title) %>%
#'        add_flextable(ft,title2) %>%
#'        print(target="mytable.pptx")
add_flextable=function(mydoc,ftable,title=""){
     if("mytable" %in% class(ftable)){
          ft<-mytable2flextable(ftable)
     } else {
          ft<-ftable
     }
     if(class(mydoc)=="rpptx"){
          mydoc <- mydoc %>% add_slide("Title and Content",master="Office Theme")
          mydoc <- mydoc %>% ph_with_text(type="title",str=title)
          mydoc<-mydoc %>% ph_with_flextable(type="body",value=ft)

     } else {
          mydoc <- mydoc %>% body_add_par(value=title,style="heading 1")
          mydoc<-mydoc %>% body_add_par(value="",style="Normal")
          mydoc<-mydoc %>% body_add_flextable(ft)
     }
     mydoc
}


#
# library(rvg)
# read_pptx() %>%
#      add_slide(layout = "Title and Content", master = "Office Theme") %>%
#      ph_with_vg(code = print(gg), type = "body") %>%
#      add_slide(layout = "Title and Content", master = "Office Theme") %>%
#      ph_with_vg(code = plot(1:10,1:10,type="b"), type = "body") %>%
#      print(target = "demo_rvg.pptx")


#' Add plot into a document object
#' @param mydoc A document object
#' @param plotstring String of an R code encoding a plot
#' @param title An character string as a plot title
#' @param vector A logical. If TRUE, vector graphics are produced instead, PNG images if FALSE.
#' @return a document object
#' @importFrom officer ph_with_text
#' @export
#' @examples
#' require(webr)
#' require(officer)
#' require(rvg)
#' require(magrittr)
#' read_pptx() %>% add_plot("plot(iris)")
#' read_docx() %>% add_plot("plot(iris)")
add_plot=function(mydoc,plotstring,title="",vector=TRUE){
     # if(title!=""){
     #      mydoc=addSlide(mydoc,"Title and Content")
     #      mydoc=addTitle(mydoc,title)
     # } else{
     #      mydoc=addSlide(mydoc,"Content")
     # }
     # mydoc=addPlot(mydoc,function() {plotfunction},vector.graphic=vector)
     # mydoc
     if(class(mydoc)=="rpptx"){
          temp=paste0("ph_with_vg(mydoc,code=",plotstring,",type = \"body\")")
          mydoc<- mydoc %>%
               add_slide(layout = "Title and Content", master = "Office Theme") %>%
               ph_with_text(type="title",str=title)
          mydoc=eval(parse(text=temp))
     } else{
          temp=paste0("body_add_vg(mydoc,code=",plotstring,")")
          mydoc <- mydoc %>%
               body_add_par(value=title,style="heading 1")
          mydoc=eval(parse(text=temp))
     }
     mydoc
}


#' Add ggplot into a document object
#' @param mydoc A document object
#' @param gg An R code encoding a ggplot
#' @param title An character string as a plot title
#' @return a document object
#' @importFrom rvg ph_with_vg body_add_vg
#' @export
#' @examples
#' require(webr)
#' require(ggplot2)
#' require(officer)
#' require(magrittr)
#' gg <- ggplot(mtcars, aes(x = mpg , y = wt, colour = factor(am))) + geom_point()
#' read_pptx() %>% add_ggplot(gg)
#' read_docx() %>% add_ggplot(gg)
add_ggplot=function(mydoc,gg,title=""){
     if(class(mydoc)=="rpptx"){
     mydoc<- mydoc %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with_vg(code = print(gg), type = "body") %>%
          ph_with_text(type="title",str=title)
     } else{
          mydoc <- mydoc %>%
               body_add_par(value=title,style="heading 1") %>%
               body_add_vg(code=print(gg))
     }
     mydoc
}

#' Add plot into a document object
#' @param mydoc A document object
#' @param plotstring An string of R code encoding plot
#' @param title An character string as a plot title
#' @param width the width of the device.
#' @param height the height of the device.
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels.
#' @param format plot format
#' @param ... additional arguments passed to png()
#' @return a document object
#' @importFrom devEMF emf
#' @importFrom officer ph_with_img body_add_img
#' @export
#' @examples
#' require(officer)
#' require(webr)
#' require(magrittr)
#' read_pptx() %>% add_img("plot(mtcars)",format="png",res=300)
#' read_docx() %>% add_img("plot(mtcars)",format="png",res=300)
add_img=function(mydoc,plotstring,title="",width=7,height=5,units="in",
                 res=300,format="emf",...){
     # produce an emf file containing the ggplot
     filename <- tempfile(fileext = paste0(".",format))
     if(format=="emf"){
     emf(file = filename, width = width, height = height)
     } else if(format %in% c("png","PNG")){
          png(filename = filename, width = width, height = height,units=units,res=res,...)
     }
     eval(parse(text=plotstring))
     dev.off()
     if(class(mydoc)=="rpptx"){
          mydoc<- mydoc %>%
               add_slide(layout = "Title and Content", master = "Office Theme") %>%
               ph_with_img(src=filename, type = "body",width=width,height=height) %>%
               ph_with_text(type="title",str=title)
     } else{
          mydoc <- mydoc %>%
               body_add_par(value=title,style="heading 1") %>%
               body_add_img(src=filename,
                              width=width,height=height)
     }
     mydoc
}

# read_docx() %>% add_img(plot(mtcars),title="plot(mtcars)",format="png",res=300) %>%
#      print(target="png.docx")



####################


#' Make a data.frame with character strings encoding R code
#' @param result character strings encoding R code
#' @param preprocessing character strings encoding R code as a preprocessing
#' @importFrom utils capture.output
Rcode2df=function(result,preprocessing){
     if(preprocessing!="") eval(parse(text=preprocessing))
     res=c()
     codes=unlist(strsplit(result,"\n",fixed=TRUE))
     for(i in 1:length(codes)){
          #if(codes[i]=="") next
          if(length(grep("cat",codes[i]))==1) {
               if(grep("cat",codes[i])==1) next
          }
          res=c(res,codes[i])
          temp=capture.output(eval(parse(text=codes[i])))
          if(length(temp)==0) temp1=""
          else  {
               temp1=Reduce(pastelf,temp)
               temp1=paste0(temp1,"\n ")
          }
          res=c(res,temp1)

     }
     data.frame(result=res,stringsAsFactors = FALSE)

}

pastelf=function(...){
     paste(...,sep="\n")
}

#' Split strings with desired length with exdent
#' @param string String
#' @param size desired length
#' @param exdent exdent
#' @importFrom stringr str_extract_all str_flatten
#' @return splitted character vector
tensiSplit <- function(string,size=100,exdent=3) {
    result=c()
    if(nchar(string)<=size) {
        result=string
    } else{
        temp=substr(string,1,size)
        result=unlist(str_extract_all(substr(string,size+1,nchar(string)), paste0('.{1,',size-exdent,'}')))
        result=paste0(str_flatten(rep(" ",exdent)),result)
        result=c(temp,result)
    }
    result
}


#' Make a FlexTable with a data.frame
#' @param df A data.frame
#' @param bordercolor A border color name
#' @param format desired format. choices are "pptx" or "docx"
#' @importFrom flextable delete_part flextable height_all void
#' @importFrom stringr str_split str_wrap
#' @return A FlexTable object
df2RcodeTable=function(df,bordercolor="gray",format="pptx"){
    # df
    #bordercolor="gray";maxlen=80
    maxlen=ifelse(format=="pptx",100,90)
    no<-code<-c()
     for(i in 1:nrow(df)){
          temp=df[i,]
          result=unlist(strsplit(temp,"\n",fixed=TRUE))
          if(length(result)>0){
               for(j in 1:length(result)){

                    splitedResult=tensiSplit(result[j],size=maxlen)
                    code=c(code,splitedResult)
                    no=c(no,rep(i,length(splitedResult)))
               }
          }
     }
     df2=data.frame(no,code,stringsAsFactors = FALSE)
     flextable(df2) %>%
          align(align="left",part="all") %>% border_remove() %>%
          bg(i=~no%%2==1,bg="#EFEFEF") %>%
          padding(padding=0) %>%
          #padding(i=~no%%2==0,padding.left=10) %>%
          font(fontname="Monaco",part="all") %>%
          delete_part(part="header") %>%
          void(j=1) %>%
          autofit() %>% height_all(height=0.2,part="all")
}

#' Make a flextable object with character strings encoding R code
#' @param result character strings encoding R code
#' @param preprocessing character strings encoding R code as a preprocessing
#' @param format desired format. choices are "pptx" or "docx"
#' @export
Rcode2flextable=function(result,preprocessing="",format="pptx"){
     df=Rcode2df(result,preprocessing=preprocessing)
     df2RcodeTable(df,format=format)

}



#' Make a R code slide into a document object
#' @param mydoc A document object
#' @param code  A character string encoding R codes
#' @param title An character string as a plot title
#' @param preprocessing A character string of R code as a preprocessing
#' @param format desired format. choices are "pptx" or "docx"
#' @return a document object
#' @export
#' @examples
#' #library(webr)
#' #library(magrittr)
#' #library(officer)
#' #code="fit=lm(mpg~hp+wt,data=mtcars)\nsummary(fit)"
#' #read_pptx() %>% add_Rcode(code,title="Regression Analysis") %>% print(target="Rcode.pptx")
add_Rcode=function(mydoc,code,title="",preprocessing="",format="pptx"){

     ft <- Rcode2flextable(code,preprocessing=preprocessing,format=format)
     mydoc <- mydoc %>% add_flextable(ft,title=title)
     mydoc
}


#' Addtitle slide
#' @param mydoc A document object
#' @param title An character string as a title
#' @param subtitle An character string as a subtitle
#' @export
#' @examples
#' #read_pptx() %>% add_title(title="Web-based analysis with R" %>% print(target="title.pptx")
add_title=function(mydoc,title="",subtitle=""){
     mydoc <- mydoc %>%
          add_slide(layout="Title Slide",master="Office Theme") %>%
          ph_with_text(type="ctrTitle",str=title) %>%
          ph_with_text(type="subTitle",str=subtitle)
     mydoc
}


#' convert data to pptx file
#' @param data A document object
#' @param title An character string as a title
#' @param preprocessing A string
#' @param filename File name
#' @param format desired format. choices are "pptx" or "docx"
#' @param width the width of the device.
#' @param height the height of the device.
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels.
#' @param rawDataName raw Data Name
#' @param rawDataFile raw Data File
#' @importFrom officer read_docx read_pptx
#' @export
data2office=function(data,title="Web-based Meta-Analysis",
                   preprocessing="",
                   filename="Report",format="pptx",width=7,height=5,units="in",
                   res=300,rawDataName=NULL,rawDataFile="rawData.RDS"){

     if(!is.null(rawDataName)){
          rawData=readRDS(rawDataFile)
          assign(rawDataName,rawData)
     }

     if(preprocessing!="") eval(parse(text=preprocessing))
     if(format=="pptx"){
          mydoc <- read_pptx() %>%
                   add_title(title=title,subtitle="prepared by web-R.org")
     } else {
          mydoc <- read_docx()
     }
     #str(data)
     for(i in 1:nrow(data)){
          #cat("data$code[",i,"]=",data$code[i],"\n")

          if(data$type[i]=="Rcode") eval(parse(text=data$code[i]))
          if(data$type[i]=="data"){
              ft=df2flextable(eval(parse(text=data$code[i])))
              mydoc=add_flextable(mydoc,ft,data$title[i])
          } else if(data$type[i]=="table"){
               ft=eval(parse(text=data$code[i]))
               mydoc=add_flextable(mydoc,ft,data$title[i])
          } else if(data$type[i]=="mytable"){
               res=eval(parse(text=data$code[i]))
               ft=mytable2flextable(res)
               mydoc=add_flextable(mydoc,ft,data$title[i])
          } else if(data$type[i]=="ggplot"){
               p<-eval(parse(text=data$code[i]))
               mydoc=add_ggplot(mydoc,p,title=data$title[i])
          } else if(data$type[i]=="plot"){
               mydoc<-add_plot(mydoc,data$code[i],title=data$title[i])

          } else if(data$type[i]=="Rcode"){

               mydoc=add_Rcode(mydoc,code=data$code[i],title=data$title[i],
                               preprocessing=preprocessing,format=format)

          } else if(data$type[i] %in% c("PNG","png")){

               mydoc<-add_img(mydoc,data$code[i],title=data$title[i],format="png")

          } else if(data$type[i] %in% c("emf","EMF")){

               mydoc<-add_img(mydoc,data$code[i],title=data$title[i])

          }


     }
     if(length(grep(".",filename,fixed=TRUE))>0) {
         target=filename
     } else{
        target=paste0(filename,".",format)
     }
    #cat("target=",target,"\n")
     mydoc %>% print(target=target)
}

#' convert data to pptx file
#' @param ... arguments to be passed to data2office()
#' @export
#' @examples
#' #library(webr)
#' #data2pptx(sampleData2)
data2pptx=function(...){
     data2office(...)
}

#' convert data to docx file
#' @param ... arguments to be passed to data2office()
#' @export
#' @examples
#' #library(webr)
#' #data2docx(sampleData2)
data2docx=function(...){
     data2office(...,format="docx")
}


#' Convert html5 code to latex
#' @param df A data.frame
html2latex=function(df){
    temp=colnames(df)
    temp=stringr::str_replace(temp,"<i>p</i>","\\\\textit{p}")
    temp=stringr::str_replace(temp,"&#946;","$\\\\beta$")
    temp=stringr::str_replace(temp,"Beta","$\\\\beta$")
    temp=stringr::str_replace(temp,"&#967;<sup>2</sup>","$\\\\chi^{2}$")
    colnames(df)=temp
    df
}



#
# data=webr::sampleData2
# data[2,3]="df2flextable(iris[1:10,])"
# data<-rbind(data,data[3,])
# data<-rbind(data,data[3,])
# data$type[6]="png"
# data$type[7]="emf"
# data
# library(magrittr)
# library(officer)
# library(flextable)
# library(moonBook)
# library(rvg)
# library(ggplot2)
#
# data2pptx(data[5:7,])
# data[6,]
# data2pptx(data)
# data1=data[3,]
# data1
# data=data1
# filename="Report.pptx"
# data2office(data)
# data2office(data,format="docx")
# data2pptx(data)
# data2docx(data)
# # data=data[3,]
# # data
#
# str(data)
#
# mydoc=read_pptx()
#
# for(i in 1:nrow(data)){
#      eval(parse(text=data$code[i]))
# mydoc=add_plot(mydoc,data$code[i],title=data$title[i])
# }
# mydoc %>% print(target="plot.pptx")
