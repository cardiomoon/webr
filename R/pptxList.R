#' Sample data for PPTxList
#' A dataset containing five objects for reproducible research
#'
#' @format A data frame with 5 rows and three columns
#' \describe{
#'    \item{type}{type of data}
#'    \item{title}{title of data}
#'    \item{code}{R code of data}
#' }
"sampleData2"


#'A dataset containing demographic data and laboratory data of 857 pateints with
#'acute coronary syndrome(ACS).
#'
#'
#'@format A data frame with 857 rows and 17 variables:
#'\describe{
#'  \item{age}{patient age in years}
#'  \item{sex}{"Male" or "Female"}
#'  \item{cardiogenicShock}{"No" or "Yes"}
#'  \item{entry}{vascular access route, either "Femoral" or "Radial"}
#'  \item{Dx}{Final diagnosis, One of the followings : STEMI, NSTEMI or Unstable Angina}
#'  \item{EF}{ejection fraction, percentage by echocardiography}
#'  \item{height}{height in centimeter}
#'  \item{weight}{weight in kilogram}
#'  \item{BMI}{body mass index in kg/m2}
#'  \item{obesity}{obesity, "No" or "Yes"}
#'  \item{TC}{total cholesterol level in mg/dL}
#'  \item{LDLC}{low density lipoprotein cholesterol level in mg/dL}
#'  \item{HDLC}{high density lipoprotein cholesterol level in mg/dL}
#'  \item{TG}{triglyceride level in mg/dL}
#'  \item{DM}{history of diabetes mellitus,"No" or "Yes"}
#'  \item{HBP}{history of hypertension,"No" or "Yes"}
#'  \item{smoking}{history of smoking, One of the followings : "Never","Ex-smoker","Smoker"}
#'}
"acs"


#' Server function of pptxList shiny module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param data A data object
#' @param preprocessing A character string of R code
#' @importFrom shiny reactiveValues updateTextAreaInput reactive h4 fileInput actionButton downloadButton hr p callModule
#' @importFrom shiny downloadHandler tagList conditionalPanel uiOutput observe observeEvent column fluidRow renderUI
#' @importFrom shiny htmlOutput imageOutput plotOutput renderImage renderPlot renderPrint tableOutput updateCheckboxInput
#' @importFrom ReporteRs renderFlexTable
#' @importFrom editData numericInput3 radioButtons3 editableDT editableDTUI selectInput3
#' @importFrom readr read_csv
#' @export
pptxList<-function(input,output,session,data=reactive(""),preprocessing=reactive(""))
{

     savedPPT=reactiveValues()

     observe({
          if(data()!=""){
               df<-data()
               savedPPT$type=df$type
               savedPPT$title=df$title
               savedPPT$code=df$code
          } else{
               savedPPT$type=c()
               savedPPT$title=c()
               savedPPT$code=c()
          }
     })
     observe({
          if(preprocessing()!=""){
               updateTextAreaInput(session,"preprocessing",value=preprocessing())
          }
     })

     pptdf=reactive({

          input$pptfile
          input$resetPPT

          df<-data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,
                         stringsAsFactors = FALSE)
          df
     })

     observeEvent(input$pptfile,{
          if(!is.null(input$pptfile)){

               mypptlist<-readr::read_csv(input$pptfile$datapath,comment="#")
               savedPPT$type=mypptlist$type
               savedPPT$title=mypptlist$title
               savedPPT$code=mypptlist$code
               result=readComment(input$pptfile$datapath)
               if(result!=""){
                    updateTextAreaInput(session,"preprocessing",value=result)
               }
          }
     })

     mydf=reactive({

          data()
     })
     # output$text1=renderPrint({
     #         pptdf2()
     # })
     output$pptListUI=renderUI({
          ns <- session$ns
          pptList=pptdf()
          count=nrow(pptList)
          tagList(

               h4("Saved PowerPoint List"),
               if(count>0) editableDTUI(ns("PPTxListTable")),
               hr(),
               fluidRow(
                    column(4,
                           h4("Upload PPTList(*.csv)"),
                           fileInput(ns("pptfile"),NA)  #"Upload PPTList(*.csv)"
                    ),
                    column(3,
                           h4("Load sampleData "),
                           actionButton(ns("loadSample"),"load sampleData")
                    ),
                    column(5,
                           h4("Reset PPT List "),
                           actionButton(ns("ResetPPT"),"reset PPT List")
                    )
               ),


               if(count>0) downloadButton(ns("savePPTxList"),"download as csv"),
               if(count>0) downloadButton(ns("downloadPPTxHTML"),"download as HTML"),
               if(count>0) downloadButton(ns("downloadPPTxPDF"),"download as PDF"),
               if(count>0) downloadButton(ns("downloadPPTxList"),"download as PPTx"),
               if(count>0) downloadButton(ns("downloadPPTxListWord"),"download as Word"),
               if(count>0) hr(),
               if(count>0) downloadButton(ns("downloadPlots"),"download Plots"),
               if(count>0) numericInput3(ns("plotRes"),"Resolution",value=300,step=1,width=80),
               if(count>0) selectInput3(ns("plotUnit"),"units",choices=c("in","cm","mm"),selected="in",width=70),
               if(count>0) numericInput3(ns("plotWidth"),"plotWidth",value=7,step=0.5,width=70),
               if(count>0) numericInput3(ns("plotHeight"),"plotHeight",value=5,step=0.5,width=70),
               if(count>0) radioButtons3(ns('plotformat'), 'Format As', c('PNG', 'SVG','PDF'),
                                         inline = TRUE,selected='PNG'),

               if(count==0) p("There is no saved data."),
               if(count>0) hr(),
               if(count>0) actionButton(ns("showPPTList"),"show/hide saved List"),
               if(count>0) conditionalPanel("true==false",
                                            checkboxInput(ns("showList"),"showList",value=FALSE)),
               if(count>0) conditionalPanel(sprintf("input['%s']==true",ns("showList")),
                                            hr(),
                                            uiOutput(ns("PPTListUI2")))

          )
     })

     observeEvent(input$showPPTList,{
         updateCheckboxInput(session,"showList",value=!input$showList)
     })

     output$PPTListUI2=renderUI({

         input$showPPTList

         ns<-session$ns

         count= length(savedPPT$title)

         mydf=data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,stringsAsFactors = FALSE)

         for(i in 1:count){
             local({
                 j<-i
                 outputname=paste0("output",j*2-1)
                 output[[outputname]]=renderPrint({
                     h4(mydf$title[j])
                 })
             })
         }
         for(i in 1:count){
             local({
                 j<-i
                 outputname=paste0("output",j*2)
                 if(savedPPT$type[j]=="table"){
                     output[[outputname]]=renderFlexTable({
                         mytable=eval(parse(text=mydf$code[j]))
                         mytable
                     })
                 } else if(savedPPT$type[j]=="mytable"){
                     output[[outputname]]=renderFlexTable({
                         res=eval(parse(text=mydf$code[j]))
                         MyFTable=mytable2FTable(res,vanilla=TRUE)
                         MyFTable
                     })
                 } else if(savedPPT$type[j]=="ggplot"){
                     output[[outputname]]=renderPlot({
                         p<-eval(parse(text=mydf$code[j]))
                         p
                     })

                 } else if(savedPPT$type[j]=="plot"){
                     output[[outputname]]=renderPlot({
                         p<-eval(parse(text=mydf$code[j]))
                         p
                     })

                 } else if(savedPPT$type[j]=="Rcode"){
                     output[[outputname]]=renderFlexTable({
                         result=Rcode2FlexTable(mydf$code[j])
                         result

                     })

                 } else if(savedPPT$type[j]=="PNG"){
                     output[[outputname]]=renderImage({
                         myfunction<-eval(parse(text=mydf$code[j]))
                         png("temp.png",width=input$plotWidth,height=input$plotHeight,units=input$plotUnit,
                             res=input$plotRes,type="cairo")
                         myfunction
                         dev.off()
                         list(src = "temp.png",
                              contentType = 'image/png',
                              width = 400,
                              height = 300,
                              alt = "This is alternate text")
                     }, deleteFile = TRUE)
                 }
             })

         }
         output_list <- lapply(1:count, function(j) {

             outputname=paste0("output",j*2)
             if(mydf$type[j] %in% c("table","mytable","Rcode")) tableOutput(ns(outputname))
             else if(mydf$type[j] %in% c("ggplot","plot")) plotOutput(ns(outputname))
             else if(mydf$type[j]=="PNG") imageOutput(ns(outputname))

         })
         output_list2 <- lapply(1:count, function(j) {

             outputname=paste0("output",j*2-1)
             htmlOutput(ns(outputname))

         })

         # Convert the list to a tagList - this is necessary for the list of items
         # to display properly.
         my_list=c(output_list,output_list2)
         for(i in 1:length(output_list)){
             my_list[[2*i-1]]=output_list2[[i]]
             my_list[[2*i]]=output_list[[i]]
         }

         do.call(tagList, my_list)


     })




     observeEvent(input$ResetPPT,{

          savedPPT$type=c()
          savedPPT$title=c()
          savedPPT$code=c()

          updateTextAreaInput(session,"preprocessing",value="")

     })

     observeEvent(input$loadSample,{

         savedPPT$type=webr::sampleData2$type
         savedPPT$title=webr::sampleData2$title
         savedPPT$code=webr::sampleData2$code

         updateTextAreaInput(session,"preprocessing",value="")

     })


     pptdf2=callModule(editableDT,"PPTxListTable",data=reactive(pptdf()))


     output$downloadPPTxHTML<- downloadHandler(
          filename = function() {
               paste('report', sep = '.','html')
          },

          content = function(file) {
               # src <- normalizePath('PPTxReport.Rmd')

               # temporarily switch to the temp dir, in case you do not have write
               # permission to the current working directory
               owd <- setwd(tempdir())
               on.exit(setwd(owd))

               data2HTML(pptdf2(),preprocessing=input$preprocessing,filename=file)
          }
     )

     output$downloadPPTxPDF<- downloadHandler(
          filename = function() {
               paste('report', sep = '.','pdf')
          },

          content = function(file) {

               # temporarily switch to the temp dir, in case you do not have write
               # permission to the current working directory
               owd <- setwd(tempdir())
               on.exit(setwd(owd))

               data2pdf(pptdf2(),preprocessing=input$preprocessing,filename=file)
          }
     )

     output$downloadPPTxList=downloadHandler(
          filename="report.pptx",
          content=function(file){

               src <- normalizePath('myppt.pptx')

               # temporarily switch to the temp dir, in case you do not have write
               # permission to the current working directory
               owd <- setwd(tempdir())
               on.exit(setwd(owd))
               file.copy(src, 'myppt.pptx')

               data=pptdf2()

               data2pptx(data,title="Web-based Meta-Analysis",
                         template="myppt.pptx",preprocessing=input$preprocessing,
                         filename=file,width=input$width,height=input$height,units=input$units,res=input$res)

          },
          contentType="application/vnd-ms-powerpoint"
     )



     output$downloadPPTxListWord=downloadHandler(
          filename="report.docx",
          content=function(file){

               owd <- setwd(tempdir())
               on.exit(setwd(owd))

               data=pptdf2()

               data2word(data,title="Web-based Meta-Analysis",
                         preprocessing=input$preprocessing,
                         filename=file,width=input$plotWidth,height=input$plotHeight,
                         units=input$plotUnit,res=input$plotRes)


          },
          contentType="application/vnd-ms-word"
     )



     output$savePPTxList = downloadHandler(
          filename="PPTxList.csv",
          content=function(file){


               owd <- setwd(tempdir())
               on.exit(setwd(owd))
               df<-pptdf2()
               writeCSVComment(df,file=file,metadata=input$preprocessing)
          },
          contentType="text/csv"
     )

     output$downloadPlots = downloadHandler(
          filename="Plot.zip",
          content=function(file){

               # temporarily switch to the temp dir, in case you do not have write
               # permission to the current working directory
               owd <- setwd(tempdir())
               on.exit(setwd(owd))

               data=pptdf2()

               data2plotzip(data,filename=file,format=input$plotformat,width=input$plotWidth,
                            height=input$plotHeight,units=input$plotUnit,res=input$plotRes,start=0,
                            preprocessing=input$preprocessing)
          },
          contentType="application/zip"
     )

     pptdf3<-reactive({
          result<-NULL
          result<-pptdf2()
          if(input$preprocessing!="") {
               attr(result,"preprocessing")=input$preprocessing
          }
          result
     })

     return(pptdf3)
}


#' Make zipped plots with a data.frame
#' @param data A data.frame
#' @param format Plot format. Choices are c("PNG","SVG","PDF")
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param start Plot start number
#' @param preprocessing A character string of R code
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
myplot2=function(data,format="PNG",width=7,height=7,units="in",res=300,start=0,preprocessing="",rawDataName=NULL,rawDataFile="rawData.RDS"){
     filename=c()
     count=nrow(data)


     if(!is.null(rawDataName)){
         rawData=readRDS(rawDataFile)
         assign(rawDataName,rawData)
     }

     if(preprocessing!=""){
         eval(parse(text=preprocessing))
     }
     j=1
     if(count>0) for(i in 1:count){
          #eval(parse(text=data$code[i]))
          if(data$type[i] %in% c("plot","ggplot")){
               path <- paste("plot_", j+start, ".",format, sep="")
               filename <- c(filename, path)
               if(format=="SVG"){
                    temp=paste0("plot",format,"2(function(){",data$code[[i]],"},path,width=width,height=height")
                    if(data$type[i]=="ggplot"){
                         temp=paste0(temp,",ggplot=TRUE")
                    }
                    temp=paste0(temp,")")
               } else{
                    temp=paste0("plot",format,"2(function(){",data$code[[i]],"},path,width=width,height=height,units=units,res=res")
                    if(data$type[i]=="ggplot"){
                         temp=paste0(temp,",ggplot=TRUE")
                    }
                    temp=paste0(temp,")")
               }
               eval(parse(text=temp))
               j=j+1
          } else{
               eval(parse(text=data$code[i]))
          }
     }
     filename
}

#' Make pdf file with a plot code
#' @param fun A R code for plot
#' @param file A path of destination file
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param ggplot A logical. Set this argument true if th R code is for ggplot
#' @importFrom grDevices cairo_pdf dev.off
plotPDF2=function(fun,file,width=7,height=5,units="in",res=300,ggplot=FALSE){

     if(ggplot){
          fun()
          ggsave(file,width=width,device=cairo_pdf,height=height,units=units,dpi=res)
     }
     else {
          cairo_pdf(file,width=width,height=height)
          #pdf(file,paper="letter")
          fun()
          dev.off()
     }

}

#' Make SVG file with a plot code
#' @param fun A R code for plot
#' @param file A path of destination file
#' @param width A plot width
#' @param height A plot height
#' @param ggplot A logical. Set this argument true if th R code is for ggplot
#' @importFrom grDevices svg
plotSVG2=function(fun,file,width=7,height=7,ggplot=FALSE){

     if(ggplot) ggsave(file,fun(),width=width,height=height)
     else {
          svg(file,width=width,height=height)
          #pdf(file,paper="letter")
          fun()
          dev.off()
     }

}

#' Make png file with a plot code
#' @param fun A R code for plot
#' @param file A path of destination file
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param ggplot A logical. Set this argument true if th R code is for ggplot
#' @importFrom grDevices png
#' @importFrom ggplot2 ggsave
plotPNG2=function(fun,file,width=7,height=7,units="in",res=300,ggplot=FALSE){

     if(ggplot) ggsave(file,fun(),width=width,height=height,units=units,dpi=res)
     else {
          png(file,width=width,height=height,units=units,res=res)
          #pdf(file,paper="letter")
          fun()
          dev.off()
     }

}


#' Make zipped plot file with a data.frame
#' @param data A data.frame
#' @param filename A path of destination file
#' @param format Plot format. Choices are c("PNG","SVG","PDF")
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param start Plot start number
#' @param preprocessing A character string of R code
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @importFrom utils zip
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' library(webr)
#' data2plotzip(sampleData2)
data2plotzip=function(data,filename="Plot.zip",format="PNG",width=8,height=6,units="in",res=300,start=0,preprocessing="",
                      rawDataName=NULL,rawDataFile="rawData.RDS"){

     fs=myplot2(data,format=format,width=width,height=height,units=units,res=res,start=start,preprocessing=preprocessing,
                rawDataName=rawDataName,rawDataFile=rawDataFile)
     zip(zipfile=filename, files=fs)
}

#' Make a word file with a data.frame
#' @param data A data.frame
#' @param title The title of a word file
#' @param preprocessing A character string of R code
#' @param filename A path of destination file
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @importFrom ReporteRs addFlexTable addPlot addImage docx
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' library(webr)
#' data2word(sampleData2)
data2word=function(data,title,preprocessing="",filename="Report.docx",
                   width=7,height=5,units="in",
                   res=300,rawDataName=NULL,rawDataFile="rawData.RDS"){

     mydoc=docx(title="Web-based Meta-Analysis")

     if(!is.null(rawDataName)){
         rawData=readRDS(rawDataFile)
         assign(rawDataName,rawData)
     }


     if(preprocessing!="") eval(parse(text=preprocessing))


     for(i in 1:nrow(data)){
          eval(parse(text=data$code[i]))
          data$title[i]
          addTitle(mydoc,data$title[i])
          if(data$type[i]=="table"){
               mytable=eval(parse(text=data$code[i]))
               mydoc=addFlexTable(mydoc,mytable)

          } else if(data$type[i]=="mytable"){
               res=eval(parse(text=data$code[i]))
               MyFTable=mytable2FTable(res,vanilla=TRUE)
               mydoc=addFlexTable(mydoc,MyFTable)
          } else if(data$type[i]=="ggplot"){
               p<-eval(parse(text=data$code[i]))
               mydoc=addPlot(mydoc,fun=print,x=p,vector.graphic=TRUE)
          } else if(data$type[i]=="plot"){

               temp=paste0("addPlot(mydoc,function() ",data$code[i],",vector.graphic=TRUE)")
               mydoc=eval(parse(text=temp))
          } else if(data$type[i]=="Rcode"){

               result=Rcode2FlexTable(data$code[i],preprocessing=preprocessing)
               mydoc = addFlexTable(mydoc,result)

          } else if(data$type[i]=="PNG"){

               png(filename="temp.png",width=width,height=height,units=units,
                   res=res,type="cairo")
               eval(parse(text=data$code[i]))
               dev.off()
               mydoc=addImage(mydoc,"temp.png",width=6,height=4)
          }
     }
     writeDoc(mydoc,file=filename)
}

#' Make a data.frame with character strings encoding R code
#' @param result character strings encoding R code
#' @param preprocessing character strings encoding R code as a preprocessing
#' @importFrom utils capture.output
Rcode2df=function(result,preprocessing){
    if(preprocessing!="") eval(parse(text=preprocessing))
    res=c()
     codes=unlist(strsplit(result,"\n",fixed=TRUE))
     for(i in 1:length(codes)){
          if(codes[i]=="") next
          if(length(grep("cat",codes[i]))==1) {
               if(grep("cat",codes[i])==1) next
          }
          res=c(res,codes[i])
          temp=capture.output(eval(parse(text=codes[i])))
          if(length(temp)==0) temp1=""
          else temp1=Reduce(pastelf,temp)
          res=c(res,temp1)

     }
     data.frame(result=res)

}

#' Make a FlexTable with a data.frame
#' @param df A data.frame
#' @param bordercolor A border color name
#' @return A FlexTable object
df2RcodeTable=function(df,bordercolor="gray"){
     df2FlexTable(df,add.rownames=FALSE,parRight=FALSE,
                  parLeft=TRUE,header.columns=FALSE,
                  bordercolor=bordercolor,oddcolor="lightcyan",evencolor="#FFFFFF")
}

#' Make a FlexTable object with character strings encoding R code
#' @param result character strings encoding R code
#' @param preprocessing character strings encoding R code as a preprocessing
Rcode2FlexTable=function(result,preprocessing=""){
     df=Rcode2df(result,preprocessing=preprocessing)
     df2RcodeTable(df)
}

#' Concatenate vectors after converting to character.
#' @param ... one or more R objects, to be converted to character vectors.
pastelf=function(...){
     paste(...,sep="\n")
}

#' Add ggplot into a document object
#' @param mydoc A document object
#' @param plot An R code encoding a ggplot
#' @param title An character string as a plot title
#' @return a document object
addggplot=function(mydoc,plot,title=""){
     if(title==""){
          mydoc=addSlide(mydoc,"Content2")
     } else{
          mydoc=addSlide(mydoc,"Title and Content")
          mydoc=addTitle(mydoc,title)
     }
     mydoc=addPlot(mydoc,fun=print,x=plot,vector.graphic=TRUE)
     mydoc
}

#' Add plot into a document object
#' @param mydoc A document object
#' @param plotfunction An R code encoding a plot
#' @param title An character string as a plot title
#' @param vector A logical. If TRUE, vector graphics are produced instead, PNG images if FALSE.
#' @return a document object
addplot=function(mydoc,plotfunction,title="",vector=TRUE){
     if(title!=""){
          mydoc=addSlide(mydoc,"Title and Content")
          mydoc=addTitle(mydoc,title)
     } else{
          mydoc=addSlide(mydoc,"Content")
     }
     mydoc=addPlot(mydoc,function() {plotfunction},vector.graphic=vector)
     mydoc
}


#' Add title slide into a document object
#' @param mydoc A document object
#' @param title An character string as a plot title
#' @param adddate A logical. If TRUE, insert a date
#' @param addsub A logical. If TRUE, insert a subtitle
#' @importFrom ReporteRs addDate addSubtitle
#' @return a document object
addTitleSlide=function(mydoc,title,adddate=TRUE,addsub=TRUE){
     mydoc=addSlide(mydoc,"Title Slide")
     mydoc=addTitle(mydoc,title)
     if(adddate) mydoc=addDate(mydoc)
     if(addsub) mydoc=addSubtitle(mydoc,"prepared by Web-R.org")
     mydoc
}

#' Make a formated text with R codes
#' @param result Character string encoding R codes as a main text
#' @param pre Character string encoding R codes as a preprocessing
#' @param post Character string encoding R codes as a postprocessing
#' @param fontsize A font size
#' @param fontfamily A character specifying the font family
#' @param cat A logical. If true, add line feed after main text
#' @param precat A logical. If true, add line feed before main text
#' @importFrom ReporteRs pot textProperties
makePot=function(result=NULL,pre=NULL,post=NULL,fontsize=NULL,fontfamily="Monaco",cat=FALSE,precat=TRUE){

    res1<-NULL
    res2<-NULL
    res3<-NULL
    if(!is.null(pre)) {
        if(precat) res1=capture.output(cat(pre,"\n"))
        else res1=capture.output(pre)
    }
    if(!is.null(result)) res2=capture.output(result)
    if(!is.null(post)) {
        if(cat) res3=capture.output(cat("\n",post))
        else res3=capture.output(post)

    }
    resall=c(res1,res2,res3)
    res=Reduce(pastelf,resall)
    if(is.null(fontsize)){

        linelength=length(gregexpr("\n",res)[[1]])
        if(linelength < 10) fontsize<-12
        else fontsize<-12-(linelength-10)%/%4-1

    }
    #browser()
    text1 <- pot(res,textProperties(font.size = fontsize,font.family = fontfamily))
    text1

}


#' Add a plot into a document object
#' @param mydoc A document object
#' @param image Character vector indicating path to image file
#' @param title title of the slide
addImageSlide=function(mydoc,image,title=""){
    if(title=="") mydoc=addSlide(mydoc,"Content")
    else mydoc=addSlide(mydoc,"Title and Content")
    if(title!="") mydoc=addTitle(mydoc,title)
    mydoc=addImage(mydoc,image)
    mydoc
}


#' Add a paragraph slide into a document object
#' @param mydoc A document object
#' @param result Character string encoding R codes as a main text
#' @param pre Character string encoding R codes as a preprocessing
#' @param post Character string encoding R codes as a postprocessing
#' @param title An character string as a plot title
#' @param fontsize A font size
#' @param fontfamily A character specifying the font family
#' @param cat A logical. If true, add line feed after main text
#' @param precat A logical. If true, add line feed before main text
#' @param new A logical. If TRUE, start with a new slide
#' @importFrom ReporteRs addParagraph
#' @return a document object
addParagraphSlide=function(mydoc,result=NULL,pre=NULL,post=NULL,title="",
                           fontsize=NULL,fontfamily="Monaco",cat=FALSE,precat=TRUE,new=TRUE){

     text1<-makePot(result=result,pre=pre,post=post,
                    fontsize=fontsize,fontfamily=fontfamily,cat=cat,precat=precat)
     if(new){
          mydoc = addSlide( mydoc, "Title and Content" )
          mydoc = addTitle( mydoc, title )
     }
     mydoc = addParagraph(mydoc,text1)
     mydoc
}

#' Add a FlexTable slide into a document object
#' @param mydoc A document object
#' @param mytable A FlexTable object
#' @param title An character string as a plot title
#' @importFrom ReporteRs addSlide addTitle
#' @return a document object
addFlexTableSlide=function(mydoc,mytable,title=""){
     if(title=="") {
          mydoc = addSlide( mydoc, "Content" )
     }
     else {
          mydoc = addSlide( mydoc, "Title and Content" )
          mydoc = addTitle( mydoc, title )
     }
     mydoc = addFlexTable(mydoc,mytable)
     mydoc
}

#' Make a R code slide into a document object
#' @param mydoc A document object
#' @param code  A character string encoding R codes
#' @param title An character string as a plot title
#' @param showCode A logical. If true, show R code in a slide
#' @param preprocessing A character string of R code as a preprocessing
#' @return a document object
addRcodeSlide=function(mydoc,code,title="",showCode=FALSE,preprocessing=""){

     if(showCode) {
          if(title=="") {
               mydoc = addSlide( mydoc, "Routput0" )
          }
          else {
               mydoc = addSlide( mydoc, "Routput" )
               mydoc = addTitle( mydoc, title )
          }
     } else {
          if(title!="") {
               mydoc = addSlide( mydoc, "Title and Content" )
               mydoc = addTitle( mydoc, title )
          } else{
               mydoc = addSlide( mydoc, "Content2" )
          }
     }
     if(showCode) mydoc = addParagraph(mydoc,code)
     # result=makeRcodePot(code)
     # mydoc = addParagraph(mydoc,result)
     result=Rcode2FlexTable(code,preprocessing=preprocessing)
     mydoc = addFlexTable(mydoc,result)
     mydoc
}


#' Make a powerpoint file with a data.frame
#' @param data A data.frame
#' @param title The title of a powerpoint file
#' @param template Character value, it represents the filename of the pptx file used as a template.
#' @param preprocessing A character string of R code
#' @param filename A path of destination file
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @importFrom ReporteRs pptx writeDoc
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' library(webr)
#' data2pptx(sampleData2)
data2pptx=function(data,title="Web-based Meta-Analysis",
                   template="myppt.pptx",preprocessing="",
                   filename="Report.pptx",width=7,height=5,units="in",
                   res=300,rawDataName=NULL,rawDataFile="rawData.RDS"){

     if(file.exists(template)) {
          mydoc = pptx(template=template)
     } else{
          mydoc = pptx()
     }
     mydoc=addTitleSlide(mydoc,"Web-based Meta-Analysis")

     if(!is.null(rawDataName)){
         rawData=readRDS(rawDataFile)
         assign(rawDataName,rawData)
     }

     if(preprocessing!="") eval(parse(text=preprocessing))


     for(i in 1:nrow(data)){
          eval(parse(text=data$code[i]))
          if(data$type[i]=="table"){
               mytable=eval(parse(text=data$code[i]))
               mydoc=addFlexTableSlide(mydoc,mytable,data$title[i])
          } else if(data$type[i]=="mytable"){
               res=eval(parse(text=data$code[i]))
               MyFTable=mytable2FTable(res,vanilla=TRUE)
               mydoc=addFlexTableSlide(mydoc,MyFTable,data$title[i])
          } else if(data$type[i]=="ggplot"){
               p<-eval(parse(text=data$code[i]))
               mydoc=addggplot(mydoc,plot=p,title=data$title[i])
          } else if(data$type[i]=="plot"){
               mydoc=eval(parse(text=paste0("addplot(mydoc,",data$code[i],",'",data$title[i],"')")))

          } else if(data$type[i]=="Rcode"){

               mydoc=addRcodeSlide(mydoc,code=data$code[i],title=data$title[i],
                                   preprocessing=preprocessing)

          } else if(data$type[i]=="PNG"){

               png(filename="temp.png",width=width,height=height,units=units,
                   res=res,type="cairo")
               eval(parse(text=data$code[i]))
               dev.off()
               mydoc=addImageSlide(mydoc,"temp.png")
               mydoc
          }


     }
     writeDoc(mydoc,file=filename)
}

#' Make a HTML5 file with a data.frame
#' @param data A data.frame
#' @param preprocessing A character string of R code
#' @param filename A path of destination file
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @importFrom rmarkdown render
#' @importFrom moonBook mytable
#' @importFrom ztable ztable print.ztable
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' library(webr)
#' data2HTML(sampleData2)
data2HTML=function(data,preprocessing="",filename="report.HTML",rawDataName=NULL,rawDataFile="rawData.RDS"){


     if(file.exists("report2.Rmd")) file.remove("report2.Rmd")

     tempReport <-  "report2.Rmd"


     cat("---\ntitle: 'Web-based Analysis with R'\n---\n",file=tempReport,append=TRUE)
     cat("```{r setup, include=FALSE}\n",file=tempReport,append=TRUE)
     cat("knitr::opts_chunk$set(echo = FALSE,message=FALSE,warning=FALSE,comment=NA,
         fig.width=9,fig.asp=0.618,fig.align='center',out.width='70%')\n",
         file=tempReport,append=TRUE)
     cat("```\n",file=tempReport,append=TRUE)

     cat("```{r,echo=FALSE,message=FALSE}\n",file=tempReport,append=TRUE)
     cat("require(moonBook)\n",file=tempReport,append=TRUE)
     cat("require(ztable)\n",file=tempReport,append=TRUE)
     cat("require(webr)\n",file=tempReport,append=TRUE)
     cat("```\n\n",file=tempReport,append=TRUE)

     if(!is.null(rawDataName)){
         cat("```{r}\n",file=tempReport,append=TRUE)
         cat("# Read Raw Data\n",file=tempReport,append=TRUE)
         temp=paste0("rawData=readRDS('",rawDataFile,"')\n")
         cat(temp,file=tempReport,append=TRUE)
         temp=paste0("assign('",rawDataName,"',rawData)\n")
         cat(temp,file=tempReport,append=TRUE)
         cat("```\n\n",file=tempReport,append=TRUE)
     }

     if(preprocessing!="") {
          cat("```{r}\n",file=tempReport,append=TRUE)
          cat("# Preprocessing\n",file=tempReport,append=TRUE)
          cat(preprocessing,'\n',file=tempReport,append=TRUE)
          cat("```\n\n",file=tempReport,append=TRUE)
     }

     mypptlist=data
     count=nrow(mypptlist)
     for(i in 1:count){

          if(!is.na(mypptlist$title[i])) {
               cat("## ",mypptlist$title[i],"\n",file=tempReport,append=TRUE)
          }

          if(mypptlist$type[i]=="mytable") {
              cat("```{r,results='asis'}\n",file=tempReport,append=TRUE)
              cat("result=",mypptlist$code[i],"\n",file=tempReport,append=TRUE)
              cat("print(ztable(result,longtable=TRUE),type='HTML')\n",file=tempReport,append=TRUE)
          } else if(mypptlist$type[i]=="table") {
               cat("```{r,results='asis'}\n",file=tempReport,append=TRUE)
          } else if(mypptlist$type[i]=="Rcode") {
               cat("```{r,echo=TRUE}\n",file=tempReport,append=TRUE)
          } else {
               cat("```{r}\n",file=tempReport,append=TRUE)
          }
          if(mypptlist$type[i]!="mytable") {
             cat(mypptlist$code[i],'\n',file=tempReport,append=TRUE)
          }
          cat("```\n\n",file=tempReport,append=TRUE)
     }

     out <- rmarkdown::render('report2.Rmd', rmarkdown::html_document())
     result=file.rename(out, filename)
     invisible(result)
}

#' Make a pdf file with a data.frame
#' @param data A data.frame
#' @param preprocessing A character string of R code
#' @param filename A path of destination file
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @importFrom rmarkdown render
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' data2pdf(sampleData2)
data2pdf=function(data,preprocessing="",filename="report.pdf",rawDataName=NULL,rawDataFile="rawData.RDS"){

     if(file.exists("report2.Rmd")) file.remove("report2.Rmd")
     tempReport <-  "report2.Rmd"

     cat("---\ntitle: 'Web-based Analysis with R'\n",file=tempReport,append=TRUE)
     cat("header-includes:\n- \\usepackage{kotex}\n- \\usepackage{multirow}\n",file=tempReport,append=TRUE)
     cat("- \\usepackage{colortbl}\n- \\usepackage{pdflscape}\n- \\usepackage[table]{xcolor}\n",file=tempReport,append=TRUE)
     cat("- \\usepackage{tabularx,booktabs}\n- \\usepackage{boxedminipage}\n- \\usepackage{graphicx}\n",
         file=tempReport,append=TRUE)
     cat("- \\usepackage{rotating}\n- \\usepackage{longtable}\n",file=tempReport,append=TRUE)
     cat("---\n",file=tempReport,append=TRUE)
     cat("```{r setup, include=FALSE}\n",file=tempReport,append=TRUE)
     cat("knitr::opts_chunk$set(echo = FALSE,message=FALSE,warning=FALSE,comment=NA,
         fig.width=9,fig.asp=0.618,fig.align='center',out.width='70%')\n",
         file=tempReport,append=TRUE)
     cat("```\n",file=tempReport,append=TRUE)

     cat("```{r,echo=FALSE,message=FALSE }\n",file=tempReport,append=TRUE)
     cat("require(moonBook)\n",file=tempReport,append=TRUE)
     cat("require(ztable)\n",file=tempReport,append=TRUE)
     cat("require(webr)\n",file=tempReport,append=TRUE)
     cat("options(ztable.type='latex')\n",file=tempReport,append=TRUE)
     cat("```\n\n",file=tempReport,append=TRUE)

     if(!is.null(rawDataName)){
         cat("```{r}\n",file=tempReport,append=TRUE)
         cat("# Read Raw Data\n",file=tempReport,append=TRUE)
         temp=paste0("rawData=readRDS('",rawDataFile,"')\n")
         cat(temp,file=tempReport,append=TRUE)
         temp=paste0("assign('",rawDataName,"',rawData)\n")
         cat(temp,file=tempReport,append=TRUE)
         cat("```\n\n",file=tempReport,append=TRUE)
     }

      if(preprocessing!="") {
          eval(parse(text=preprocessing))
          cat("```{r}\n",file=tempReport,append=TRUE)
          cat("# Preprocessing\n",file=tempReport,append=TRUE)
          cat(preprocessing,'\n',file=tempReport,append=TRUE)

          cat("```\n\n",file=tempReport,append=TRUE)
     }

     mypptlist=data
     count=nrow(mypptlist)
     for(i in 1:count){



          if(!is.na(mypptlist$title[i])) {
               cat("## ",mypptlist$title[i],"\n",file=tempReport,append=TRUE)
          }


          if(mypptlist$type[i]=="table") {
               cat("```{r,results='asis'}\n",file=tempReport,append=TRUE)
               temp=mypptlist$code[i]

               result<-eval(parse(text=temp))
               if("FlexTable" %in% class(result)){
                    cat("result=",mypptlist$code[i],"\n",file=tempReport,append=TRUE)
                    cat("df=attr(result,'df')\n",file=tempReport,append=TRUE)
                    cat("df=html2latex(df)\n",file=tempReport,append=TRUE)
                    cat("class(df)='data.frame'\n",file=tempReport,append=TRUE)
                    cat("print(ztable(df,longtable=TRUE),type='latex')\n",file=tempReport,append=TRUE)
               } else {
                    cat(mypptlist$code[i],'\n',file=tempReport,append=TRUE)


               }

          } else if(mypptlist$type[i]=="mytable"){
               cat("```{r,results='asis'}\n",file=tempReport,append=TRUE)
               cat("result=",mypptlist$code[i],"\n",file=tempReport,append=TRUE)
               cat("print(ztable(result,longtable=TRUE),type='latex')\n",file=tempReport,append=TRUE)
          } else if(mypptlist$type[i]=="Rcode") {
               cat("```{r,echo=TRUE}\n",file=tempReport,append=TRUE)
               cat(mypptlist$code[i],'\n',file=tempReport,append=TRUE)
          } else {
               cat("```{r}\n",file=tempReport,append=TRUE)
               cat(mypptlist$code[i],'\n',file=tempReport,append=TRUE)
          }
          cat("```\n\n",file=tempReport,append=TRUE)
     }

     out <- rmarkdown::render('report2.Rmd', params=list(format="PDF"),rmarkdown::pdf_document())
     result=file.rename(out, filename)
     invisible(result)
}


#' Make FlexTable with mytable : Internal function used in mytable2doc or mytable2ppt
#'
#' @param res An object of class moonBook::mytable
#' @param vanilla A logical. If true, make a vanilla table
#' @param bg A background color
#' @param parRight A logical. If true, right text alignment
#' @param padding Paragraph left and right padding - 0 or positive integer value
#' @param widths A numeric vector specifying columns widths in inches.
#' @importFrom utils read.csv
#' @importFrom ReporteRs addHeaderRow cellProperties spanFlexTableRows spanFlexTableColumns chprop
#' @importFrom moonBook mycsv
#' @return An object of ReporteRs::FlexTable
#' @export
#' @examples
#' require(moonBook)
#' require(ReporteRs)
#' res=mytable(Dx~.,data=acs)
#' res=mytable(Dx+sex~.,data=acs)
#' mytable2FTable(res)
#'
mytable2FTable=function(res, vanilla=FALSE,bg="#5B7778",parRight=TRUE,padding=5,widths=NULL){

     #vanilla=FALSE;bg="#5B7778";parRight=TRUE;padding=5;widths=NULL
     res
     mycsv(res,"test.csv",row.names = FALSE)
     test=read.csv("test.csv",colClasses = "character")
     file.remove("test.csv")
     #str(test)

     if("cbind.mytable" %in% class(res)){
          (pcolumns=seq(from=(ncol(test)-1)%/%length(res)+1,to=ncol(test),(ncol(test)-1)/length(res)))
          for(i in 1:length(pcolumns)) test[[pcolumns[i]]][test[[pcolumns[i]]]=="0.000"]="< 0.001"
          tableNo=length(attr(res,"caption"))
          colPerTable=(length(test)-1)/tableNo
          select=c(1,seq(2,length(test),by=colPerTable))
          TableName=names(test)[select]
          TableName
          colspan=c(1,rep(colPerTable,tableNo))

          header=paste0(unlist(test[1,]),"\n",unlist(test[2,]))
          test=test[-c(1,2),]
          colnames(test)=header
          if(vanilla){
               MyFTable=FlexTable(test,header.columns=FALSE)
          } else {
               myCellProps=cellProperties( border.color = "#EDBD3E",padding.left=padding,padding.right=padding)
               MyFTable=FlexTable(test,header.columns=FALSE
                                  , body.cell.props = cellProperties( border.color = "#EDBD3E",padding.left=padding,padding.right=padding)
                                  , header.cell.props = cellProperties( background.color = bg )
                                  , header.text.props = textProperties(color = "white",font.weight = "bold"))
               # MyFTable=setZebraStyle( MyFTable, odd = "#FFFFFF", even = "#DDDDDD" )
          }


          MyFTable=addHeaderRow(MyFTable,text.properties = textProperties(color = ifelse(vanilla,"black","white"),font.weight = "bold"),
                                value=TableName,colspan=colspan)
          MyFTable=addHeaderRow(MyFTable,text.properties = textProperties(color = ifelse(vanilla,"black","white"),font.weight = "bold"),
                                value=header)
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
          rowno
          change<-c()
          for(i in 1:(length(rowno)-1)){
               if(rowno[i]!=rowno[i+1]){
                    change=c(change,i)
               } else{
                    stop=c(stop,i)
               }
          }
          change=c(change,i+1)
          change
          if(!vanilla){
               MyFTable[rowno==1,]=chprop(myCellProps,background.color ="#DDDDDD")
               MyFTable[rowno==0,]=chprop(myCellProps,background.color ="#FFFFFF")
          }
          if(!rowno[1]){
               for(j in 1:length(pcolumns)) {
                    MyFTable=spanFlexTableRows(MyFTable,j=pcolumns[j],from=1,to=change[1])
               }
          }
          if(length(change)>1) for(i in 1:(length(change)-1)){
               if(change[i+1]>change[i]+1){
                    # MyFTable=spanFlexTableColumns(MyFTable,i=change[i]+1,from=1,to=ncol(test)-1)
                    for(j in 1:length(pcolumns)) {
                         MyFTable=spanFlexTableRows(MyFTable,j=pcolumns[j],from=change[i]+1,to=change[i+1])
                    }
               }
          }
          MyFTable[,1:ncol(test),to='header']=parCenter()
          MyFTable[,1]=parLeft()
          MyFTable

     } else{
          test[[ncol(test)]][test[[ncol(test)]]=="0.000"]="< 0.001"
          header=paste0(colnames(test),"\n",unlist(test[1,]))
          test=test[-1,]
          colnames(test)=header
          #MyFTable=vanilla.table(test)
          if(vanilla) {
               MyFTable=vanilla.table(test)
          } else {
               myCellProps=cellProperties( border.color = "#EDBD3E",padding.left=padding,padding.right=padding)
               MyFTable=FlexTable(test
                                  , body.cell.props = myCellProps
                                  , header.cell.props = cellProperties( background.color = bg )
                                  , header.text.props = textProperties(color = "white",font.weight = "bold"))
          }
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
          rowno
          change<-c()
          for(i in 1:(length(rowno)-1)){
               if(rowno[i]!=rowno[i+1]){
                    change=c(change,i)
               } else{
                    stop=c(stop,i)
               }
          }
          change
          change=c(change,i+1)
          change
          if(!vanilla){
               MyFTable[rowno==1,]=chprop(myCellProps,background.color ="#DDDDDD")
               MyFTable[rowno==0,]=chprop(myCellProps,background.color ="#FFFFFF")


               # if(!rowno[1]){
               #         MyFTable=spanFlexTableColumns(MyFTable,i=1,from=1,to=ncol(test)-1)
               #         MyFTable=spanFlexTableRows(MyFTable,j=ncol(test),from=1,to=change[1])
               # }
               if(length(change)>1) for(i in 1:(length(change)-1)){
                    if(change[i+1]>change[i]+1){
                         MyFTable=spanFlexTableColumns(MyFTable,i=change[i]+1,from=1,to=ncol(test)-1)
                         MyFTable=spanFlexTableRows(MyFTable,j=ncol(test),from=change[i]+1,to=change[i+1])
                    }
               }
          }
          #MyFTable=setZebraStyle( MyFTable, odd = "#FFFFFF", even = "#DDDDDD" )

          if(!is.null(widths)) MyFTable=setFlexTableWidths(MyFTable,widths=widths)
          MyFTable[,,to='header']=parCenter()
          if(parRight) MyFTable[,]=parRight()
          else MyFTable[,]=parCenter()

          MyFTable[,1:ncol(test),to='header']=parCenter()
          MyFTable[,1]=parLeft()
     }
     MyFTable
}


#' Write a cav file with comment
#' @param data A data.frame
#' @param file A path for destination file
#' @param metadata A character string representing R codes as a preprocessing
#' @param comment A string used to identify comments
#' @importFrom utils write.table
#' @export
writeCSVComment=function(data,file,metadata="",comment="#"){
     if(metadata!=""){
          count=length(unlist(strsplit(metadata,"\n")))+1
          temp=paste0(comment,unlist(strsplit(metadata,"\n")))
          temp=paste(temp,collapse="\n")
          temp=paste0(comment,count,"\n",temp)
          temp
          writeLines(text=temp,con=file)
     }
     # append the data.frame
     write.table(data, file = file, append = T, row.names = F, sep = ',',col.names=TRUE)
}

#' Read comment from a file
#' @param filename A path for destination file
#' @param comment A string used to identify comments
#' @export
readComment=function(filename,comment="#"){
     res=readLines(filename,1)
     res
     count=0
     if(!is.na(unlist(strsplit(res,comment,fixed=TRUE))[2])){
          count=as.numeric(unlist(strsplit(res,"#"))[2])
     }
     count
     if(count==0) {
          result=""
     } else{
          result=readLines(filename,count)
          result
          result=result[-1]
          result=gsub(comment,"",result)
          result=paste(result,collapse="\n")
     }
     result
}


#' Export pptList file to desired format
#' @param file The name of the file which the data are to be read from.
#' @param format desired ouput format. Possible choices are one of the c("HTML","pdf","word","pptx","plotzip")
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @export
exportCSV=function(file,format="HTML",rawDataName=NULL,rawDataFile="rawData.RDS"){
    if(!is.null(rawDataName)){
        rawData=readRDS(rawDataFile)
        assign(rawDataName,rawData)
    }
    data<-readr::read_csv(file,comment="#")
    preprocessing<-webr::readComment(file)
    temp=paste0("webr::data2",format,'(data,preprocessing="',preprocessing,'",rawDataName="',rawDataName,"\")")
    print(temp)
    eval(parse(text=temp))
}
