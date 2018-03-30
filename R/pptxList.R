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

               if(count==0) p("There is no saved data.")
               # if(count>0) hr(),
               # if(count>0) actionButton(ns("showPPTList"),"show/hide saved List")
               # ,
               # if(count>0) conditionalPanel("true==false",
               #                              checkboxInput(ns("showList"),"showList",value=FALSE)),
               # if(count>0) conditionalPanel(sprintf("input['%s']==true",ns("showList")),
               #                              hr(),
               #                              uiOutput(ns("PPTListUI2")))

          )
     })

     # observeEvent(input$showPPTList,{
     #     updateCheckboxInput(session,"showList",value=!input$showList)
     # })

     # output$PPTListUI2=renderUI({
     #
     #     input$showPPTList
     #
     #     ns<-session$ns
     #
     #     count= length(savedPPT$title)
     #
     #     mydf=data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,stringsAsFactors = FALSE)
     #
     #     for(i in 1:count){
     #         local({
     #             j<-i
     #             outputname=paste0("output",j*2-1)
     #             output[[outputname]]=renderPrint({
     #                 h4(mydf$title[j])
     #             })
     #         })
     #     }
     #     for(i in 1:count){
     #         local({
     #             j<-i
     #             outputname=paste0("output",j*2)
     #             if(savedPPT$type[j]=="table"){
     #                 output[[outputname]]=renderFlexTable({
     #                     mytable=eval(parse(text=mydf$code[j]))
     #                     mytable
     #                 })
     #             } else if(savedPPT$type[j]=="mytable"){
     #                 output[[outputname]]=renderFlexTable({
     #                     res=eval(parse(text=mydf$code[j]))
     #                     MyFTable=mytable2FTable(res,vanilla=TRUE)
     #                     MyFTable
     #                 })
     #             } else if(savedPPT$type[j]=="ggplot"){
     #                 output[[outputname]]=renderPlot({
     #                     p<-eval(parse(text=mydf$code[j]))
     #                     p
     #                 })
     #
     #             } else if(savedPPT$type[j]=="plot"){
     #                 output[[outputname]]=renderPlot({
     #                     p<-eval(parse(text=mydf$code[j]))
     #                     p
     #                 })
     #
     #             } else if(savedPPT$type[j]=="Rcode"){
     #                 output[[outputname]]=renderFlexTable({
     #                     result=Rcode2FlexTable(mydf$code[j])
     #                     result
     #
     #                 })
     #
     #             } else if(savedPPT$type[j]=="PNG"){
     #                 output[[outputname]]=renderImage({
     #                     myfunction<-eval(parse(text=mydf$code[j]))
     #                     png("temp.png",width=input$plotWidth,height=input$plotHeight,units=input$plotUnit,
     #                         res=input$plotRes,type="cairo")
     #                     myfunction
     #                     dev.off()
     #                     list(src = "temp.png",
     #                          contentType = 'image/png',
     #                          width = 400,
     #                          height = 300,
     #                          alt = "This is alternate text")
     #                 }, deleteFile = TRUE)
     #             }
     #         })
     #
     #     }
     #     output_list <- lapply(1:count, function(j) {
     #
     #         outputname=paste0("output",j*2)
     #         if(mydf$type[j] %in% c("table","mytable","Rcode")) tableOutput(ns(outputname))
     #         else if(mydf$type[j] %in% c("ggplot","plot")) plotOutput(ns(outputname))
     #         else if(mydf$type[j]=="PNG") imageOutput(ns(outputname))
     #
     #     })
     #     output_list2 <- lapply(1:count, function(j) {
     #
     #         outputname=paste0("output",j*2-1)
     #         htmlOutput(ns(outputname))
     #
     #     })
     #
     #     # Convert the list to a tagList - this is necessary for the list of items
     #     # to display properly.
     #     my_list=c(output_list,output_list2)
     #     for(i in 1:length(output_list)){
     #         my_list[[2*i-1]]=output_list2[[i]]
     #         my_list[[2*i]]=output_list[[i]]
     #     }
     #
     #     do.call(tagList, my_list)
     #
     #
     # })




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

               # temporarily switch to the temp dir, in case you do not have write
               # permission to the current working directory
               owd <- setwd(tempdir())
               on.exit(setwd(owd))

               data=pptdf2()

               data2pptx(data,title="Web-based Meta-Analysis",
                         preprocessing=input$preprocessing,
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

               data2docx(data,title="Web-based Meta-Analysis",
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
          if(data$type[i] %in% c("plot","ggplot","PNG","png")){
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


#' Make a HTML5 file with a data.frame
#' @param data A data.frame
#' @param preprocessing A character string of R code
#' @param filename A path of destination file
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @param vanilla logical. Whether or not make vanilla table
#' @importFrom rmarkdown render
#' @importFrom moonBook mytable
#' @importFrom ztable ztable print.ztable
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' library(webr)
#' data2HTML(sampleData2)
data2HTML=function(data,preprocessing="",filename="report.HTML",rawDataName=NULL,rawDataFile="rawData.RDS",
                   vanilla=FALSE){


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
     cat("require(ggplot2)\n",file=tempReport,append=TRUE)
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
              cat("mytable2flextable(",mypptlist$code[i],",vanilla=",vanilla,")\n",file=tempReport,append=TRUE)

          } else if(mypptlist$type[i]=="data"){
              cat("```{r,results='asis'}\n",file=tempReport,append=TRUE)

          } else if(mypptlist$type[i]=="table") {
               cat("```{r,results='asis'}\n",file=tempReport,append=TRUE)
          } else if(mypptlist$type[i]=="Rcode") {
               cat("```{r,echo=TRUE}\n",file=tempReport,append=TRUE)
          } else {
               cat("```{r}\n",file=tempReport,append=TRUE)
          }
          if(mypptlist$type[i]=="data"){
              cat("df2flextable(",mypptlist$code[i],",vanilla=",vanilla,")\n",file=tempReport,append=TRUE)
          } else if(mypptlist$type[i]=="table"){
                code=set_argument(mypptlist$code[i],argument="vanilla",value=vanilla)
                cat(code,"\n",file=tempReport,append=TRUE)
          } else if(mypptlist$type[i]!="mytable") {
             cat(mypptlist$code[i],'\n',file=tempReport,append=TRUE)
          }
          cat("```\n\n",file=tempReport,append=TRUE)
     }

     out <- rmarkdown::render('report2.Rmd', rmarkdown::html_document())
     result=file.rename(out, filename)
     file.remove("report2.Rmd")
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
     cat("require(ggplot2)\n",file=tempReport,append=TRUE)
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
               if("flextable" %in% class(result)){
                    cat("result=",mypptlist$code[i],"\n",file=tempReport,append=TRUE)
                    cat("df=result$body$dataset\n",file=tempReport,append=TRUE)
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
          } else if(mypptlist$type[i]=="data"){
              cat("```{r,results='asis'}\n",file=tempReport,append=TRUE)
              cat("print(ztable(",mypptlist$code[i],",longtable=TRUE),type='latex')\n",file=tempReport,append=TRUE)
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
     file.remove("report2.Rmd")
     invisible(result)
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
