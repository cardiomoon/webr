library(shiny)
library(ggplot2)
library(ReporteRs)
library(editData)
library(moonBook)
library(readr)
library(webr)

ui=fluidPage(
    pptxListInput("pptxlist")
)
server=function(input,output,session){
     mydf=callModule(pptxList,"pptxlist")
}
shinyApp(ui,server)
