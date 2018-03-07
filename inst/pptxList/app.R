library(shiny)
library(moonBook)
library(webr)

ui=fluidPage(
    pptxListInput("pptxlist")
)
server=function(input,output,session){
     mydf=callModule(pptxList,"pptxlist")
}
shinyApp(ui,server)
