#' UI of pptxList shiny module
#' @param id A string
#' @importFrom shiny NS textAreaInput checkboxInput
#' @export
#' @examples
#' library(shiny)
#' library(ggplot2)
#' library(editData)
#' library(moonBook)
#' library(readr)
#' if(interactive()){
#' ui=fluidPage(
#'     pptxListInput("pptxlist")
#'     )
#' server=function(input,output,session){
#'     mydf=callModule(pptxList,"pptxlist")
#' }
#' shinyApp(ui,server)
#' }
pptxListInput=function(id){
    ns=NS(id)

    tagList(
        checkboxInput("showpreprocessing","show preprocessing"),
        conditionalPanel(condition="input.showpreprocessing==true",
                         textAreaInput(ns("preprocessing"),"preprocessing",value="",width='100%',height = '100%')
        ),
        uiOutput(ns("pptListUI"))
    )

}
