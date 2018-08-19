#'Chooser Input
#'@param inputId  input Id
#'@param leftLabel Label for left column
#'@param rightLabel Label for right column
#'@param leftChoices choices for left column
#'@param rightChoices choices for right column
#'@param size number of column lines to be displayed
#'@param multiple logical enalble multiple selection
#'@param width width of left and right columns in pixel
#'@importFrom shiny reactive renderUI tagList NS uiOutput tags singleton HTML div icon
#'@export
chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE,width=100) {

     leftChoices <- lapply(leftChoices, tags$option)
     rightChoices <- lapply(rightChoices, tags$option)

     if (multiple)
          multiple <- "multiple"
     else
          multiple <- NULL

     style=paste0("width: ",width,"px;")

     tagList(
          singleton(tags$head(
               tags$script(src="chooser-binding.js"),
               tags$style(type="text/css",
                          HTML(".chooser-container { display: inline-block; }")
               )
          )),
          div(id=inputId, class="chooser",
              div(class="chooser-container chooser-left-container",
                  tags$label(leftLabel),
                  tags$br(),
                  tags$select(class="left", size=size, multiple=multiple, leftChoices,style=style)
              ),
              div(class="chooser-container chooser-center-container",
                  icon("arrow-circle-o-right", "right-arrow fa-3x"),
                  tags$br(),
                  icon("arrow-circle-o-left", "left-arrow fa-3x")
              ),
              div(class="chooser-container chooser-right-container",
                  tags$label(rightLabel),
                  tags$br(),
                  tags$select(class="right", size=size, multiple=multiple, rightChoices,style=style)
              )
          )
     )
}



#' UI of chooser Module
#' @param id id
#'@export
chooserUI=function(id){
     ns<-NS(id)
     uiOutput(ns("chooser"))
}

#' Server function of chooser Module
#' @param input input
#' @param output output
#' @param session session
#' @param leftChoices choices for left column
#' @param rightChoices choices for right column
#' @param size number of column lines to be displayed
#' @param width width of left and right columns in pixel
#' @importFrom shiny callModule
#' @export
chooser=function(input,output,session,leftChoices, rightChoices=reactive(c()),
                 size=reactive(0),width=reactive(130)){
     ns<-session$ns

     output$chooser=renderUI({
         mychoices=union(leftChoices(),rightChoices())
         colsize=ifelse(size()==0,min(length(mychoices),20),size())
         colwidth=ifelse(width()==0,(max(nchar(mychoices))+1)*8+3,width())

          tagList(
               chooserInput(ns("chooser1"), "All Columns", "Selected Columns",
                            leftChoices(), rightChoices(), size = colsize, multiple = TRUE,width=colwidth
               )
          )
     })

     result=reactive(input$chooser1)
     return(result)
}


#' UI of chooser2 Module
#' @param id id
#'@export
chooser2UI=function(id){
    ns<-NS(id)
    uiOutput(ns("chooser"))
}

#' Server function of chooser2 Module
#' @param input input
#' @param output output
#' @param session session
#' @param leftChoices choices for left column
#' @param rightChoices choices for right column
#' @param size number of column lines to be displayed
#' @param width width of left and right columns in pixel
#' @importFrom shiny reactiveValues actionButton observeEvent
#' @export
chooser2=function(input,output,session,leftChoices, rightChoices=reactive(c()),
                 size=reactive(0),width=reactive(130)){
    ns<-session$ns

    RV=reactiveValues(choices=c(),selected=c())

    output$chooser=renderUI({

        RV$choices=leftChoices()
        RV$selected=rightChoices()

        tagList(
            actionButton(ns("allvar"),"Select All"),
            actionButton(ns("resetvar"),"Reset Variables"),
            chooserUI(ns("chooser1"))

        )
    })

    observeEvent(input$allvar,{
        RV$selected=union(RV$selected,RV$choices)
        RV$choices=c()
    })

    observeEvent(input$resetvar,{
        RV$choices=leftChoices()
        RV$selected=rightChoices()
    })


   result=callModule(chooser,"chooser1",leftChoices=reactive(RV$choices),rightChoices=reactive(RV$selected))

   observeEvent(result(),{
       RV$choices=result()$left
       RV$selected=result()$right
   })


    # result=reactive(input$chooser1)
    return(result)
}


.onAttach <-function(libname,pkgname){

    packageStartupMessage("Welcome to webr package\nRegister inputHandler for chooserInput")
shiny::registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
    if (is.null(data))
        NULL
    else
        list(left=as.character(data$left), right=as.character(data$right), selected=as.character(data$selected))
}, force = TRUE)
}
