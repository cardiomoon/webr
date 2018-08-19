library(webr)

require(moonBook)

ui=fluidPage(
    h4("Dynamic control of leftChoices of chooserInput()"),
    radioButtons("dataname","Select data",choices=c("acs","mtcars","iris","radial")),
    p("You can see the vars in Exclude1 and Exclude2 being excluded from the Choices"),
    fluidRow(

        column(5,
              chooser2UI("chooser")
        ),
        column(7,
               fluidRow(
                   column(3,
                          br(),
                          actionButton("toGroup1","Send to Exclude1")
                   ),
                   column(4,
                          selectInput("group1","Exclude1",choices=c("None",1:5),multiple=TRUE)
                   ),
                   column(2,
                          br(),
                          actionButton("resetGroup1","Reset")
                   )
               ),
               fluidRow(
                   column(3,
                          br(),
                          actionButton("toGroup2","Send to Exclude2")
                   ),
                   column(4,
                          selectInput("group2","Exclude2",choices=c("None",1:5),multiple=TRUE)
                   ),
                   column(2,
                          br(),
                          actionButton("resetGroup2","Reset")
                   )
               )

        )),
    br(),
    verbatimTextOutput("text")
)


server=function(input,output,session){

    data=reactive({eval(parse(text=input$dataname))})

    observe({
        groupvar<-groupvar1<-groupvar2<-colnames(data())
        if(length(input$group2)>0) groupvar1=setdiff(groupvar,input$group2)
        updateSelectInput(session,"group1",choices=c(groupvar1),selected=input$group1)
        if(length(input$group1)>0) groupvar2=setdiff(groupvar,input$group1)
        updateSelectInput(session,"group2",choices=c(groupvar2),selected=input$group2)

    })

    choices1=reactive({
        groups=union(input$group1,input$group2)
        setdiff(colnames(data()),groups)
    })

    result=callModule(chooser2,"chooser",leftChoices=choices1)


    observeEvent(input$toGroup1,{
           updateSelectInput(session,"group1",selected=c(input$group1,result()$right))
    })
    observeEvent(input$toGroup2,{
        updateSelectInput(session,"group2",selected=c(input$group2,result()$right))
    })
    observeEvent(input$resetGroup1,{
        updateSelectInput(session,"group1",selected="")
    })
    observeEvent(input$resetGroup2,{
        updateSelectInput(session,"group2",selected="")
    })

    output$text=renderPrint({
        cat("str(result())\n")
        str(result())
        cat("input$Exclude1\n")
        str(input$group1)
    })

}

shinyApp(ui,server)

