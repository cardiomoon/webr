#' Add title to docx file
#' @param x A document object
#' @param title Title
#' @param size font size
#' @param color font color
#' @param before Whether or not add blank paragraph before title
#' @param after Whether or not add blank paragraph after title
#' @importFrom officer shortcuts fpar ftext body_add_fpar
#' @importFrom stats update
#' @export
add_title=function(x,title="",size=20,color=NULL,before=TRUE,after=TRUE){
    bold_face <- shortcuts$fp_bold(font.size = size)
    if(!is.null(color)) bold_face=update(bold_face,color=color)
    fpar1=fpar(ftext(title, prop = bold_face))
    if(before) x <- x %>% body_add_par("",style="Normal")
    x <- x %>% body_add_fpar(fpar1)
    if(after) x <- x %>% body_add_par("",style="Normal")
    x
}


#' Add text to document
#' @param mydoc A document object
#' @param title An character string as a plot title
#' @param text text string to be added
#' @param style text style
#' @export
add_text=function(mydoc,title="",text,style="Normal"){
    if(class(mydoc)=="rpptx"){
        mydoc <- mydoc %>%
            add_slide(layout = "Title and Content", master = "Office Theme") %>%
            ph_with_text(type="title",str=title) %>%
            ph_with_text(type="body",str=text)

    } else{
        mydoc %>%
           add_title(title) %>%
           body_add_par(value=text,style=style)
    }
    mydoc
}

#' Add two ggplots into a document object
#' @param mydoc A document object
#' @param title An character string as a plot title
#' @param plot1 An R code encoding the first ggplot
#' @param plot2 An R code encoding the second ggplot
#' @param width plot width in inches
#' @param height plot height in inches
#' @return a document object
#' @importFrom officer body_end_section
#' @export
#' @examples
#' require(ggplot2)
#' require(magrittr)
#' require(officer)
#' gg1 <- ggplot(data = iris, aes(Sepal.Length, Petal.Length)) + geom_point()
#' gg2 <- ggplot(data = iris, aes(Sepal.Length, Petal.Length, color = Species)) + geom_point()
#' read_docx() %>% add_2ggplots(title="Two plots",plot1=gg1,plot2=gg2)
add_2ggplots=function(mydoc,title="Two plots",plot1,plot2,width=3,height=2.5){
    gg1<-plot1
    gg2<-plot2

    if(class(mydoc)=="rpptx"){

        mydoc<- mydoc %>%
            add_slide(layout = "Two Content", master = "Office Theme") %>%
            ph_with_text(type="title",str=title) %>%
            ph_with_vg(code = print(gg1), type = "body") %>%
            ph_with_vg(code = print(gg2), type = "body",index=2)


    } else{
        mydoc<-mydoc %>%
            add_title(title) %>%
            body_end_section() %>%
            body_add_vg(code=print(gg1),width=width,height=height) %>%
            body_add_vg(code=print(gg2),width=width,height=height) %>%
            body_end_section(continuous = TRUE,
                             colwidths = c(.5, .5), space = .05, sep = FALSE)
    }
    mydoc

}
#' Add two plots into a document object
#' @param mydoc A document object
#' @param title An character string as a plot title
#' @param plotstring1 An R code string encoding the first plot
#' @param plotstring2 An R code string encoding the second plot
#' @param width plot width in inches
#' @param height plot height in inches
#' @return a document object
#' @export
#' @examples
#' require(magrittr)
#' require(officer)
#' plotstring1="plot(1:10)"
#' plotstring2="hist(rnorm(100))"
#' read_docx() %>% add_2plots(plotstring1,plotstring2,title="Two plots")
add_2plots=function(mydoc,plotstring1,plotstring2,title="",width=3,height=2.5){

    if(class(mydoc)=="rpptx"){
        temp1=paste0("ph_with_vg(mydoc,code=",plotstring1,",type = \"body\")")
        temp2=paste0("ph_with_vg(mydoc,code=",plotstring2,",type = \"body\",index=2)")
        mydoc<- mydoc %>%
            add_slide(layout = "Two Content", master = "Office Theme") %>%
            ph_with_text(type="title",str=title)
        mydoc=eval(parse(text=temp1))
        mydoc=eval(parse(text=temp2))
    } else{
        temp1=paste0("body_add_vg(mydoc,code=",plotstring1,
                     ",width=",width,",height=",height,")")
        temp2=paste0("body_add_vg(mydoc,code=",plotstring2,
                     ",width=",width,",height=",height,")")
        mydoc <- mydoc %>%
            add_title(title) %>%
            body_end_section()
        mydoc=eval(parse(text=temp1))
        mydoc=eval(parse(text=temp2))
        mydoc=body_end_section(mydoc,continuous = TRUE,
                         colwidths = c(.5, .5), space = .05, sep = FALSE)
    }
    mydoc
}

# plotstring1="plot(1:10)"
# plotstring2="hist(rnorm(100))"
#
# read_docx() %>% add_2plots(plotstring1="plot(1:10)",plotstring2="hist(rnorm(100))",title="Two plots") %>% print(target="test.docx")

# gg1 <- ggplot(data = iris, aes(Sepal.Length, Petal.Length)) +
#     geom_point()
# gg2 <- ggplot(data = iris, aes(Sepal.Length, Petal.Length, color = Species)) +
#     geom_point()
#
# text1="This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.
# When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:"
#
# my_doc <- read_pptx()  %>%
#     add_text("Text1",text=text1) %>%
#     add_2ggplots(title="Two plots",plot1=gg1,plot2=gg2) %>%
#     add_text("Text2",text=text1)
#
# print(my_doc, target = "section.pptx")
