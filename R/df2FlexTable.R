#' Convert data.frame to FlexTable
#' @param df A data.frame
#' @param vanilla A logical. If true, make a vanilla table
#' @param bg A background color
#' @param add.rownames A logical. Whether or not add rownames
#' @param header.columns A logical. Whether or not add header columns
#' @param mode A character string. Choices are "html" or "latex"
#' @param bordercolor A border color
#' @param oddcolor A color of odd rows
#' @param evencolor A color of even rows
#' @param parRight A logical. If true, right text alignment
#' @param parLeft A logical. If true, left text alignment
#' @param padding.left Paragraph left padding - 0 or positive integer value
#' @param padding.right Paragraph right padding - 0 or positive integer value
#' @param padding.top Paragraph top padding - 0 or positive integer value
#' @param padding.bottom Paragraph bottom padding - 0 or positive integer value
#' @param widths A numeric vector specifying columns widths in inches.
#' @param digits An integer indicating the number of decimal places
#' @param NA2space A logical. If true, convert NA value to space
#' @param pcol An integer indicating p value. If specified, convert value less than 0.01 to "< 0.001" in given column.
#' @importFrom ReporteRs vanilla.table FlexTable setZebraStyle setFlexTableWidths parLeft parRight parCenter
#' @return A FlexTable object
#' @export
#' @examples
#' library(ReporteRs)
#' df2FlexTable(head(iris))
#' df2FlexTable(head(iris),type="latex")
df2FlexTable=function(df,vanilla=FALSE,bg="#5B7778",add.rownames=TRUE,header.columns=TRUE,mode="html",bordercolor="#EDBD3E",oddcolor = "#FFFFFF", evencolor = "#DDDDDD",
                      parRight=TRUE,parLeft=TRUE,padding.left=5,padding.right=5,padding.top=2,padding.bottom=2,widths=NULL,digits=NULL,NA2space=FALSE,pcol=NULL){
        if(!is.null(digits)){
                for(i in 1:length(digits)){
                        if(is.na(digits[i])) next
                        df[[i]]=round(df[[i]],digits[i])
                }
        }
        if(!is.null(pcol)){
                for(i in 1:length(pcol)){
                        df[[pcol]][df[[pcol]]<0.001]="< 0.001"
                }
        }
        if(NA2space) df[is.na(df)]=""
        if("mytable" %in% class(df)) {
                MyTable=mytable2FTable(df)
        } else{
                if(mode=="html"){
                        colnames(df)[colnames(df)=="\u03B2"]="&#946;"
                        colnames(df)[colnames(df)=="chisq"]="&#967;<sup>2</sup>"
                        colnames(df)[colnames(df)=="x2df"]="&#967;<sup>2</sup>/df"
                        colnames(df)[colnames(df)=="p"]="<i>p</i>"
                } else if(mode=="pptx"){
                        colnames(df)[colnames(df)=="\u03B2"]="β"
                        colnames(df)[colnames(df)=="chisq"]="χ2"
                        colnames(df)[colnames(df)=="x2df"]="χ2/df"
                        colnames(df)[colnames(df)=="p"]="p"
                }
                if(vanilla) {
                        MyTable=vanilla.table(df,add.rownames = add.rownames)
                } else {
                        MyTable=FlexTable(df,add.rownames = add.rownames
                                          , header.columns = header.columns
                                          , body.cell.props = cellProperties( border.color = bordercolor,padding.left=padding.left,padding.top=padding.top,
                                                                              padding.right=padding.right,padding.bottom=padding.bottom)
                                          , header.cell.props = cellProperties( background.color = bg,padding.left=padding.left,padding.top=padding.top,
                                                                                padding.right=padding.right,padding.bottom=padding.bottom)
                                          , header.text.props = textProperties(color = "white",font.weight = "bold"))
                        MyTable=setZebraStyle( MyTable, odd = oddcolor, even = evencolor )
                }
                if(!is.null(widths)) MyTable=setFlexTableWidths(MyTable,widths=widths)
                if(header.columns) MyTable[,,to='header']=parCenter()
                if(parRight) MyTable[,]=parRight()
                else if(parLeft) MyTable[,]=parLeft()
                else MyTable[,]=parCenter()
                if(add.rownames) MyTable[,1]=parCenter()
                MyTable
        }
        attr(MyTable,"df")=df
        MyTable
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
