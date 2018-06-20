#' Make transparent theme
#' @param size border size. default value is 0
#' @importFrom ggplot2 theme element_rect element_blank
#' @export
transparent=function(size=0){


        temp=theme(rect= element_rect(fill = 'transparent',size=size),
                   panel.background=element_rect(fill = 'transparent'),
                   panel.border=element_rect(size=size),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank())
        temp
}

#' Make default palette
#' @param n number of colors
#' @importFrom grDevices hcl
#' @export
gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
}


#' Make subcolors with main colors
#' @param main character. main colors
#' @param no number of subcolors
#' @importFrom ztable gradientColor
#' @export
makeSubColor=function(main,no=3){
        result=c()
        for(i in 1:length(main)){
                temp=ztable::gradientColor(main[i],n=no+2)[2:(no+1)]
                result=c(result,temp)
        }
        result
}


#'Draw a PieDonut plot
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param start offset of starting point from 12 o'clock in radians
#'@param addPieLabel A logical value. If TRUE, labels are added to the Pies
#'@param addDonutLabel A logical value. If TRUE, labels are added to the Donuts
#'@param showRatioDonut A logical value. If TRUE, ratios are added to the DonutLabels
#'@param showRatioPie A logical value. If TRUE, ratios are added to the PieLabels
#'@param ratioByGroup A logical value. If TRUE, ratios ara calculated per group
#'@param showRatioThreshold An integer. Threshold to show label as a ratio of total. default value is 0.02.
#'@param labelposition A number indicating the label position
#'@param labelpositionThreshold label position threshold. Default value is 0.1.
#'@param r0 Integer. start point of pie
#'@param r1 Integer. end point of pie
#'@param r2 Integer. end point of donut
#'@param explode pies to explode
#'@param selected donuts to explode
#'@param explodePos explode position
#'@param color color
#'@param pieAlpha transparency of pie
#'@param donutAlpha transparency of pie
#'@param maxx maximum position of plot
#'@param showPieName logical. Whether or not show Pie Name
#'@param showDonutName logical. Whether or not show Pie Name
#'@param title title of plot
#'@param pieLabelSize integer. Pie label size
#'@param donutLabelSize integer. Donut label size
#'@param titlesize integer. Title size
#'@param explodePie Logical. Whether or not explode pies
#'@param explodeDonut Logical. Whether or not explode donuts
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@importFrom ggplot2 aes geom_segment coord_fixed scale_fill_manual xlim ylim annotate geom_text guides
#'@importFrom grid grid.newpage viewport
#'@importFrom ggforce geom_arc_bar theme_no_axes
#'@importFrom rlang .data
#'@importFrom dplyr arrange group_by summarize lag
#'@importFrom tidyr spread complete
#'@importFrom scales percent
#'@importFrom moonBook addLabelDf getMapping
#'@export
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'browser=c("MSIE","Firefox","Chrome","Safari","Opera")
#'share=c(50,21.9,10.8,6.5,1.8)
#'df=data.frame(browser,share)
#'PieDonut(df,aes(browser,count=share),r0=0.7,start=3*pi/2,labelpositionThreshold=0.1)
#'PieDonut(df,aes(browser,count=share),r0=0.7,explode=5,start=3*pi/2)
#'PieDonut(mtcars,aes(gear,carb),start=3*pi/2,explode=3,explodeDonut=TRUE,maxx=1.7)
#'PieDonut(mtcars,aes(carb,gear),r0=0)
#'PieDonut(acs,aes(smoking,Dx),title="Distribution of smoking status by diagnosis")
#'PieDonut(acs,aes(Dx,smoking),ratioByGroup=FALSE,r0=0)
#'PieDonut(acs,aes(Dx,smoking),selected=c(1,3,5,7),explodeDonut=TRUE)
#'PieDonut(acs,aes(Dx,smoking),explode=1,selected=c(2,4,6,8),labelposition=0,explodeDonut=TRUE)
#'PieDonut(acs,aes(Dx,smoking),explode=1)
#'PieDonut(acs,aes(Dx,smoking),explode=1,explodeDonut=TRUE,labelposition=0)
#'PieDonut(acs,aes(Dx,smoking),explode=1,explodePie=FALSE,explodeDonut=TRUE,labelposition=0)
#'PieDonut(acs,aes(Dx,smoking),selected=c(2,5,8), explodeDonut=TRUE,start=pi/2,labelposition=0)
#'PieDonut(acs,aes(Dx,smoking),r0=0.2,r1=0.9,r2=1.3,explode=1,start=pi/2,explodeDonut=TRUE)
#'PieDonut(acs,aes(Dx,smoking),r0=0.2,r1=0.9,r2=1.3,explode=1,start=pi/2,labelposition=0)
#'PieDonut(acs,aes(Dx,smoking),explode=1,start=pi,explodeDonut=TRUE,labelposition=0)
#'require(dplyr)
#'df=mtcars %>% group_by(gear,carb) %>% summarize(n=n())
#'PieDonut(df,aes(pies=gear,donuts=carb,count=n),ratioByGroup=FALSE)
PieDonut=function(data,mapping,
                  start=0,
                  addPieLabel=TRUE,addDonutLabel=TRUE,
                  showRatioDonut=TRUE,showRatioPie=TRUE,
                  ratioByGroup=TRUE,
                  showRatioThreshold=0.02,
                  labelposition=2,
                  labelpositionThreshold=0.1,
                  r0=0.3,r1=1.0,r2=1.2,
                  explode=NULL,
                  selected=NULL,
                  explodePos=0.1,
                  color="white",
                  pieAlpha=0.8,
                  donutAlpha=1.0,
                  maxx=NULL,
                  showPieName=TRUE,
                  showDonutName=FALSE,
                  title=NULL,
                  pieLabelSize=4,
                  donutLabelSize=3,
                  titlesize=5,
                  explodePie=TRUE,explodeDonut=FALSE,
                  use.label=TRUE,use.labels=TRUE){


        # data=mtcars;mapping=aes(gear,carb)
        # data=acs;mapping=aes(Dx,smoking)

        # require(tidyverse)
        # df=mtcars %>% group_by(gear,carb) %>% summarize(n=n())
        # data=df;mapping=aes(x=gear,y=carb,count=n)
        #  data=browsers;mapping=aes(pies=browser,donuts=version,count=share)
        # start=pi/2
        # addPieLabel=TRUE;addDonutLabel=TRUE
        # showRatioDonut=TRUE;showRatioPie=TRUE
        # ratioByGroup=TRUE
        # showRatioThreshold=0.02
        #  pieAlpha=0.7;donutAlpha=1.0
        # explodePie=TRUE;explodeDonut=TRUE
        # explode=1
        # explodePos=0.1
        # color="white"
        # r0=0;r1=1;r2=1.2
        # labelposition=1
        # pieLabelSize=4
        # donutLabelSize=3
        # use.label=TRUE;use.labels=TRUE
        # maxx=NULL
        # explode=1;explodeDonut=TRUE;labelposition=0
        # selected=NULL;title=NULL;titlesize=5
        # data=acs;mapping=aes(Dx)
        # browser=c("MSIE","Firefox","Chrome","Safari","Opera")
        # share=c(50,21.9,10.8,6.5,1.8)
        # df=data.frame(browser,share)
        # data=df;mapping=aes(browser,count=share);r0=0.7;explode=5;start=3*pi/2
        # data=mtcars;mapping=aes(gear,carb)
        # showPieName=TRUE;showDonutName=FALSE
        # data=mtcars;mapping=aes(gear,carb);explode=3;explodeDonut=TRUE


        (cols=colnames(data))
        if(use.labels) data=addLabelDf(data,mapping)

        count<-NULL

        if("count" %in% names(mapping)) count<-getMapping(mapping,"count")
        count

        pies<-donuts<-NULL
        (pies=getMapping(mapping,"pies"))
        if(is.null(pies)) (pies=getMapping(mapping,"pie"))
        if(is.null(pies)) (pies=getMapping(mapping,"x"))

        (donuts=getMapping(mapping,"donuts"))
        if(is.null(donuts)) (donuts=getMapping(mapping,"donut"))
        if(is.null(donuts)) (donuts=getMapping(mapping,"y"))



                if(!is.null(count)){

                        df<-data %>% group_by(.data[[pies]]) %>%dplyr::summarize(Freq=sum(.data[[count]]))
                        df
                } else{
                        df=data.frame(table(data[[pies]]))
                }
                colnames(df)[1]=pies
                df$end=cumsum(df$Freq)
                df$start=dplyr::lag(df$end)
                df$start[1]=0
                total=sum(df$Freq)
                df$start1=df$start*2*pi/total
                df$end1=df$end*2*pi/total
                df$start1=df$start1+start
                df$end1=df$end1+start
                df$focus=0
                if(explodePie) df$focus[explode]=explodePos
                df$mid=(df$start1+df$end1)/2
                df$x=ifelse(df$focus==0,0,df$focus*sin(df$mid))
                df$y=ifelse(df$focus==0,0,df$focus*cos(df$mid))
                df$label=df[[pies]]
                df$ratio=df$Freq/sum(df$Freq)
                if(showRatioPie) {
                        df$label=paste0(df$label,"\n(",scales::percent(df$ratio),")")


                }

                df$labelx=(r0+r1)/2*sin(df$mid)+df$x
                df$labely=(r0+r1)/2*cos(df$mid)+df$y
                if(!is.factor(df[[pies]])) df[[pies]]<-factor(df[[pies]])
                df

                mainCol=gg_color_hue(nrow(df))


                df$radius=r1
                df$radius[df$focus!=0]=df$radius[df$focus!=0]+df$focus[df$focus!=0]
                df$hjust=ifelse((df$mid %% (2*pi))>pi,1,0)
                df$vjust=ifelse(((df$mid %% (2*pi)) <(pi/2))|(df$mid %% (2*pi) >(pi*3/2)),0,1)
                df$segx=df$radius*sin(df$mid)
                df$segy=df$radius*cos(df$mid)
                df$segxend=(df$radius+0.05)*sin(df$mid)
                df$segyend=(df$radius+0.05)*cos(df$mid)
                df

                if(!is.null(donuts)){
                subColor=makeSubColor(mainCol,no=length(unique(data[[donuts]])))
                subColor


                data
                if(!is.null(count)){

                    df3 <- as.data.frame(data[c(donuts,pies,count)])
                    colnames(df3)=c("donut","pie","Freq")
                    df3
                    df3<-eval(parse(text="complete(df3,donut,pie)"))

                    # df3<-df3 %>% complete(donut,pie)
                    df3$Freq[is.na(df3$Freq)]=0
                    if(!is.factor(df3[[1]])) df3[[1]]=factor(df3[[1]])
                    if(!is.factor(df3[[2]])) df3[[2]]=factor(df3[[2]])

                    df3<-df3 %>% arrange(.data$pie,.data$donut)
                    a<-df3 %>% spread(.data$pie,value=.data$Freq)
                    # a<-df3 %>% spread(pie,value=Freq)
                    a=as.data.frame(a)
                    a
                    rownames(a)=a[[1]]
                    a=a[-1]
                    a
                    colnames(df3)[1:2]=c(donuts,pies)


                } else{
                    df3=data.frame(table(data[[donuts]],data[[pies]]),stringsAsFactors = FALSE)
                    colnames(df3)[1:2]=c(donuts,pies)
                    a=table(data[[donuts]],data[[pies]])
                    a
                }

                a
                df3
                df3$group=rep(colSums(a),each=nrow(a))
                df3$pie=rep(1:ncol(a),each=nrow(a))
                total=sum(df3$Freq)
                total
                df3$ratio1=df3$Freq/total
                df3
                if(ratioByGroup) {
                        df3$ratio=scales::percent(df3$Freq/df3$group)
                } else {
                        df3$ratio<-scales::percent(df3$ratio1)
                }
                df3$end=cumsum(df3$Freq)
                df3
                df3$start=dplyr::lag(df3$end)
                df3$start[1]=0

                df3$start1=df3$start*2*pi/total
                df3$end1=df3$end*2*pi/total
                df3$start1=df3$start1+start
                df3$end1=df3$end1+start
                df3$mid=(df3$start1+df3$end1)/2
                df3$focus=0

                if(!is.null(selected)){
                        df3$focus[selected]=explodePos
                } else if(!is.null(explode)) {
                        selected=c()
                        for(i in 1:length(explode)){
                              start=1+nrow(a)*(explode[i]-1)
                              selected=c(selected,start:(start+nrow(a)-1))
                        }
                        selected
                        df3$focus[selected]=explodePos
                }
                df3
                df3$x=0
                df3$y=0
                df

                if(!is.null(explode)){
                        explode
                for(i in 1:length(explode)){

                xpos=df$focus[explode[i]]*sin(df$mid[explode[i]])
                ypos=df$focus[explode[i]]*cos(df$mid[explode[i]])

                df3$x[df3$pie==explode[i]]=xpos
                df3$y[df3$pie==explode[i]]=ypos
                }
                }
                df3$no=1:nrow(df3)
                df3$label=df3[[donuts]]
                if(showRatioDonut) {
                        if(max(nchar(levels(df3$label)))<=2) df3$label=paste0(df3$label,"(",df3$ratio,")")
                        else df3$label=paste0(df3$label,"\n(",df3$ratio,")")
                }
                df3$label[df3$ratio1==0]=""

                if(labelposition==0) df3$label[df3$ratio1<showRatioThreshold]=""


                df3$hjust=ifelse((df3$mid %% (2*pi))>pi,1,0)
                df3$vjust=ifelse(((df3$mid %% (2*pi)) <(pi/2))|(df3$mid %% (2*pi) >(pi*3/2)),0,1)
                df3$no=factor(df3$no)
                df3
                # str(df3)
                labelposition
                if(labelposition>0){
                        df3$radius=r2
                        if(explodeDonut) df3$radius[df3$focus!=0]=df3$radius[df3$focus!=0]+df3$focus[df3$focus!=0]

                        df3$segx=df3$radius*sin(df3$mid)+df3$x
                        df3$segy=df3$radius*cos(df3$mid)+df3$y
                        df3$segxend=(df3$radius+0.05)*sin(df3$mid)+df3$x
                        df3$segyend=(df3$radius+0.05)*cos(df3$mid)+df3$y

                        if(labelposition==2) df3$radius=(r1+r2)/2
                        df3$labelx= (df3$radius)*sin(df3$mid)+df3$x
                        df3$labely= (df3$radius)*cos(df3$mid)+df3$y
                } else{
                        df3$radius=(r1+r2)/2
                        if(explodeDonut) df3$radius[df3$focus!=0]=df3$radius[df3$focus!=0]+df3$focus[df3$focus!=0]
                        df3$labelx=df3$radius*sin(df3$mid)+df3$x
                        df3$labely=df3$radius*cos(df3$mid)+df3$y
                }
                df3$segx[df3$ratio1==0]=0
                df3$segxend[df3$ratio1==0]=0
                df3$segy[df3$ratio1==0]=0
                df3$segyend[df3$ratio1==0]=0
                if(labelposition==0){
                df3$segx[df3$ratio1<showRatioThreshold]=0
                df3$segxend[df3$ratio1<showRatioThreshold]=0
                df3$segy[df3$ratio1<showRatioThreshold]=0
                df3$segyend[df3$ratio1<showRatioThreshold]=0
                }
                df3

                del=which(df3$Freq==0)
                del
                if(length(del)>0) subColor<-subColor[-del]
                subColor
                }
                p <- ggplot() + theme_no_axes() + coord_fixed()

                if(is.null(maxx)) {
                        r3=r2+0.3
                } else{
                        r3=maxx
                }


                p1<-p + geom_arc_bar(aes_string(x0 = "x", y0 = "y",
                                                r0 = as.character(r0), r = as.character(r1),
                                                start="start1",end="end1",
                                         fill = pies),alpha=pieAlpha,color=color,
                                     data = df)+transparent()+
                        scale_fill_manual(values=mainCol)+
                        xlim(r3*c(-1,1))+ylim(r3*c(-1,1))+guides(fill=FALSE)
                if((labelposition==1)&(is.null(donuts))){
                        p1<-p1+ geom_segment(aes_string(x="segx",y="segy",
                                                        xend="segxend",yend="segyend"),data=df)+
                                geom_text(aes_string(x="segxend",y="segyend",label="label",hjust="hjust",vjust="vjust"),size=pieLabelSize,data=df)

                } else if((labelposition==2)&(is.null(donuts))){
                        p1<-p1+ geom_segment(aes_string(x="segx",y="segy",
                                                        xend="segxend",yend="segyend"),data=df[df$ratio<labelpositionThreshold,])+
                                geom_text(aes_string(x="segxend",y="segyend",label="label",hjust="hjust",vjust="vjust"),size=pieLabelSize,data=df[df$ratio<labelpositionThreshold,])+
                                geom_text(aes_string(x="labelx",y="labely",label="label"),size=pieLabelSize,data=df[df$ratio>=labelpositionThreshold,])


                } else{
                        p1 <-p1+geom_text(aes_string(x="labelx",y="labely",label="label"),size=pieLabelSize,data=df)
                }

                if(showPieName) p1<-p1+annotate("text",x=0,y=0,label=pies,size=titlesize)
                p1

                if(!is.null(donuts)){


                # donutAlpha=1.0;color="white"
                # explodeDonut=FALSE
                if(explodeDonut) {
                p3<-p+geom_arc_bar(aes_string(x0 = "x", y0 = "y", r0 = as.character(r1),
                                              r = as.character(r2), start="start1",end="end1",
                                       fill = "no",explode="focus"),alpha=donutAlpha,color=color,
                                   data = df3)
                } else{
                        p3<-p+geom_arc_bar(aes_string(x0 = "x", y0 = "y", r0 = as.character(r1),
                                                      r = as.character(r2), start="start1",end="end1",
                                               fill = "no"),alpha=donutAlpha,color=color,
                                           data = df3)
                }

                p3<-p3+transparent()+
                        scale_fill_manual(values=subColor)+
                        xlim(r3*c(-1,1))+ylim(r3*c(-1,1))+guides(fill=FALSE)

                p3
                if(labelposition==1){
                p3<-p3+ geom_segment(aes_string(x="segx",y="segy",
                                         xend="segxend",yend="segyend"),data=df3)+
                        geom_text(aes_string(x="segxend",y="segyend",
                                      label="label",hjust="hjust",vjust="vjust"),size=donutLabelSize,data=df3)
                } else if(labelposition==0){
                        p3<-p3+geom_text(aes_string(x="labelx",y="labely",
                                             label="label"),size=donutLabelSize,data=df3)
                } else{
                        p3<-p3+ geom_segment(aes_string(x="segx",y="segy",
                                                        xend="segxend",yend="segyend"),data=df3[df3$ratio1<labelpositionThreshold,])+
                                geom_text(aes_string(x="segxend",y="segyend",
                                                     label="label",hjust="hjust",vjust="vjust"),size=donutLabelSize,data=df3[df3$ratio1<labelpositionThreshold,])+
                                geom_text(aes_string(x="labelx",y="labely",
                                                            label="label"),size=donutLabelSize,data=df3[df3$ratio1>=labelpositionThreshold,])

                }

                if(!is.null(title)) p3<-p3+annotate("text",x=0,y=r3,label=title,size=titlesize)
                else if(showDonutName) p3<-p3+annotate("text",x=(-1)*r3,y=r3,label=donuts,hjust=0,size=titlesize)

                grid.newpage()
                print(p1,vp=viewport(height=1,width=1))
                print(p3,vp=viewport(height=1,width=1))
                }
                else{
                    p1
                }


        # }

}

