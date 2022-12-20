plaicePlot <- function(seaObject, shape = 'spline', borderCol = 'black',
                        vlines=NULL, vlineCol="#6E6E66",
                        vlab=NULL, vlabSize = 3,
                        separateIndependentClones = FALSE,
                        clonesToFill = NULL,
                        showLegend = FALSE, markMeasuredTimepoints = NULL,
                        main = NULL, mainPos = 'middle', mainSize = 5,
                        xlab = NULL, ylab = FALSE, pad.left = 0.005,
                        annotations = NULL, annotationsSize=3,
                        interactivePlot = TRUE){
    x<-NULL
    y<-NULL
    name<-NULL
    ymin<-NULL
    ymax<-NULL
    tp<-NULL
    lab<-NULL


    ##get x and y values for bottom visualization (clone evo)
    seaObject <- .bottomLayout(seaObject,
                        separateIndependentClones = separateIndependentClones)

    if (shape == 'polygon'){
        coordsTblBtm <- .getCoordsTable(seaObject)

    }else if(shape == 'spline'){
        coordsTblBtm <- .getSmoothCoordsTbl(seaObject=seaObject,
                                        pad.left=pad.left)
    } else{
        stop('Shape must be wither "spline" or "polygon"')
    }

    ##get x and y values for top visualization (alleleaware)
    seaObject <-.topLayout(seaObject,
                        separateIndependentClones = separateIndependentClones)

    if (shape == 'polygon'){
        coordsTblTop <- .getCoordsTable(seaObject)

    }else if(shape == 'spline'){
    coordsTblTop <- .getSmoothCoordsTbl(seaObject=seaObject, pad.left=pad.left)
    }

    ##Color vector for allele marking
    btmCloneColors <- .getBottomCloneCols(seaObject, clonesToFill=clonesToFill)


    ##Plaice plot
    plcPlot <- ggplot(coordsTblBtm, aes(x = x, y = y)) +
        geom_polygon_interactive(aes(fill = name, group = name,
                                    tooltip = name,
                                    data_id = name),
                                show.legend = showLegend,
                                color = borderCol,
                                size = 0.25
        ) +
        scale_fill_manual(values = c(seaObject@col),
                                            name = "Clone labels") +
        theme_void() + theme(legend.position = "left")+
        ##new scale for alleleaware coloring
        new_scale('fill') +
        geom_polygon_interactive(aes(fill = name, group = name,
                                    tooltip = name, data_id = name),
                                color = borderCol,
                                size = 0.25,
                                data = coordsTblTop) +
        scale_fill_manual(values = btmCloneColors) +
        guides(fill='none') + #no legend
        geom_hline(yintercept = 0, size = 0.7)



    ##VERTICAL LINES
    if(!is.null(vlines)){
        vlinesData <- data.frame(vlines = vlines,
                                ymin = rep(-100,length(vlines)),
                                ymax = rep(100,length(vlines)))

        plcPlot <- plcPlot + geom_segment(mapping = aes(x = vlines,
                                                        xend=vlines,
                                                        y = ymin,
                                                        yend = ymax),
                                        data = vlinesData, color = vlineCol,
                                        show.legend = FALSE)

        if(!is.null(vlab)){
            vlinesData <- vlinesData %>% add_column(vlab = vlab)
            plcPlot <- plcPlot + geom_text(mapping = aes(x = vlines,
                                                        y = 100,
                                                        label = vlab,
                                                        vjust = -1),
                                        size = vlabSize,
                                        data = vlinesData) +
                expand_limits(y=100+vlabSize)
        }
    }

    ##BOTTOM TRIANGLES
    if(!is.null(markMeasuredTimepoints)){
        measuredTpData <- data.frame(tp = markMeasuredTimepoints,
                                    y = rep(-102,
                            length(markMeasuredTimepoints)))
        plcPlot <- plcPlot + geom_point(mapping = aes(x = tp, y = y),
                                        data = measuredTpData,
                                        shape = 'triangle',
                                        size = 2)
    }

    ##MAIN TITLE
    if(!is.null(main)){
        if(mainPos == 'middle'){
            mainCoords <- (min(seaObject@timepoints) +
                            max(seaObject@timepoints))/2
        } else{
            if(mainPos == 'left'){
                mainCoords <- min(seaObject@timepoints)

            }else{
                if(mainPos == 'right'){
                    mainCoords <- max(seaObject@timepoints)
                }else{
                    stop('main position (mainPos) must be "left", "middle",
                        or "right"')
                }
            }
        }

        mainY <- mainSize*4 + 100

        if(mainY < 110 & !is.null(vlab)){
            mainY    <- 110
        }

        mainLabel <- data.frame(lab = main, x = mainCoords, y = mainY)

        plcPlot <- plcPlot + geom_text(mapping = aes(x = x, y = y,
                                                    label = lab),
                                    size = mainSize,
                                    data = mainLabel)
    }

    ##X AXIS LABEL
    if(!is.null(xlab)){

        axisLabelX <- data.frame(lab = xlab,
                                x = ((min(seaObject@timepoints) +
                                        max(seaObject@timepoints))/2),
                                y = -106)

        plcPlot <- plcPlot + geom_text(mapping = aes(x = x, y = y,
                                                    label = lab),
                                        data = axisLabelX)
    }

    ##Y AXIS LABEL (+ LINES AND %)
    if(ylab){
        pad <- max(coordsTblTop$x)-min(coordsTblTop$x)
        yLine <- data.frame(x = rep(min(coordsTblTop$x) - pad*0.05,2),
                                                ymin = c(0.5,-0.5),
                                                ymax = c(100,-100))
        percentages <- data.frame(lab = c('0%','100%','100%','0%'),
                                x = rep(min(coordsTblTop$x) - pad*0.1,2),
                                y = c(4,100,-4,-100))
        ylabs <- data.frame(lab=c('Remaining healthy allele',
                                'Cancer Cell Fraction'),
                            x = rep(min(coordsTblTop$x) - pad*0.15,2),
                            y = c(-50,50))

        plcPlot <- plcPlot +
            geom_segment(mapping = aes(x = x, xend=x, y = ymin, yend = ymax),
                                    data = yLine, color = 'black',
                        show.legend = FALSE) +
            geom_text(mapping = aes(x = x, y = y, label = lab),
                                data = percentages,
                                size=2.5) +
            geom_text(mapping = aes(x = x, y = y, label = lab, angle = 90),
                                data = ylabs,
                                size=3.7)

    }
    ##Annotations
    if(!is.null(annotations)){
        if(sum(annotations$col %in% c('black','Black')) == 0){
            colorsScale <- 'white'
        } else {
            colorsScale <- c('black','white')
        }
        plcPlot <- plcPlot + geom_text(mapping = aes(x = x, y = y,
                                                    label = lab,
                                                    color = col),
                                        size = annotationsSize,
                                        data = annotations) +
            guides(color = 'none') +
            scale_color_discrete(type = colorsScale)
    }

    if (interactivePlot){
        ###MAKE INTERACTIVE
    tooltip_css <- "color:black;padding:10px;border-radius:10px 20px 10px 20px;"

        plcPlot <- girafe(ggobj = plcPlot, width_svg = 10, height_svg = 10) %>%
            girafe_options(opts_sizing(rescale = FALSE),
                                        opts_hover_inv(css = "opacity:0.3;"),
                                        opts_hover(css = "opacity:1;"),
                                        opts_tooltip(css = tooltip_css,
                    use_fill=TRUE))
    }
    return(plcPlot)
}
