plotWidget <- function(seaObject, shark = TRUE, dolphin = TRUE,
                        shape = 'spline', borderCol = NULL, vlines = NULL,
                        vlineCol="#6E6E66", vlab=NULL, vlabSize = 3,
                        pos = 'center',
                        separateIndependentClones=FALSE, showLegend = FALSE,
                        markMeasuredTimepoints = NULL, downloadWidget = NULL,
                        mainDph = NULL, mainPosDph = 'middle', mainSizeDph = 5,
                        mainShk = NULL,
                        xlab = NULL, ylab = NULL, pad.left = 0.005,
                        annotations = NULL, width = 12, height = 9){


    tooltip_css <- "color:black;padding:10px;border-radius:10px 20px 10px 20px;"

    if(shark & dolphin){
        splot <- sharkPlot(seaObject, showLegend = showLegend, main = mainShk)
        fplot <- dolphinPlot(seaObject, vlines = vlines, vlineCol = vlineCol,
                            vlab = vlab, shape = shape, borderCol = borderCol,
                            markMeasuredTimepoints = markMeasuredTimepoints,
                            main = mainDph, mainPos=mainPosDph,
                            vlabSize = vlabSize,
                            mainSize = mainSizeDph, xlab = xlab,
                            ylab = ylab, pad.left = pad.left, pos=pos,
                        separateIndependentClones = separateIndependentClones,
                            annotations = annotations)

        ##interactive plot to html widget
        gPlot <- girafe(ggobj = fplot+splot, width_svg = width,
                        height_svg = height) %>%
            girafe_options(opts_sizing(rescale = FALSE),
                            opts_hover_inv(css = "opacity:0.3;"),
                            opts_hover(css = "opacity:1;"),
                            opts_tooltip(css = tooltip_css, use_fill=TRUE))
    }

    else{
        if(shark & !dolphin){
            p <- sharkPlot(seaObject, showLegend = showLegend, main = mainShk)
        }
        if(dolphin & !shark){
            p <- dolphinPlot(seaObject, showLegend = showLegend,
                            vlabSize = vlabSize,
                            vlines = vlines, vlineCol = vlineCol,
                            vlab = vlab, shape = shape, borderCol = borderCol,
                            markMeasuredTimepoints = markMeasuredTimepoints,
                            main = mainDph, mainPos=mainPosDph, pos = pos,
                            mainSize = mainSizeDph, xlab = xlab,
                            ylab = ylab, pad.left = pad.left,
                        separateIndependentClones = separateIndependentClones,
                            annotations = annotations)
        }

        gPlot <- girafe(ggobj = p, width_svg = width, height_svg = height) %>%
            girafe_options(opts_sizing(rescale = FALSE),
                            opts_hover_inv(css = "opacity:0.3;"),
                            opts_hover(css = "opacity:1;"),
                            opts_tooltip(css = tooltip_css, use_fill=TRUE))
    }

    if(!is.null(downloadWidget)){
        htmlwidgets::saveWidget(gPlot, downloadWidget,
                                                        selfcontained = TRUE)
    }


    return(gPlot)
}

