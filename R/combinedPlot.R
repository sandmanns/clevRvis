combinedPlot <- function(seaObject, shark = TRUE, dolphin = TRUE,
                        shape = 'spline', borderCol = NULL, vlines = NULL,
                        vlineCol="#6E6E66", vlab=NULL, vlabSize = 3,
                        pos = 'center',
                        separateIndependentClones=FALSE, showLegend = FALSE,
                        markMeasuredTimepoints = NULL, downloadWidget = NULL,
                        mainDph = NULL, mainPosDph = 'middle', mainSizeDph = 5,
                        mainShk = NULL,
                        xlab = NULL, ylab = NULL, pad.left = 0.005,
                        annotations = NULL, width = 12, height = 9){

    if(is(seaObject, "seaObject") == FALSE){
        stop('No seaObject provided as input.')
    }
    if(shark != TRUE && shark != FALSE){
        stop("No valid input for shark provided.")
    }
    if(dolphin != TRUE && dolphin != FALSE){
        stop("No valid input for dolphin provided.")
    }
    if(shape != "spline" && shape != "polygon"){
        stop("No valid input for shape provided.")
    }
    if(!is.null(borderCol) && is.null(.isColor(borderCol))){
        stop("No valid input for borderCol provided.")
    }
    if(!is.null(vlines) && !is.numeric(vlines)){
        stop("No valid input for vlines provided.")
    }
    if(is.null(.isColor(vlineCol))){
        stop("No valid input for vlineCol provided.")
    }
    if(!is.null(vlab) && !is.character(vlab) && !is.numeric(vlab)){
        stop("No valid input for vlab provided.")
    }
    if(!is.numeric(vlabSize)){
        stop("No valid input for vlabSize provided.")
    }
    if(pos != "center" && pos != "bottom" && pos != "top"){
        stop("No valid input for pos provided.")
    }
    if(separateIndependentClones != TRUE && separateIndependentClones != FALSE){
        stop("No valid input for separateIndependentClones provided.")
    }
    if(showLegend != TRUE && showLegend != FALSE){
        stop("No valid input for showLegend provided.")
    }
    if(!is.null(markMeasuredTimepoints) && !is.numeric(markMeasuredTimepoints)){
        stop("No valid input for markMeasuredTimepoints provided.")
    }
    if(!is.null(downloadWidget) && !is.character(downloadWidget)){
        stop("No valid input for downloadWidget provided.")
    }
    if(!is.null(mainDph) && !is.character(mainDph)){
        stop("No valid input for mainDph provided.")
    }
    if(mainPosDph != "left" && mainPosDph != "middle" && mainPosDph != "right"){
        stop("No valid input for mainPosDph provided.")
    }
    if(!is.numeric(mainSizeDph)){
        stop("No valid input for mainSizeDph provided.")
    }
    if(!is.null(mainShk) && !is.character(mainShk)){
        stop("No valid input for mainShk provided.")
    }
    if(!is.null(xlab) && !is.character(xlab)){
        stop("No valid input for xlab provided.")
    }
    if(!is.null(ylab) && !is.character(ylab)){
        stop("No valid input for ylab provided.")
    }
    if(!is.null(pad.left) && !is.numeric(pad.left)){
        stop("No valid input for pad.left provided.")
    }
    if(!is.null(annotations) && !is.data.frame(annotations)){
        stop("No valid input for annotations provided.")
    }
    if(!is.numeric(width) || width <= 0){
        stop("No valid input for width provided.")
    }
    if(!is.numeric(height) || height <= 0){
        stop("No valid input for height provided.")
    }

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

