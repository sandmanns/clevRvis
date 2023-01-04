sharkPlot <- function(seaObject, showLegend = FALSE, main = NULL){
    
    if(is(seaObject, "seaObject") == FALSE){
        stop('No seaObject provided as input.')
    }
    if(showLegend != TRUE && showLegend != FALSE){
        stop("No valid input for showLegend provided.")
    }
    if(!is.null(main) && !is.character(main)){
        stop("No valid input for main provided.")
    }
    
    name<-NULL
    lab<-NULL

    cloneLabels <- seaObject@cloneLabels
    ##If it's just one clone
    if (nrow(seaObject@fracTable)==1){
        df <- data.frame(name = cloneLabels, x = 0, y = 0)
        intPlot <- ggplot(df) + geom_point_interactive(size = 10,
                                                    mapping = aes(x = x,
                                                            y = y,
                                                            color = name,
                                                            tooltip = name,
                                                            data_id = name),
                                                    show.legend = showLegend)+
            scale_colour_manual(values = seaObject@col,
                                name = "Clone labels") + theme_void()

        if(!is.null(main)){
            mainDf <- data.frame(lab = main, x = 0, y = 1)
            intPlot <- intPlot + geom_text(mapping = aes(x = x, y = y,
                                                        label = lab),
                                            size = 5, data = mainDf) +
                expand_limits(y=-1)
        }
    }
    ##If all clones are independent
    else if(sum(seaObject@parents)==0){
        intPlot <- .indepClonesPlot(cloneLabels = cloneLabels,
                                    showLegend = showLegend,
                                    cloneCols = seaObject@col)
    }
    else{
        intPlot <- .basicSharkPlot(seaObject)

        ##Number of expected trees
        numTrees <- sum(seaObject@parents == 0)
        ##list with parent vectors divided by families
        l_from <- .getFromTo(seaObject)[seq_len(numTrees)]
        ##list with child vectors divided by families
        l_to <- tail(.getFromTo(seaObject),numTrees)
        ##get initial (parent) clones
        initialClones <- which(seaObject@parents==0)
        ##Table with the known parents (not independent)
        parentTbl <- intPlot$data %>%
            select(name, x, y) %>%
            filter(name %in% initialClones)

        ##Labels of independent clones
    indepCloneLabels <- .getIndependentCloneLabels(numTrees, l_to)$cloneLabs
        ##number of independent clones
        count <- .getIndependentCloneLabels(numTrees, l_to)$count

        ##x and y positions of the independent clones
        indepCloneXpos <- .getIndependentCloneXY(count, parentTbl, intPlot)$x
        indepCloneYpos <- .getIndependentCloneXY(count, parentTbl, intPlot)$y

        ##create new dataframe for the points
        plotData <- data.frame(x=intPlot$data$x, y=intPlot$data$y,
                                id = intPlot$data$name)

        if (count != 0){
            ##dataframe with the independent clones data
            indepClonesData <- data.frame(x = indepCloneXpos,
                                            y = indepCloneYpos,
                                            id = indepCloneLabels)

            ##bind the rows to the points dataframe
            plotData <- rbind(plotData, indepClonesData)
        }


        ##get the order of the sorted clones
        if (seaObject@defaultLabels){
            clone_ids <- parse_number(sort(plotData$id))
        }else{
            clone_ids <- sort(cloneLabels, index.return = TRUE)$ix
        }

        ##Add column with clone labels
        plotData <- plotData %>%
            add_column(name=cloneLabels[as.numeric(plotData$id)])


        ##Add independent clones to the plot
        intPlot <- intPlot +
            geom_point_interactive(size = 10, mapping = aes(x = x,
                                                            y = y,
                                                            color = name,
                                                            tooltip = name,
                                                            data_id = name),
                                    show.legend = showLegend,
                                    data=plotData)+
            scale_colour_manual(values = seaObject@col[clone_ids],
                                                    name = "Clone labels") +
            expand_limits(x=c(min(plotData$x)-0.25, max(plotData$x)+0.25),
                        y=c(min(plotData$y)-0.25, max(plotData$y)+0.25))

    }
    if(!is.null(main) & length(cloneLabels)>1){
        if(sum(seaObject@parents)==0){
            x <- mean(plotData$x, na.rm = TRUE)
            y <- max(plotData$y+1, na.rm = TRUE)
        }else{
            y <- mean(plotData$y, na.rm = TRUE)
            x <- min(plotData$x-1, na.rm = TRUE)
        }

        mainLabel <- data.frame(lab = main,x,y)

        intPlot <- intPlot + geom_text(mapping = aes(x = x, y = y,
                                                    label = lab),
                                        size = 5,
                                        data = mainLabel)
    }

    return(intPlot)
}



