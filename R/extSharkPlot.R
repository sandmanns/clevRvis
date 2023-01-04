extSharkPlot <- function(seaObject, showLegend = FALSE, main = NULL,
                        timepoints = NULL, width = 10, interactivePlot = TRUE){
    
    if(is(seaObject, "seaObject") == FALSE){
        stop('No seaObject provided as input.')
    }
    if(showLegend != TRUE && showLegend != FALSE){
        stop("No valid input for showLegend provided.")
    }
    if(!is.null(main) && !is.character(main)){
        stop("No valid input for main provided.")
    }
    if(!is.null(timepoints) && !is.numeric(timepoints)){
        stop("No valid input for timepoints provided.")
    }
    if(!is.numeric(width) || width <= 0){
        stop("No valid input for width provided.")
    }
    if(interactivePlot != TRUE && interactivePlot != FALSE){
        stop("No valid input for interactivePlot provided.")
    }
    
    if(is.null(timepoints)){
        timepoints <- seaObject@timepoints ## by default all timepoints
    }
    ##Which ccfs to show
    fracTable <- seaObject@fracTable[,which(seaObject@timepoints %in% timepoints)]

    if (!is.array(fracTable)){
        if(length(timepoints)==1){
            y <- rev(seq_len(length(fracTable)))
            x <- rep(0, length(fracTable))
        } else{
            y <-    rep(0, length(fracTable))
            x <- seq_len(length(fracTable))
        }
        lab <- as.factor(seaObject@cloneLabels)
    } else {
        y <- rep(rev(seq_len(nrow(fracTable))),ncol(fracTable))
        lab <- as.factor(seaObject@cloneLabels[seq_len(nrow(fracTable))])
        x <- rep(seq_len(ncol(fracTable)),each = nrow(fracTable))
    }
    ccf <- as.numeric(fracTable)
    ##Data frame for plotting ccf as size
    df <- data.frame(lab, x, y, ccf)

    if (length(which(ccf == 0))>0){
        df <- df[-which(ccf == 0),] ##don't plot ccf = 0
    }

    ##to plot size from 0 to 100
    df <- df %>% add_row(x=NA, y=NA, lab=NA, ccf=0)
    df <- df %>% add_row(x=NA, y=NA, lab=NA, ccf=100)

    #get sorted
    if (seaObject@defaultLabels){
        clone_ids <- parse_number(sort(unique(as.character(df$lab))))
    }else{
        clone_ids <- sort(seaObject@cloneLabels, index.return = TRUE)$ix
    }

    if(!is.array(fracTable)){
        if(length(timepoints)==1){
            dfTP <- data.frame(x=0, y = max(df$y, na.rm = TRUE)+1,
                                lab = timepoints)
        } else{
            dfTP <- data.frame(x=seq_len(length(fracTable)),
                                y = rep(max(df$y, na.rm = TRUE)+1,
                        length(fracTable)),
                                lab = timepoints)
        }
    } else{
        ##Column labels df
        dfTP <- data.frame(x=seq_len(ncol(fracTable)),
                            y = rep(max(df$y, na.rm = TRUE)+1,
                            ncol(fracTable)),
                            lab = timepoints)
    }
    ##Interactive plot
    dotplot <- ggplot() +
                geom_point_interactive(mapping = aes(x = x, y = y, color = lab,
                                                    size = ccf, tooltip = ccf,
                                                    data_id = lab),
                                        data = df,
                                        show.legend = showLegend) +
        scale_colour_manual(values = seaObject@col[clone_ids],
                                                name = "Clone labels",
                                                na.translate = FALSE) +
        scale_size_area(name='Relative CCFs', max_size = 15) + theme_void() +
        expand_limits(x=c(min(df$x, na.rm = TRUE)-0.5,
                        max(df$x, na.rm = TRUE)+0.5),
                        y=c(min(df$y, na.rm = TRUE)-0.5,
                            max(df$y, na.rm = TRUE)+0.5)) +
        guides(colour = guide_legend(override.aes = list(size=5))) +
        geom_text(mapping = aes(x = x, y = y, label = lab),
                            data = dfTP)

    if(!is.array(fracTable)){
        dotplot <- dotplot + expand_limits(y=-1)
    }
    ##Add basic shark plot to the left side
    shkPlt <- sharkPlot(seaObject, main=main)

    ##Interactive
    if (interactivePlot){
    tooltip_css <- "color:black;padding:10px;border-radius:10px 20px 10px 20px;"

        suppressWarnings(girafe(ggobj = shkPlt+dotplot, width_svg = width,
                                height_svg = 10) %>%
                        girafe_options(opts_sizing(rescale = FALSE),
                                        opts_hover_inv(css = "opacity:0.3;"),
                                        opts_hover(css = "opacity:1;"),
                                        opts_tooltip(css = tooltip_css,
                                        use_fill=TRUE)))
    } else {
        suppressWarnings(plot_grid(shkPlt,dotplot))
    }

}



