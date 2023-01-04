createSeaObject <- function(fracTable, parents, timepoints,
                            col = NULL, cloneLabels = NULL,
                            originTimepoint = NULL,
                            timepointInterpolation = TRUE,
                            therapyEffect = NULL) {

    if(!is.matrix(fracTable) || !is.numeric(fracTable)){
        stop("No valid input for fracTable provided.")
    }
    if(!is.vector(parents) || !is.numeric(parents)){
        stop("No valid input for parents provided.")
    }
    if(!is.vector(timepoints) || !is.numeric(timepoints)){
        stop("No valid input for timepoints provided.")
    }
    if(!is.null(col) && is.null(.isColor(col))){
        stop("No valid input for col provided.")
    }
    if(!is.null(cloneLabels) && !is.character(cloneLabels)){
        stop("No valid input for cloneLabels provided.")
    }
    if(!is.null(originTimepoint) && !is.numeric(originTimepoint)){
        stop("No valid input for originTimepoint provided.")
    }
    if(timepointInterpolation != TRUE && timepointInterpolation != FALSE){
        stop("No valid input for timepointInterpolation provided.")
    }
    if(!is.null(therapyEffect) && !is.numeric(therapyEffect)){
        stop("No valid input for therapyEffect provided.")
    }
    
    # nest levels
    nestLevels <- .getAllNestLevels(parents)

    ## Change the name of the clones to numbers (in fracTable)
    rownames(fracTable) <- seq_len(dim(fracTable)[1])

    ## timepoints as column names
    colnames(fracTable) <- timepoints

    # default clone labels are just Clone: + 1:numClones
    if (is.null(cloneLabels)) {
        cloneLabels <- paste('Clone:', as.character(seq_len(nrow(fracTable))))
        defaultLabels <- TRUE
    } else {defaultLabels <- FALSE}

    ## Check for single timepoints that timepoint estimation is true
    if (!timepointInterpolation & length(timepoints) == 1) {
        stop("ERROR: To visualize clonal evolution from single timepoint
            timepointInterpolation mustbe set to TRUE")
    } else if (is.null(originTimepoint) & length(timepoints) == 1) {
        stop('ERROR: To visualize clonal evolution from a single timepoint
            originTimepoint must be manually specified.')
    }

    # check origin timepoint
    if (is.null(originTimepoint)) {
        originTimepoint <- timepoints[1] - (timepoints[2] - timepoints[1])
    } else {
        if (!(is.numeric(originTimepoint))) {
            stop("ERROR: originTimepoint must be a numeric value")
        }
        if (originTimepoint >= min(timepoints)) {
            stop("ERROR: originTimepoint must be a timepoint before
                the first timepoint in the timepoints vector")
        }
    }

    ## Values lower than 0.1 are considered 0
    fracTable[which(fracTable < 0.1)] <- 0

    ## sanity checks on input data
    .validateInputs(fracTable, parents, cloneLabels)

    ## Therapy effect estimation
    if (!is.null(therapyEffect)) {
        fracTable <- .getTherapyEffect(fracTable = fracTable,
                                        timepoints = timepoints,
                                        parents = parents,
                                        therapyEffect = therapyEffect)

        timepoints <- as.numeric(colnames(fracTable))
    }


    ## Estimate new timepoints
    if (timepointInterpolation) {
        fracTable <- .estimateTimepoints(nestLevels = nestLevels,
                                        fracTable = fracTable,
                                        parents = parents,
                                        timepoints = timepoints,
                                        originTimepoint = originTimepoint,
                                        therapyEffect = therapyEffect)
        timepoints <- as.numeric(colnames(fracTable))
    }

    ## Recalculate timepoints if both (new timepoints,therapy effect) are
    ## estimated
    if (!is.null(therapyEffect) & timepointInterpolation) {
        timepoints <- .recalculateTimepoints(therapyEffect, timepoints)
        colnames(fracTable) <- timepoints
    }




    ## Change the name of the clones to numbers (in fracTable)
    rownames(fracTable) <- seq_len(dim(fracTable)[1])



    ## sanity checks on calculated data
    .validateInputs(fracTable, parents, cloneLabels)

    ## create the object
    seaObject <- new("seaObject", ytop = list(), ybtm = list(), col = c("NULL"),
                    timepoints = as.numeric(colnames(fracTable)),
                    fracTable = fracTable, parents = parents,
                    nestLevels = nestLevels, cloneLabels = cloneLabels,
                    defaultLabels = defaultLabels,
                    originTimepoint = originTimepoint
    )

    ## vector with each clones family
    seaObject <- .cloneFamVec(seaObject)

    # set default colors to start
    seaObject <- .setColors(seaObject, col = col)


    return(seaObject)
}
