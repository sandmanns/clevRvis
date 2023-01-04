exploreTrees <- function(fracTable, timepoints){

    if(!is.matrix(fracTable) || !is.numeric(fracTable)){
        stop("No valid input for fracTable provided.")
    }
    if(!is.vector(timepoints) || !is.numeric(timepoints)){
        stop("No valid input for timepoints provided.")
    }
    #get matrix (TRUE/FALSE) indicating each clone's possible parents
    possible <- .possibleParents(fracTable)

    #when only one option for parent -> check where it sums up to >100
    possibilities <- .checkPossibleParents(fracTable, possible)

    #final validity check
    #is number of options to consider <20,000?
    solutions <- .getParentOptions(possibilities, fracTable, timepoints)

    return(solutions)
}
