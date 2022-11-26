methods::setClass("seaObject",
    representation(
        ytop = "list",
        ybtm = "list",
        xpos = "list",
        col = "character",
        timepoints = "numeric",
        fracTable = "matrix",
        parents = "numeric",
        nestLevels = "numeric",
        cloneFamily = "numeric",
        cloneLabels = "character",
        defaultLabels = "logical",
        originTimepoint = "numeric"
    )
)

#' Initialize the seaObject class
#'
#' @return no return value
#'
#.initSeaObjectClass <- function() {
#    methods::setClass(
#        "seaObject",
#        representation(
#            ytop = "list",
#            ybtm = "list",
#            xpos = "list",
#            col = "character",
#            timepoints = "numeric",
#            fracTable = "matrix",
#            parents = "numeric",
#            nestLevels = "numeric",
#            cloneFamily = "numeric",
#            cloneLabels = "character",
#            defaultLabels = "logical",
#            originTimepoint = "numeric"
#        )
#    )
#}

#' Validate some key assumptions about the seaObject's data
#'
#' @param fracTable A numeric matrix containing tumor fraction estimates for
#' all clones at all timepoints
#' @param parents An integer vector specifying parental relationships between
#' clones
#' @param cloneLabels A character vector of names to assign to each clone when
#' plotting a legend
#'
#' @return No return value - stops execution with an error if invalid inputs
#' are detected
#'

.validateInputs <- function(fracTable, parents, cloneLabels) {
    clones <-    seq_len(dim(fracTable)[1])
    timepts <- seq_len(dim(fracTable)[2])

    ##no cluster can go from present to absent and then back
    ##difference in clusters cannot go from present to absent and then back
    for (clone in clones) {
        startedClone <- FALSE
        endedClone <- FALSE
        startedDiff <- FALSE
        endedDiff <- FALSE

        for (timept in timepts) {
            ##check individual clones
            if (fracTable[clone, timept] > 0.000001) {
                if (startedClone & endedClone) {
                    stop(
                        paste(
                            "Clone", clone, "goes from present to absent
                            (fraction=0) and then back to present."
                        )
                    )
                }
                startedClone <- TRUE
            } else {
                if (startedClone) {
                    endedClone <- TRUE
                }
            }

            ##check difference in clusters
            diffr <-
                fracTable[clone, timept] -
                    sum(fracTable[which(parents == clone), timept])
            if (diffr > 0.0001) {
                if (startedDiff & endedDiff) {
                    stop(
                        paste(
                            "The difference between clone ",
                            clone,
                            "and its subclones
                            goes from present to absent (difference = 0) and
                            then back to present at timepoint",
                            timept,
                            ". Subclones can't have the same CCF as the parent
                                        (difference = 0) and then have less
                                        CCF again."
                        )
                    )
                }
                startedDiff <- TRUE
            } else {
                if (startedDiff) {
                    endedDiff <- TRUE
                }
            }
        }
    }

    ##clusters of entirely zero get a warning
    if (length(which(rowSums(fracTable) == 0)) > 0) {
        warning(
            "WARNING: at least one cluster has fraction zero at all timepoints.
                        It will not be displayed"
        )
    }

    ##make sure that each timepoint doesn't sum to more than the parental value
    #at a given nest level (or 100% for level 0)
    for (timept in timepts) {
        independentClones <- which(parents == 0)
        if (sum(fracTable[independentClones, timept]) > 100.000001) {
            stop(
                paste(
                    "clones with same nest level cannot have values that sum to
                    more than 100%: Problem is in clusters ",
                    paste(independentClones, collapse = ",")
                )
            )
        }

        for (i in unique(parents)) {
            if (i > 0) {
                neighbors <- which(parents == i)
                if (sum(fracTable[neighbors, timept]) > fracTable[parents[neighbors[1]],timept] + 0.000001) {
                    stop(
                        paste(
                            "clones with same parent cannot have values that
                            sum tomore than the percentage of the parent:
                            Problem is in clusters ", paste(neighbors,
                            collapse = ","), "at timepoint", timept
                        )
                    )
                }
            }
        }
    }

    ##number of clone labels is equal to the number of clones
    if (length(cloneLabels) != nrow(fracTable)) {
        stop("The number of cloneLabels provided must be equal to the
            number of clones")
    }

}


#' Fill the col slot of the seaObject
#'
#' @param seaObject A seaObject
#' @param col (optional) A vector with each clone's color
#'
#' @return A seaObject with the col slot filled

.setColors <- function(seaObject, col = NULL) {
    nclones <- nrow(seaObject@fracTable)
    if (!(exists("nclones"))) {
        warning(
            "WARNING: Could not set colors, as the number of rows in the
            fracTable slot of the seaObject could not be calculated"
        )
        return(seaObject)
    }

    if (is.null(col)) {
        ##use default color scheme
        col <- .getRelatedColors(seaObject)

        seaObject@col <- col[seq_len(nclones)]
        return(seaObject)
    }

    ##else colors provided, check them for sanity
    if (length(col) != nrow(seaObject@fracTable)) {
        stop(
            paste(
                "ERROR: number of colors provided must be equal to the number
                of clones (", nclones, ")",
                sep = ""
            )
        )
    }
    seaObject@col <- col
    return(seaObject)
}


#' Given an integer indicating the length of the longest branch, return a color
#' matrix with as many rows as the integer given
#'
#' @param lenBranch An integer vector specifying the length of the longest
#' branch in the clonal evolution tree
#'
#' @return A matrix of dimensions 25 columns (hue changes) and lenBranch rows
#' (darkness changes)
.getColorMatrix <- function(lenBranch) {
    ordr <- c(seq(18, 25), seq(1, 17))
    rbw <- rainbow_hcl(25, l = 85)[ordr]
    allColors <- c()
    for (col in rbw) {
        darkCol <- darken(col, 0.7)
        darkColors <- colorRampPalette(c(col, darkCol))(lenBranch)
        allColors <- c(allColors, darkColors)
    }
    colMatrix <- matrix(allColors, ncol = length(rbw), byrow = FALSE)

    return(colMatrix)
}



#' Given a seaObject, define the color of each clone taking into account the
#' phylogeny.
#'
#' @param seaObject A seaObject
#'
#' @return A character vector with each clones color
.getRelatedColors <- function(seaObject) {
    parents <- NULL

    lenBranch <- max(seaObject@nestLevels) + 1
    colorMatrix <- .getColorMatrix(lenBranch = lenBranch)
    nclones <- length(seaObject@cloneLabels)
    cloneCols <- rep(NA, nclones)
    parentClone <- -1
    parentX <- c(1, rep(NA, nclones))
    parentY <- c(0, rep(NA, nclones))
    cloneBranches <- .getAllBranches(seaObject@parents)

    ## first stablish the color for the initial clones (parent = 0)
    indepClones <- which(seaObject@parents == 0)
    if (length(indepClones) > 1) {
        step <-
            25 %/% length(indepClones) #as far away as possible in the matrix
        x <- 0
        for (clone in indepClones) {
            x <- x + step
            y <- 1
            if (x > 25) {
                x <- x %/% 25
            }
            cloneCols[clone] <- colorMatrix[y, x]
            parentX[clone + 1] <- x
            parentY[clone + 1] <- y
        }
        ##get which clones where not initial
        branchedClones <-
            seq_len(nclones)[-which(seq_len(nclones) %in% indepClones)]
    } else{
        branchedClones <- seq_len(nclones)
    }

    ##Deal with the non-initial clones
    for (clone in branchedClones) {
        parent <- seaObject@parents[clone]
        ##color change depends on the number of branches
        if (sum(seaObject@parents == parent) > 4) {
            branchStep <- 0
        } else {
            branchStep <- 2
        }
        ## If it's a branch (parent already in the vector)
        if (parent %in% parentClone) {
            counts <- sum(parentClone == parent) + branchStep
            modX <- FALSE
            ## alternate between the two sides of the matrix to variate the
            ## color, darken the color
            if (counts %% 2 == 1) {
                x <- parentX[parent + 1] + ((counts + 1) / 2)

                if (x %in% parentX) {
                    if (sum((which(parentX == x) - 1) %in% cloneBranches[[clone]]) == 0) {
                        parentsInBranch <-
                            which(names(table(parents)) %in% cloneBranches[[clone]])
                        counts <- counts +
                            sum(table(parents)[parentsInBranch] > 1)
                        if (counts %% 2 == 1) {
                            x <- parentX[parent + 1] +
                                ((counts + 1) / 2) + branchStep
                        } else{
                            x <- parentX[parent + 1] - (counts / 2) -
                                branchStep
                        }
                        modX <- TRUE
                    }
                }
            } else if (counts %% 2 == 0 & !modX) {
                x <- parentX[parent + 1] - (counts / 2)
                if (x %in% parentX) {
                    if (sum((which(parentX == x) - 1) %in% cloneBranches[[clone]]) == 0) {
                        parentsInBranch <-
                            which(names(table(parents)) %in% cloneBranches[[clone]])
                        counts <-
                            counts + sum(table(parents)[parentsInBranch] > 1)
                        x <- parentX[parent + 1] + (counts / 2)
                        modX <- TRUE
                    }
                }
            }
            ##For linear evolution (parent not in the vector) --> darken the
            ##color
        } else {
            x <- parentX[parent + 1]
        }
        y <- parentY[parent + 1] + 1

        ##Fix x and y values to be inside the matrix
        if (x > 25) {
            x <- x %/% 25
        }
        if (y > lenBranch) {
            y <- y %/% lenBranch
        }
        if (x == 0) {
            x <- 25
        }
        if (x < 0) {
            x <- 25 + x
        }


        ##check another clone doesn't have the same color
        repColor <- colorMatrix[y, x] %in% cloneCols
        if (repColor) {
            while (repColor) {
                if (x < 12) {
                    x <- x + 1 + branchStep
                } else {
                    x <- x - 1 - branchStep
                }

                repColor <- colorMatrix[y, x] %in% cloneCols
            }
        }

        cloneCols[clone] <- colorMatrix[y, x] ##Add color to the vector
        parentClone <-
            c(parentClone, seaObject@parents[clone]) #update parents vec
        parentX[clone + 1] <-
            x #update parents vectors with the clone x and y positions
        parentY[clone + 1] <- y
    }
    return(cloneCols)
}

#' Given the a list representing the parents of each clone, and the number
#' specifying which clone to test, returns how deeply it is nested
#'
#' @param parents An integer vector specifying parental relationships between
#' clones
#' @param x The integer specifying which subclone to calculate nest level for
#'
#' @return An integer representing how deeply this subclone is nested
#' @seealso .getAllNestLevels
.getNestLevel <- function(parents, x) {
    #sanity checks
    if (x > length(parents)) {
        stop(
            paste(
                "cannot have a parent that does not exist in list. parent =",
                x, ", length(parents) =", length(parents)
            )
        )
    }
    if (x < 0) {
        stop("cannot have a value in parents of less than zero")
    }

    if (parents[x] == 0) {
        return(0)
    } else {
        return(.getNestLevel(parents, parents[x]) + 1)
    }
}



#' Given the a list representing the parents of each clone, return a vector
#' specifying how deeply each clone is nested
#'
#' @param parents An integer vector specifying parental relationships between
#' clones
#'
#' @return An integer vector representing how deeply each subclone is nested
#' @seealso .getNestLevel
#'
.getAllNestLevels <- function(parents) {
    nest.level <- c()
    for (i in seq_len(length(parents))) {
        nest.level <- c(nest.level, .getNestLevel(parents, i))
    }
    return(nest.level)
}



#' Given a seaObject, return the seaObject with the cloneFamily slot filled
#' with a numeric vector specifying which family each clone belongs to
#'
#' @param seaObject A seaObject
#'
#' @return A seaObject with the cloneFamily slot filled
#'
.cloneFamVec <- function(seaObject) {
    numFamilies <- sum(seaObject@parents == 0)
    listFams <- tail(.getFromTo(seaObject), numFamilies)
    cloneFam <- c()
    for (i in seq_len(length(seaObject@parents))) {
        for (j in seq_len(numFamilies)) {
            if (i %in% as.numeric(listFams[[j]])) {
                cloneFam <- c(cloneFam, j)
            }
        }
    }

    seaObject@cloneFamily <- cloneFam
    return(seaObject)
}



#'Given a cancer cell fraction table as a matrix,
#'return a matrix with (TRUE/FALSE) indicating each clone's possible parents
#'
#' @param fracTable a matrix with the cancer cell fraction of each clone at
#' each timepoint
#'
#' @return matrix with (TRUE/FALSE) indicating each clone's possible parents
#'
.possibleParents <- function(fracTable){
    ##limit options per position
    possible <- matrix(rep(TRUE, (nrow(fracTable)*nrow(fracTable))),
                    nrow = nrow(fracTable))
    row.names(possible) <- c(0:(nrow(fracTable)-1))

    fracTable[nrow(fracTable),] <- NA

    for(i in seq_len(ncol(fracTable))){
        while(sum(!is.na(fracTable[,i]))>1){
            small <- which.min(fracTable[,i])
            impossible <- which(fracTable[,i]>fracTable[small,i])
            for(j in impossible){
                possible[small+1,j] <- FALSE
            }
            fracTable[which.min(fracTable[,i]),i] <- NA
        }
    }
    for(i in seq_len(ncol(possible)-1)){
        possible[i+1,i] <- FALSE
    }

    return(possible)
}


#'validate when only one option for parent that it does not sum up to >100
#'
#' @param fracTable a matrix with the cancer cell fraction of each clone at
#' each timepoint
#' @param possible matrix with (TRUE/FALSE) indicating each clone's possible parents
#'
#' @return matrix with (TRUE/FALSE) indicating each clone's validated possible
#' parents
#'
.checkPossibleParents <- function(fracTable, possible){

    check<-which(colSums(possible)==1)
    checked<-c()
    while(!identical(check,checked)){
        for(i in setdiff(check,checked)){
            for(j in seq_len(ncol(fracTable))){
                compare_with<-fracTable[i,j]
                for(k in c(seq_len(length(fracTable[,j])))[-i]){
                    if((compare_with+fracTable[k,j])>100){
                        possible[which(possible[,i]==TRUE),k]<-FALSE
                    }
                }
            }
            checked<-c(checked,i)
        }
        check<-which(colSums(possible)==1)
    }

    possibilities<-list()
    for(i in seq_len(ncol(possible))){
        possibilities[[i]]<-names(possible[possible[,i]==TRUE,i])
    }
    return(possibilities)
}


#'Get the possible parental relations
#'
#' @param possibilities matrix with (TRUE/FALSE) indicating each clone's
#' validated possible parents
#' @param fracTable a matrix with the cancer cell fraction of each clone at
#' each timepoint
#' @param timepoints numeric vector with time points
#'
#' @return list of numeric vectors corresponding to the possible parental
#' relations
#'
.getParentOptions <- function(possibilities, fracTable, timepoints){
    temp<-tryCatch({
        combinations<-expand.grid(possibilities)
    }, error=function(e){
        return(NULL)
    })

    solutions <- list()
    found <- 1
    if(!is.null(temp)){
        if(nrow(combinations) < 20000){
            if(ncol(fracTable) == 1){

                for(i in seq_len(nrow(combinations))){
                    temp <- tryCatch({
                        x <- createSeaObject(
                        fracTable,
                        parents = as.numeric(t(as.matrix(combinations[i,]))),
                        timepoints,
                        timepointInterpolation = TRUE,
                        originTimepoint = -1)
                    }, error=function(e){
                        return(NULL)
                    })

                    if(!is.null(temp)){
                        solutions[[found]] <- as.numeric(t(as.matrix(combinations[i,])))
                        found <- found+1
                    }
                }
            }else{
                for(i in seq_len(nrow(combinations))){
                    temp <- tryCatch({
                        x <- createSeaObject(
                        fracTable,
                        parents = as.numeric(t(as.matrix(combinations[i,]))),
                        timepoints,
                        timepointInterpolation = TRUE)
                    }, error=function(e){
                        return(NULL)
                    })

                    if(!is.null(temp)){
                        solutions[[found]] <- as.numeric(t(as.matrix(combinations[i,])))
                        found <- found+1
                    }
                }
            }
            ##name vectors
            names(solutions) <- unlist(lapply(solutions,
                                        function(x) paste(x, collapse = ',')))

            return(solutions)
        }else{
            message("Too many possible options.")
        }
    }else{
        message("Too many possible options.")
    }
}





#### ESTIMATE TIMEPOINTS #######



#' get a list with clones present at a certain timepoint divided by families.
#'
#' @param parents A vector indicating the parental relations
#' @param fracTable Vector with CCFs of clones at a timepoint
#'
#' @return list with clones divided by families, first half of the list is
#' parent clones, second half are children clones.
#'
.getListFamilies <- function(parents, fracTable, listFam = NULL) {
    started <- FALSE #flag
    fam_n <- 1
    numFam <- sum(parents == 0) #number of expected families
    fam_v <- c()
    if (numFam == 1) {
        listFam <- which(fracTable != 0)
    }
    else{
        while (fam_n < numFam) {
            #which clones are present in the timepoint
            present <- which(fracTable != 0)
            for (clone in present) {
                parent <- parents[clone]

                #add vector to the list
                if (parent == 0 & started) {
                    listFam[[fam_n]] <- fam_v
                    fam_n <- fam_n + 1
                    fam_v <- c()
                    started <- FALSE
                }

                #start the vector
                if (parent == 0 & !(started)) {
                    started <- TRUE
                    fam_v <- c(fam_v, clone)
                }

                #Add clones to family vector while the lists are empty
                if (parent != 0 & started & length(listFam) == 0) {
                    fam_v <- c(fam_v, clone)
                }

                #if lists are not empty
                if (parent != 0 & started & length(listFam) > 0) {
                    ##idx is the position of the family where the clone belongs
                    idx <- detect_index(listFam, ~ parent %in% .)

                    #check the list to see if parent is there
                    if (idx != 0) {
                        listFam[[idx]] <- c(listFam[[idx]], clone)
                    }

                    if (idx == 0) {
                        fam_v <- c(fam_v, clone)
                    }

                }

            }

            #when we are done iterating add the remainings to the lists
            listFam[[fam_n]] <- fam_v
            fam_n <- fam_n + 1
        }
    }

    return(listFam)
}

#' given the first and last value and the length get a linear sequence vector
#'
#' @param first first value of the vector
#' @param last last value of the vector
#' @param length desired length of the vector (including given first and last
#' value)
#'
#' @return Numeric vector of length 'length' with linear sequence starting with
#' 'first' value and finishing with 'last'.
#' @seealso .interpolate
#' @examples
#' \donttest{
#' .linearseq(1,5,5)
#' .linearseq(90,10,3)
#' .linearseq(7,7,4)
#' }
.linearseq <- function(first, last, length) {
    # Compute step
    if (last > first) {
        c <- (last - first) / (length - 1)
        return(seq(first, last, c))
    } else if (last < first) {
        c <- (first - last) / (length - 1)
        return(rev(seq(last, first, c)))
    } else if (last == first) {
        return(rep(first, length))
    }
}


#' given a vector with NA values, fill in the empty slots so that, with the
#' filled slots it forms a linear sequence
#'
#' @param lt numeric vector with NA values
#'
#' @return Numeric vector with all NA slots filled
#' @seealso .linearseq
#' @examples
#' \donttest{
#' .interpolate(c(1,NA,NA,4,NA,8))
#' }
.interpolate <- function(lt) {
    # Find missing elements
    idx <- 1
    while (idx <= length(lt)) {
        if (is.na(lt[idx])) {
            # Found a gap (find the end of the missing sequence)
            begin_idx <- idx - 1
            end_idx <- idx + 1
            while (end_idx <= length(lt) & is.na(lt[end_idx])) {
                end_idx <- end_idx + 1
            }
            # Fill the gap with a linear sequence
            lt[begin_idx:end_idx] <-
                .linearseq(lt[begin_idx], lt[end_idx], end_idx - begin_idx + 1)
            # Update index
            idx <- end_idx + 1
        }
        else{
            idx <- idx + 1
        }

    }
    return(lt)
}


#' get the estimated CCFs and timepoints before the first measured timepoint
#'
#' @param listFam list of the present clones in the first measured timepoint
#' divided by families
#' @param originPoint A numeric value specifying the estimated timepoint where
#' the first clone emerged.
#' @param nestLevels An integer vector specifying how deeply a given clone is
#' nested in the overall hierarchy
#' @param nclones An integer value specifying the total number of clones
#' @param fracTable A numeric matrix containing tumor fraction estimates for
#' all clones at all timepoints
#' @param parents An integer vector specifying parental relationships between
#' clones
#'
#'
#' @return A list with the new estimated CCFs matrix and a vector with the new
#' estimated timepoints
#' @seealso .estimateTimepoints

.originTimepoints <-
    function(listFam,
            originPoint,
            nestLevels,
            nclones,
            fracTable,
            parents,
            timepoints) {
        newCCFtable <- data.frame(id = c(seq_len(nclones)))
        n <- unlist(listFam) #present clones in tp as vector
        n_l <- nestLevels[n] #nest levels of present clones
        numNewTp <-
            length(unique(n_l)) #number of new timepoints (before tp1)
        ##Vector with new estimated timepoints values
        newTimepoints <-
            .interpolate(c(originPoint, rep(NA, numNewTp - 1), timepoints[1]))

        ##k = {1,..,numNewTp} -> first estimated timepoint = originPoint,
        ##first clones with nestLevel = 0
        for (k in seq_len(numNewTp)) {
            newCCF <- c() #vector with new ccf
            for (l in seq_len(nclones)) {
                ##Case when: present clone, nest level is 0 and first new
                ##estimated tp
                if ((l %in% n) & nestLevels[l] == 0 & ncol(newCCFtable) == 1) {
                    newCCF <- c(newCCF, 0.1)
                }

                ##Case when: present clone and not first estimated tp
                else if ((l %in% n)    & ncol(newCCFtable) > 1) {
                    ## Clone wasn't present before
                    if (newCCFtable[l, ncol(newCCFtable)] == 0 &
                            nestLevels[l] == k - 1) {
                        newCCF <- c(newCCF, 0.1)
                    }
                    ##Clone was present before
                    else if (newCCFtable[l, ncol(newCCFtable)] > 0 |
                                    is.na(newCCFtable[l, ncol(newCCFtable)])) {
                        newCCF <- c(newCCF, NA)
                    }
                    ##Not its timepoint to emerge
                    else if (newCCFtable[l, ncol(newCCFtable)] == 0 &
                                    !(nestLevels[l] == k - 1)) {
                        newCCF <- c(newCCF, 0.0)
                    }
                }
                ##Case when: clone is not present
                else {
                    newCCF <- c(newCCF, 0.0)
                }

            }
            #Add the ccf calculated to the new fracTable
            newCCFtable <- newCCFtable %>% add_column(newCCF)

        }
        firstTpCCFs <- fracTable[, 1]
        ##calculate difference in ccfs
        absCCFs <- c()
        for (clone in seq_len(length(firstTpCCFs))) {
            children <- which(parents == clone)
            cloneCCF <- firstTpCCFs[clone] - sum(firstTpCCFs[children])
            absCCFs <- c(absCCFs, cloneCCF)
        }
        ##Add children's ccf to parent
        for (col in seq(2, ncol(newCCFtable))) {
            replaceCCF <- which(is.na(newCCFtable[, col]))
            for (clone in rev(replaceCCF)) {
                children <- which(parents == clone)
                newCCFtable[clone, col] <-
                    absCCFs[clone] + sum(newCCFtable[children, col])
            }
        }

        return(list(newccf = newCCFtable,
                                newtp = newTimepoints[-length(newTimepoints)]))
    }





#' get the estimated CCFs between two measured timepoints
#'
#' @param nclones An integer value specifying the total number of clones
#' @param newCCFtable A data frame with the previous estimated CCFs.
#' @param newClones A numeric vector specifying which of the present clones are
#' new in this measured timepoint
#' @param whichClones A numeric vector specifying which of the new clones
#* should
#' emerge in this estimated timepoint
#'
#'
#' @return A vector with the new estimated CCFs
#' @seealso .estimateTimepoints

.getNewCCFvector <-
    function(nclones, newCCFtable, newClones, whichClones) {
        newCCF <- c()
        for (clone in seq_len(nclones)) {
            #if it is already present
            if (newCCFtable[clone, ncol(newCCFtable)] > 0 |
                    is.na(newCCFtable[clone, ncol(newCCFtable)])) {
                newCCF <- c(newCCF, NA)
            }
            #if it is not present and should emerge
            else if (newCCFtable[clone, ncol(newCCFtable)] == 0 &
                            clone %in% newClones[whichClones]) {
                newCCF <- c(newCCF, 0.1)
            }
            #if it's not present and should remain not present
            else {
                newCCF <- c(newCCF, 0)
            }
        }
        return(newCCF)
    }



#' Get a new CCFs table with the estimated and measured CCFs
#'
#' @param nestLevels An integer vector specifying how deeply a given clone is
#' nested in the overall hierarchy
#' @param fracTable A numeric matrix containing tumor fraction estimates for
#' all clones at all timepoints
#' @param originPoint A numeric value specifying the estimated timepoint where
#' the first clone emerged
#' @param parents An integer vector specifying parental relationships between
#' clones
#' @param timepoints A numeric vector specifying the measured timepoints for
#' each column of the matrix
#'
#'
#' @return A matrix with the estimated and measured CCFs (and Timepoints as
#' column names)
#' @examples
#' \donttest {
#' .estimateTimepoints(nestLevels = c(0,1,2),
#'                     fracTable = matrix(c(20,10,0,50,30,25),ncol=2),
#' originTimepoint = -30, parents = c(0,1,2), timepoints = c(0,50))
#' }
.estimateTimepoints <-
    function(nestLevels,
                    fracTable,
                    originTimepoint,
                    parents,
                    timepoints,
                    therapyEffect = NULL) {
        nclones <- nrow(fracTable)
        presentClones <- c()
        cloneBranches <- .getAllBranches(parents)

        for (tp in seq_len(length(timepoints))) {
            prevClones <- presentClones
            listFam <- .getListFamilies(parents, fracTable[, tp])
            presentClones <- unlist(listFam)

            newClones <- presentClones[!presentClones %in% prevClones]
            #nestLevelsNewClones <- nestLevels[newClones]
            nestLevelsNewClones <- rep(NA, length(newClones))
            for (clone in seq_len(length(newClones))) {
                parentClone <- parents[newClones[clone]]
                if (!(parentClone %in% newClones)) {
                    nestLevelsNewClones[clone] <- 0
                } else {
                    parentNestLevel <-
                        nestLevelsNewClones[which(newClones == parentClone)]
                    nestLevelsNewClones[clone] <- parentNestLevel + 1
                }
            }
            numNewTp <- length(unique(nestLevelsNewClones))

            ##estimated timepoints before first measured tp
            if (tp == 1) {
                newValues <-
                    .originTimepoints(
                        listFam,
                        originPoint = originTimepoint,
                        nestLevels,
                        nclones,
                        fracTable,
                        parents = parents,
                        timepoints = timepoints
                    )
                newTimepoints <- newValues$newtp
                newCCFtable <- newValues$newccf

            } else{
                #for the rest of timepoints

                for (newTp in seq_len(numNewTp)) {
                    ##which clones (index) are the next to be added
                    whichClones <-
                        which(nestLevelsNewClones == min(nestLevelsNewClones))

                    ##the first new timepoint will be to modify the measured
                    ##one
                    if (newTp == 1) {
                        firstClones <- newClones[whichClones]
                        newCCFtable[firstClones, ncol(newCCFtable)] <- 0.1

                        ##Check that adding 0.1 to a child clone doesn't add to
                        ##more than parent clone overall

                for (clone in firstClones) {
                    for (inClone in rev(cloneBranches[[clone]])[seq_len(length(cloneBranches[[clone]]) - 1)]) {
                        if (parents[inClone] > 0) {
                            neighbours <- which(parents == parents[inClone])
                            neighboursCCF <-
                                sum(newCCFtable[neighbours, ncol(newCCFtable)])
                            parentCCF <-
                                newCCFtable[parents[inClone], ncol(newCCFtable)]
                            parentDiffCCF <- parentCCF - neighboursCCF
                            ##If it does increase 0.1 to parent
                            if (parentDiffCCF < 0) {
                                newCCFtable[parents[inClone], ncol(newCCFtable)] <-
                                    newCCFtable[parents[inClone], ncol(newCCFtable)] + 0.1
                            }
                        }
                    }
                }

                    } else{
                        ##for the rest of new timepoints loop through all
                        ##the clones
                        ##Estimate CCF vector
                        newCCF <- .getNewCCFvector(nclones, newCCFtable,
                                                    newClones, whichClones)

                        ##add the new column to the table
                        newCCFtable <- cbind(newCCFtable, newCCF)
                    }

                    ##update the list of clones to emerge
                    newClones <- newClones[-whichClones]
                    nestLevelsNewClones <-
                        nestLevelsNewClones[!nestLevelsNewClones == min(nestLevelsNewClones)]
                }

                ##Compute the new estimated timepoints values
                if (tp <= length(timepoints) & numNewTp > 1) {
                    btwTimepoints <- .linearseq(timepoints[tp - 1],
                                                timepoints[tp], numNewTp + 1)
                    newTimepoints <- c(newTimepoints,
                                        btwTimepoints[2:(length(btwTimepoints) - 1)])
                }

            }

            newCCFtable <- cbind(newCCFtable, fracTable[, tp])
            newTimepoints <- c(newTimepoints, timepoints[tp])

            if (tp > 1 & !(is.null(therapyEffect)) & numNewTp > 0) {
                ##therapyEffect is a vector with two timepoints
                if (length(therapyEffect) == 2) {
                    tpBefore <- therapyEffect[1]
                    tpAfter <- therapyEffect[2]
                    therapyEffectTP <- (tpBefore + tpAfter) / 2
                } else if (length(therapyEffect) == 1) {
                    therapyEffectTP <- therapyEffect
                    tpAfter <-
                        timepoints[which(timepoints == therapyEffectTP) + 1]
                }

                if (therapyEffectTP == timepoints[tp - 1] &
                        tpAfter == timepoints[tp]) {
                    newCCFtable <-
                        .getIntmCCF(nclones, parents, newCCFtable,
                                    fracTable, tp)
                } else{
                    newCCFtable <- t(apply(newCCFtable, 1, .interpolate))
                }
            } else{
                ##Fill the new fracTable with the intermediate values
                newCCFtable <- t(apply(newCCFtable, 1, .interpolate))
            }
        }

        newTimepoints <-    round(newTimepoints, 2)

        newCCFtable <- round(newCCFtable[, 2:ncol(newCCFtable)], 2)
        if (is.null(nrow(newCCFtable))) {
            newCCFtable <- matrix(newCCFtable, ncol = length(newCCFtable))
        }
        ##timepoints as column names
        colnames(newCCFtable) <- newTimepoints

        return(newCCFtable)
    }

.getIntmCCF <- function(nclones, parents, newCCFtable, fracTable, tp) {
        ##Fill in the NAs
        lastClones <- which(!(seq_len(nclones) %in% parents))
        ##Last clones assumed to evolve linear way
        if (length(lastClones) > 1) {
            CCFsLastClones <-
                t(apply(newCCFtable[lastClones, 2:ncol(newCCFtable)], 1,
                        .interpolate))
        } else {
            CCFsLastClones <-
                .interpolate(newCCFtable[lastClones, 2:ncol(newCCFtable)])
        }

        newCCFtable[lastClones, 2:ncol(newCCFtable)] <- CCFsLastClones


        ##calculate difference in ccfs
        absCCFs <- c()
        for (clone in seq_len(nclones)) {
            children <- which(parents == clone)
            cloneCCF <- fracTable[clone, tp] - sum(fracTable[children, tp])
            absCCFs <- c(absCCFs, cloneCCF)
        }
        ##Add children's ccf to parent
        for (col in seq(2, ncol(newCCFtable))) {
            replaceCCF <- which(is.na(newCCFtable[, col]))
            for (clone in rev(replaceCCF)) {
                children <- which(parents == clone)
                newCCFvalue <- absCCFs[clone] +
                                sum(newCCFtable[children, col])
                ##Keep the ccf if the estimated is 0 and the clone is present
                ##before and after
                if (newCCFvalue == 0 &
                    fracTable[clone, tp - 1] > 0 &
                    fracTable[clone, tp] > 0) {
                    newCCFtable[clone, col] <- fracTable[clone, tp - 1]
                } else {
                    newCCFtable[clone, col] <- newCCFvalue
                }
            }
        }

        return(newCCFtable)
    }

#### THERAPY EFFECT ######

#' Given a specific timepoint for estimating the therapy effect, get the before
#' and after measured timepoints
#'
#' @param therapyEffect A numeric value specifying the timepoint where to
#' calculate the estimated therapy effect
#' @param timepoints A numeric vector specifying the measured timepoints
#'
#' @return A list with two values: tpBefore (timepoint before) and tpAfter
#' (timepoint after)
#' @seealso .getTherapyEffect
.getTpBeforeAfter <- function(therapyEffect, timepoints) {
    i <- 1

    while (therapyEffect > timepoints[i]) {
        tpBefore <- timepoints[i]
        i <- i + 1
    }
    tpAfter <- timepoints[i]

    return(list(tpBefore = tpBefore, tpAfter = tpAfter))
}



#' Get which clones where present before therapy effect
#'
#' @param tpBefore A numeric value specifying the measured timepoint just
#' before the estimated therapy effect
#' @param fracTable A numeric matrix containing tumor fraction estimates for
#' all clones at all timepoints
#' @param timepoints A numeric vector specifying the measured timepoints
#'
#' @return A numeric vector with the indexes of the clones present before
#' therapy
#' @seealso .getTherapyEffect
.getClonesPresent <- function(tpBefore, fracTable, timepoints) {
    tp <- which(timepoints == tpBefore)

    return(which(fracTable[, tp] > 0))
}



#' Get a vector with the estimated therapy effect CCFs
#'
#' @param tpBefore A numeric value specifying the measured timepoint just
#' before the estimated therapy effect
#' @param tpAfter A numeric value specifying the measured timepoint just after
#' the estimated therapy effect
#' @param fracTable A numeric matrix containing tumor fraction estimates for
#' all clones at all timepoints
#' @param parents A numeric vector specifying the parental relations between
#' clones
#' @param presentClones An integer vector specifying the clones present during
#' therapy effect
#'
#' @return A numeric vector with the estimated therapy effect CCFs
#' @seealso .getTherapyEffect
.getTherapyCCF <- function(tpBefore, tpAfter,fracTable, parents,
                            presentClones, timepoints) {
        newCCF_vec <- c()
        indBefore <- which(timepoints == tpBefore)
        indAfter <- which(timepoints == tpAfter)
        checkMinTps <- c(indBefore, indAfter)
        ##Get CCF for clones present
        for (clone in seq_len(nrow(fracTable))) {
            newMinCCFs <- c()
            if (clone %in% presentClones) {
                for (tp in checkMinTps) {
                    children <- which(parents == clone)
                    CCFchildren <- sum(fracTable[children, tp])
                    newCCF <- fracTable[clone, tp] - CCFchildren
                    newMinCCFs <- c(newMinCCFs, newCCF)
                }
                newCCF_vec <- c(newCCF_vec, min(newMinCCFs))
            } else{
                newCCF_vec <- c(newCCF_vec, 0)
            }
        }
        #Update CCF for plotting (add present children ccf)
        for (clone in rev(seq_len(nrow(fracTable)))) {
            children <- which(parents == clone)
            newCCF_vec[clone] <-
                newCCF_vec[clone] + sum(newCCF_vec[children])
        }

        for (clone in presentClones) {
            if (newCCF_vec[clone] == 0 & fracTable[clone, indAfter] > 0) {
                newCCF_vec[clone] <- 0.2
            }
        }

        return(newCCF_vec)
    }



#' Get a vector with the estimated therapy effect CCFs
#'
#' @param fracTable A numeric matrix containing tumor fraction estimates for
#' all clones at all timepoints
#' @param timepoints A numeric vector specifying the measured timepoints
#' @param parents A numeric vector specifying the parental relations between
#' clones
#' @param therapyEffect A numeric value specifiying the timepoint on which to
#' estimate the therapy effect or a numeric vector with length=2 specifying
#' the consecutive measured timepoints between which to estimate
#' the therapy effect.
#'
#' @return A matrix with the estimated therapy effect CCFs and the measured
#' ones.
#' With timepoints as colnames.
#' @examples
#' \dontrun{
#' .getTherapyEffect(fracTable = matrix(c(90,10,0,47,35,30), ncol = 2),
#' timepoints = c(0,50), parents = c(0,1,2), therapyEffect = 25)
#' }
.getTherapyEffect <- function(fracTable, timepoints, parents, therapyEffect) {
        ##therapyEffect is a vector with two timepoints
        if (length(therapyEffect) == 2) {
            tpBefore <- therapyEffect[1]
            tpAfter <- therapyEffect[2]
            if (!(tpBefore %in% timepoints)) {
                stop(
                    paste(
                        'The first timepoint provided in therapyEffect is not a
                        valid timepoint.', tpBefore,
                        'is not a measured timepoint'
                    )
                )
            } else if (!(tpAfter %in% timepoints)) {
                stop(
                    paste(
                        'The second timepoint provided in therapyEffect is not a
                        valid timepoint.', tpAfter, 'is not a measured timepoint'
                    )
                )
            } else{
                therapyEffectTP <- (tpBefore + tpAfter) / 2
            }

            ##therapyEffect is a single timepoint where to estimate the therapy effect
        } else if (length(therapyEffect == 1)) {
            if (therapyEffect > max(timepoints) |
                    therapyEffect < min(timepoints)) {
                stop(
                    paste(
                        'The therapyEffect value must be between two measured
                        timepoints.', therapyEffect, 'is not a valid therapy
                        effect timepoint.'
                    )
                )
            } else {
                ##Timepoints before and after the therapy effect estimation tp
                tpBefore <- .getTpBeforeAfter(therapyEffect = therapyEffect,
                                            timepoints = timepoints)$tpBefore
                tpAfter <- .getTpBeforeAfter(therapyEffect = therapyEffect,
                                            timepoints = timepoints)$tpAfter
                therapyEffectTP <- therapyEffect
            }

        } else{
            stop(
                paste(
                    'therapyEffect must be either a vector with two values
                    (two measured timepoints) or a estimated therapy effect
                    timepoint', therapyEffect, 'is not a valid value'
                )
            )
        }


        ##Clones present in the timepoints before
        presentClones <-
            .getClonesPresent(tpBefore = tpBefore,
                                fracTable = fracTable,
                                timepoints = timepoints)


        #CCFs of all clones at the therapy effect timepoint
        newCCF_vec <-
            .getTherapyCCF(
                tpBefore = tpBefore,
                tpAfter = tpAfter,
                fracTable = fracTable,
                parents = parents,
                presentClones = presentClones,
                timepoints = timepoints
            )

        ##Update timepoints and frac table
        newTimepoints <-
            insert(timepoints,
                        ats = which(timepoints == tpAfter),
                        values = therapyEffectTP)

        newFracTable <-
            as.data.frame(fracTable) %>% add_column(newCCF_vec,
                            .before = which(timepoints == tpAfter))

        newFracTable <- as.matrix(newFracTable)
        colnames(newFracTable) <- newTimepoints

        return(newFracTable)
    }




#' When therapy effect and timepoint estimation happen between the same measured
#' timepoints,
#' recalculate the estimated timepoints so there is equidistant development
#'
#' @param therapyEffect A numeric value specifiying the timepoint on which to
#' estimate the therapy effect
#'     or a numeric vector with length=2 specifying the consecutive measured
#'     timepoints between which to estimate
#'     the therapy effect.
#' @param timepoints A numeric vector specifying the timepoints for each column
#' of the matrix
#'
#' @return A numeric vector with the measured and estimated timepoints

.recalculateTimepoints <- function(therapyEffect, timepoints) {
    ##only if therapy effect is given as two measured timepoints
    if (length(therapyEffect) == 2) {
        tpIndex1 <- which(round(timepoints, 2) == therapyEffect[1])
        tpIndex2 <- which(round(timepoints, 2) == therapyEffect[2])
        intermediateTps <- tpIndex2 - tpIndex1

        if (intermediateTps > 2) {
            for (i in seq(tpIndex1 + 1, tpIndex2 - 1)) {
                timepoints[i] <- NA
            }

            timepoints <- round(.interpolate(timepoints), 2)
        }
    }
    return(timepoints)
}



#### SHARK + DOLPHIN ##########


#' get the shark plot layout of all non-independent clones
#'
#' @param seaObject A sea object
#'
#' @return plot with all relations showed as grey lines and white clones
#' @seealso sharkPlot
#'
.basicSharkPlot <- function(seaObject) {
    from <- c()
    to <- c()
    clones <- rownames(seaObject@fracTable)
    for (i in seq_along(seaObject@parents)) {
        parent <- seaObject@parents[i]
        if (parent != 0) {
            from <- c(from, clones[parent])
            to <- c(to, clones[i])
        }
    }

    ## create an edge list data frame giving the hierarchical structure of
    ##the clones
    edges <- data.frame(from, to)

    ## Create a graph object
    mygraph <- graph_from_data_frame(edges)

    ##create the interactive plot
    p <- ggraph(mygraph, layout = 'tree', circular = FALSE,) +
        geom_edge_elbow(color = 'light grey',
                                        edge_width = 2,
                                        strength = 0.8) +
        geom_point(
            size = 8,
            mapping = aes(x = x, y = y),
            show.legend = FALSE,
            color =    'white'
        ) +
        theme_graph() + scale_y_reverse(expand = c(0.25, 0.25)) +
        coord_flip() +
        scale_x_reverse(expand = c(0.25, 0.25))

    return(p)
}




#' get a list divided with clones divided by families.
#'
#' @param seaObject A sea object
#'
#' @return list with clones divided by families, first half of the list is
#' parent clones, second half are children clones.
#'
.getFromTo <- function(seaObject) {
    p <- seaObject@parents
    c <- rownames(seaObject@fracTable)
    numTrees <- sum(p == 0)
    from <- c()
    to <- c()
    started <- FALSE #flag
    l_from <- list() #list with from vectors
    l_to <- list() #list with to vectors
    tree <- 1

    while (tree <= numTrees) {
        for (i in seq(1, length(p))) {
            #iterate along parents indexes

            parent <- p[i]

            ##when we reach a clone with parent 0 and we already started a
            ##family save the vectors to the list and reset
            if (parent == 0 & started) {
                l_from[[tree]] <- from
                l_to[[tree]] <- to
                tree <- tree + 1
                from <- c()
                to <- c()
                started <- FALSE
            }

            ##When we reach a clone with parent 0 start + add 0 (from) and
            ##parent clone (to)
            if (parent == 0 & !(started)) {
                started <- TRUE
                from <- c(from, 0)
                to <- c(to, c[i])
            }

            ##Add clones to from and to vectors while the lists are empty
            if (parent != 0 & started & length(l_from) == 0) {
                from <- c(from, c[parent])
                to <- c(to, c[i])
            }

            ##if lists are not empty
            if (parent != 0 & started & length(l_from) > 0) {
                ##idx is the position of the family where the clone belongs
                idx <- detect_index(l_to, ~ parent %in% .)

                ##if it belongs to a family --> append to the family
                if (idx != 0) {
                    l_from[[idx]] <- c(l_from[[idx]], c[parent])
                    l_to[[idx]] <- c(l_to[[idx]], c[i])
                }

                ##if it doesn't belong to any known family add to the new one
                if (idx == 0) {
                    from <- c(from, c[parent])
                    to <- c(to, c[i])
                }
            }
        }

        ##when we are done iterating add the remainings to the lists
        l_from[[tree]] <- from
        l_to[[tree]] <- to
        tree <- tree + 1

    }
    return(c(l_from, l_to))
}



#' get the independent clones labels and the count of how many independent
#' clones are there.
#'
#' @param numTrees An integer indicating how many families of clones are there
#' @param l_to A list with children clones divided by families.
#'
#' @return list with the independent clones labels and the count of how many
#' independent clones are there.
#' @seealso sharkPlot
.getIndependentCloneLabels <- function(numTrees, l_to) {
    count <- 0
    indep_clone_labs <- c()
    for (g in seq_len(numTrees)) {
        grp <- l_to[[g]]
        if (length(grp) == 1) {
            count <- count + 1
            indep_clone_labs <- c(indep_clone_labs, grp)
        }
    }
    returnList <-
        list('cloneLabs' = indep_clone_labs, 'count' = count)

    return(returnList)
}



#' get x and y positions for plotting independent clones
#'
#' @param count An integer indicating how many independent clones are there
#' @param parentTbl data frame containing x, y and clone label of the
#' non-independent clones
#' @param intPlot basic plot without the independent clones
#'
#' @return list with the independent clones x and y positions
#' @seealso sharkPlot
.getIndependentCloneXY <- function(count, parentTbl, intPlot) {
    indep_clone_ypos <- c()
    indep_clone_xpos <- c()
    for (c in seq_len(count)) {
        if (nrow(parentTbl) == 1) {
            min_xpos <- min(c(intPlot$data$x, indep_clone_xpos))
            indep_clone_xpos <- c(indep_clone_xpos, min_xpos - 1)
        }
        else{
            min_xpos <- min(c(intPlot$data$x, indep_clone_xpos))
            xpos <- min_xpos -
                (tail(parentTbl$x, 1) - abs(tail(parentTbl$x, 2)[1]))
            indep_clone_xpos <- c(indep_clone_xpos, xpos)

        }
    }
    indep_clone_ypos <- rep.int(max(intPlot$data$y), count)

    returnList <- list('x' = indep_clone_xpos, 'y' = indep_clone_ypos)

    return(returnList)
}




#' shark plot when all clones are independent
#'
#' @param cloneLabels Independent clones labels
#' @param ccf Vector with the clones CCFs at a specific timepoint
#' @param showLegend Boolean - whether to show the legend or not
#' @param cloneCols Vector with each clone colors
#'
#'
#' @return basic shark plot with all independent clones
#' @examples
#' \donttest {
#' .indepClonesPlot(cloneLabels = c('A','B','C'), showLegend = T,
#'                  cloneCols = c("#B0D8FF","#83B2D7","#588CAF"))
#' }
#' @seealso sharkPlot
.indepClonesPlot <-
    function(cloneLabels, showLegend = FALSE, cloneCols) {
        name <- as.factor(cloneLabels)
        x <- rep(0, length(cloneLabels))
        y <- seq(length(cloneLabels)) - 1
        data <- data.frame(name, x, y)

        p <- ggplot(data) +
            geom_point_interactive(
                size = 10,
                mapping = aes(
                    x = x,
                    y = y,
                    color = name,
                    tooltip = name,
                    data_id = name
                ),
                show.legend = showLegend
            ) +
            scale_colour_manual(values = cloneCols,
                                name = "Clone labels") + theme_void()

        return(p)
    }



###DOLPHIN

#' Generate key points of the layout that will be used for plotting with center
#' visualization
#'
#' @param seaObject A sea object with appropriate slots filled
#' (fracTable, parents, nestLevels)
#' @param separateIndependentClones Boolean - Should independently-arising
#' clones (with parent 0) be separated by blank space in the plot?
#'
#' @return A sea object with layout slots filled in
#'
#' @examples
#' \dontrun{
#' .layoutSeaObjectClones(seaObject)
#'
#' .layoutSeaObjectClones(seaObject, separateIndependentClones = TRUE)
#'
#' }
#'
.layoutSeaObjectClones <- function(seaObject, separateIndependentClones) {
        innerSpace <- lapply(rownames(seaObject@fracTable), .getInnerSpace,
                            seaObject)
        outerSpace <- .getOuterSpace(seaObject)

        ytop.vec <- c()
        ybtm.vec <- c()
        xpos.vec <- c()

        ##for each timepoint
        for (timepos in seq_len(length(seaObject@timepoints))) {
            timepoint <- seaObject@timepoints[timepos]

            ytop <- rep(NA, length(seaObject@parents))
            ybtm <- rep(NA, length(seaObject@parents))
            xpos <- rep(timepoint, length(seaObject@parents))

            ##starting with those with no parents, then moving through each
            ##existing parent
            parentsList <- 0
            while (length(parentsList) > 0) {
                parent <- parentsList[[1]]
                children <- which(seaObject@parents == parent)
                parentsList <- parentsList[-1]
                parentsList <- c(parentsList, children)
                numChildren <- length(children)
                spacing <- 0
                ##start at the bottom plus half the outer space
                y <- outerSpace[timepos] / 2


                ## (unless we are separating indpendent clones, in which case
                ## we divide outer spacing info in between and around
                if (separateIndependentClones & parent == 0) {
                    numZeros <- length(which(seaObject@parents == 0))
                    if (numZeros > 1 & outerSpace[timepos] > 0) {
                        spacing <- (outerSpace[timepos] / (numZeros + 1)) * 0.05
                    }

                }
                if (parent != 0) {
                    ##consider inner spacing if this is a subclone
                    y <- ybtm[parent]
                    spacing <- innerSpace[[parent]][timepos] /
                                            (.getNumChildren(seaObject, parent,
                                                            timepos) + 1)
                }

                ##for each clone that has this parent, get coords
                for (clone in children) {
                    ##clone absent, don't need to add positions
                    if (seaObject@fracTable[clone, timepos] == 0) {
                        xpos[clone] <- NA
                        ##smooth ending to dying clones
                        if (timepos > 1) {
                            if (seaObject@fracTable[clone, timepos - 1] > 0) {
                                ybtm[clone] <- y + spacing / 2
                                ytop[clone] <- y + spacing / 2
                                xpos[clone] <- timepoint - 0.25
                            }
                        }
                    } else {
                        #clone is still here, deal with it
                        ybtm[clone] <- y + spacing
                        y <- y + seaObject@fracTable[clone, timepos]
                        ytop[clone] <- y + spacing
                        y <- y + spacing
                    }
                }
            }
            ybtm.vec <- c(ybtm.vec, ybtm)
            ytop.vec <- c(ytop.vec, ytop)
            xpos.vec <- c(xpos.vec, xpos)
        }

        ##turn coords into a matrix so that we go by clone instead of by
        ##timepoint
        ybtm <- matrix(ybtm.vec, ncol = ncol(seaObject@fracTable))
        ytop <- matrix(ytop.vec, ncol = ncol(seaObject@fracTable))
        xpos <- matrix(xpos.vec, ncol = ncol(seaObject@fracTable))

        ybtm.list <- list()
        ytop.list <- list()
        xpos.list <- list()

        ##now, split into lists per clone
        for (i in seq_len(nrow(seaObject@fracTable))) {
            ybtm.list[[i]] <- ybtm[i, !is.na(ybtm[i, ])]
            ytop.list[[i]] <- ytop[i, !is.na(ytop[i, ])]
            xpos.list[[i]] <- xpos[i, !is.na(xpos[i, ])]
        }

        seaObject@ybtm <- ybtm.list
        seaObject@ytop <- ytop.list
        seaObject@xpos <- xpos.list

        return(seaObject)
    }



#' Given a sea object generate key points of the layout that will be used for
#' plotting with bottom visualization
#'
#' @param seaObject A sea object with appropriate slots filled
#' (fracTable, parents, nestLevels)
#' @param separateIndependentClones Boolean - Should independently-arising
#' clones (with parent 0) be separated by blank space in the plot?
#'
#' @return A sea object with layout slots filled in
#'
#' @examples
#' \dontrun{
#' .bottomLayout(seaObject)
#'
#' .bottomLayout(seaObject, separateIndependentClones = TRUE)
#' }
.bottomLayout <- function(seaObject, separateIndependentClones = FALSE) {
        cloneFam <- seaObject@cloneFamily ##vector with each clones family
        cloneBranches <- .getAllBranches(seaObject@parents)
        clonesOrder <- .getClonesOrder(
            nestLevels <- seaObject@nestLevels,
            parents <- seaObject@parents,
            cloneFam <- cloneFam
        )$clonesOrder
        deepestClone <- .getClonesOrder(
            nestLevels <- seaObject@nestLevels,
            parents <- seaObject@parents,
            cloneFam <- cloneFam
        )$deepestClone

        innerSpace <- lapply(rownames(seaObject@fracTable), .getInnerSpace,
                            seaObject)
        outerSpace <- .getOuterSpace(seaObject)

        ytop.vec <- c()
        ybtm.vec <- c()
        xpos.vec <- c()

        ##for each timepoint
        for (timepos in seq_len(length(seaObject@timepoints))) {
            timepoint <- seaObject@timepoints[timepos]

            ytop <- rep(NA, length(seaObject@parents))
            ybtm <- rep(NA, length(seaObject@parents))
            xpos <- rep(timepoint, length(seaObject@parents))

            for (parent in clonesOrder) {
                if (parent == 0) {
                    parentClones <- which(seaObject@parents == 0)
                    children <-
                        c(clonesOrder[2],
                        parentClones[-which(parentClones == clonesOrder[2])])
                } else{
                    children <-
                        clonesOrder[sort(match(which(seaObject@parents == parent),
                                    clonesOrder))]
                }
                spacing <- 0
                ##start at the bottom
                y <- 0

                if (parent != 0) {
                    ##consider inner spacing if this is a subclone
                    y <- ybtm[parent]
                    spacing <- innerSpace[[parent]][timepos] /
                                        (.getNumChildren(seaObject, parent,
                                                        timepos) + 1)
                }

                ##for each clone that has this parent, get coords
                for (clone in children) {
                    ##clone absent, don't need to add positions
                    if (seaObject@fracTable[clone, timepos] == 0) {
                        xpos[clone] <- NA
                        ##smooth ending to dying clones
                        if (timepos > 1) {
                            if (seaObject@fracTable[clone, timepos - 1] > 0) {
                                if (clone %in% cloneBranches[[deepestClone]]) {
                                    ybtm[clone] <- y
                                    ytop[clone] <- y + spacing / 2
                                    xpos[clone] <- timepoint - 0.25
                                } else{
                                    ybtm[clone] <- y + spacing / 2
                                    ytop[clone] <- y + spacing / 2
                                    xpos[clone] <- timepoint - 0.25
                                }
                            }
                        }
                    } else {
                        #clone is still here, deal with it
                        #Bottom family --> bottom visualization
                        if (separateIndependentClones & parent == 0) {
                            spacing <- outerSpace[timepos]
                            ybtm[clone] <- y
                            y <- y + seaObject@fracTable[clone, timepos]
                            ytop[clone] <- y
                            y <- y + spacing * 0.05
                        } else{
                            if (clone %in% cloneBranches[[deepestClone]]) {
                                ybtm[clone] <- y
                                y <- y + seaObject@fracTable[clone, timepos]
                                ytop[clone] <- y
                                y <- y + spacing
                            } else{
                                ##other families --> normal viz
                                ybtm[clone] <- y + spacing
                                y <- y + seaObject@fracTable[clone, timepos]
                                ytop[clone] <- y + spacing
                                y <- y + spacing
                            }
                        }


                    }
                }
            }
            ybtm.vec <- c(ybtm.vec, ybtm)
            ytop.vec <- c(ytop.vec, ytop)
            xpos.vec <- c(xpos.vec, xpos)
        }


        ##turn coords into a matrix so that we go by clone instead of by
        ##timepoint
        ybtm <- matrix(ybtm.vec, ncol = ncol(seaObject@fracTable))
        ytop <- matrix(ytop.vec, ncol = ncol(seaObject@fracTable))
        xpos <- matrix(xpos.vec, ncol = ncol(seaObject@fracTable))


        ybtm.list <- list()
        ytop.list <- list()
        xpos.list <- list()

        ##now, split into lists per clone
        for (i in clonesOrder[2:length(clonesOrder)]) {
            ybtm.list[[i]] <- ybtm[i, !is.na(ybtm[i, ])]
            ytop.list[[i]] <- ytop[i, !is.na(ytop[i, ])]
            xpos.list[[i]] <- xpos[i, !is.na(xpos[i, ])]
        }

        seaObject@ybtm <- ybtm.list
        seaObject@ytop <- ytop.list
        seaObject@xpos <- xpos.list

        return(seaObject)
    }




#' get the number of non-zero children at a particular timepoint
#'
#' @param seaObject A sea object
#' @param clone An integer representing the clone number to check
#' @param timepoint The timepoint at which to check
#'
#' @return the number of children with non-zero fractions
#'
.getNumChildren <- function(seaObject, clone, timepoint) {
    if (clone == 0) {
        return(0)
    }
    return(length(which(seaObject@fracTable[which(seaObject@parents == clone), timepoint] >
                                                0)))
}


#' Get the percentage of a clone that is only that clone and not occupied by
#' subclones
#'
#' @param clone The number of the clone to check
#' @param seaObject A fish object
#'
#' @return A number giving the percentage of this clone that is uniquely this
#' clone
.getInnerSpace <- function(clone, seaObject) {
    total <- seaObject@fracTable[as.numeric(clone), ]
    for (i in which(seaObject@parents == clone)) {
        total <- total - seaObject@fracTable[i, ]
    }
    return(total)
}

#' Get the amount of non-tumor space outside of all clones
#'
#' @param seaObject A sea object
#'
#' @return A numeric vector representing non-tumor space at each timepoint
#'
.getOuterSpace <- function(seaObject) {
    ##return the sums of all clones with parents of 0 at each timepoint
    z <- seaObject@fracTable[which(seaObject@parents == 0), ]
    if (is.vector(z)) {
        #only one row, just return it
        return(100 - z) ##Max minus parent clones CCFs
    }
    return(100 - colSums(z))
}




#' Get X and Y coordinates for polygon shape for all clones
#'
#' @param seaObject A sea object
#'
#' @return A data frame with the columns id, name (= clone Label), x and y

.getCoordsTable <- function(seaObject) {
    nclones <- nrow(seaObject@fracTable)
    y <- c()
    id <- c()
    x <- c()
    name <- c()
    for (clone in seq_len(nclones)) {
        cloneY <- c(seaObject@ytop[[clone]], rev(seaObject@ybtm[[clone]]))
        id <- c(id, rep(clone, length(cloneY)))
        x <- c(x, seaObject@xpos[[clone]], rev(seaObject@xpos[[clone]]))
        y <- c(y, cloneY)
        name <-
            c(name, rep(seaObject@cloneLabels[clone], length(cloneY)))
    }


    coordsTbl <- data.frame(
        id = as.factor(id),
        name = factor(name, levels = seaObject@cloneLabels),
        x = x,
        y = y
    )

    return(coordsTbl)
}





#' Get X and Y coordinates for spline shape for one clone
#'
#' @param xpos A vector of x values for control points
#' @param ytop A vector of y values for control points on the top
#' @param ybtm A vector of y values for control points on the bottom
#' @param pad.left A numeric amount of extra padding to add to the left side
#' of the shape
#' @param cloneLab A character value corresponding to the clone label
#'
#' @return A data frame with the columns name (= clone Label), x and y
#' @examples
#' \dontrun{
#' .getCloneCoords(xpos=c(0,30,75,150), ytop=c(100,51,51,99), ybtm=c(0,49,49,1),
#' cloneLab='Clone1',)
#' }
.getCloneCoords <- function(xpos, ytop, ybtm, pad.left = 0, cloneLab) {
    range <- max(xpos) - min(xpos)
    flank <- range * 0.0001

    xpos <- c(rbind(xpos - flank * 2, xpos - flank, xpos, xpos + flank, xpos +
                    flank * 2))
    #extra control points (x axis) around measured timepoint

    ybtm <- c(rbind(ybtm, ybtm, ybtm, ybtm, ybtm))
    ytop <- c(rbind(ytop, ytop, ytop, ytop, ytop))

    #Starting point (x,y)
    xst <- xpos[1] - pad.left
    yst <- (ytop[1] + ybtm[1]) / 2

    xst <- c(xst - flank * 2, xst, xst + flank * 2)
    yst <- c(yst, yst, yst)

    #top line
    top <- spline(c(xst, xpos), c(yst, ytop), n = 70)
    btm <- spline(c(xst, xpos), c(yst, ybtm), n = 70)
    #x and y coordinates for bottom line

    cloneName_vec <- rep(cloneLab, length(c(top$x, rev(btm$x))))
    coordsTbl <- data.frame(name = cloneName_vec,
                            x = c(top$x, rev(btm$x)),
                            y = c(top$y, rev(btm$y)))

    return(coordsTbl)
}




#' Get X and Y coordinates for spline shape for all clones
#'
#' @param seaObject A sea object
#' @param pad.left A numeric amount of extra padding to add to the left side
#' of the shape
#'
#' @return A data frame with the columns name (= clone Label), x and y

.getSmoothCoordsTbl <- function(seaObject, pad.left) {
    smoothCoords <- NULL
    pad <- (max(seaObject@timepoints) - min(seaObject@timepoints)) * pad.left

    for (clone in seq_len(length(seaObject@cloneLabels))) {
        cloneCoords <- .getCloneCoords(
            xpos = seaObject@xpos[[clone]],
            ytop = seaObject@ytop[[clone]],
            ybtm = seaObject@ybtm[[clone]],
            pad.left = pad,
            cloneLab = as.factor(seaObject@cloneLabels[clone])
        )

        smoothCoords <- rbind(smoothCoords, cloneCoords)
    }

    return(smoothCoords)
}




#### PLAICE PLOT ######


##Get branches from each clone to 0
#' Given the a list representing the parents of each clone, and the number
#' specifying which clone to test,
#' returns a vector with all its ancestral clones from the same branch
#'
#' @param parents An integer vector specifying parental relationships between
#' clones
#' @param x The integer specifying which subclone to get the branch of
#'
#' @return An integer vector with all the clone's ancestral clones from the
#' same branch
#' @seealso .getAllBranches
#'
.getBranch <- function(parents, x) {
    #sanity checks
    if (x > length(parents)) {
        stop(
            paste(
                "cannot have a parent that does not exist in list. parent =",
                x,
                ", length(parents) =",
                length(parents)
            )
        )
    }
    if (x < 0) {
        stop("cannot have a value in parents of less than zero")
    }

    if (parents[x] == 0) {
        return(c(0)) #return initial clone (normal cell)
    } else {
        ##return clone's parent and compute again
        return(c(.getBranch(parents, parents[x]), parents[x]))
    }
}



#' Given the a vector representing the parents of each clone, return a list of
#' vectors specifying which clones are in each clone's branch
#'
#' @param parents An integer vector specifying parental relationships between
#' clones
#'
#' @return List of vectors specifying which clones are in each clone's branch
#' @seealso .getNestLevel
#'
.getAllBranches <- function(parents) {
    branch <- list()
    for (i in seq_len(length(parents))) {
        branch[[i]] <- c(.getBranch(parents, i), i)
    }
    return(branch)
}



#' Get an integer vector specifying the order which to get each clone's x and
#' y positions
#' for the bottom visualization, so that the "longest" family is on the bottom.
#'
#' @param nestLevels An integer vector specifying how deeply a given clone is
#' nested in the overall hierarchy
#' @param parents An integer vector specifying parental relationships between
#' clones
#' @param cloneFam An integer vector specifying which family each clone belongs
#' to
#'
#' @return Vector with clone's order of layout
.getClonesOrder <- function(nestLevels, parents, cloneFam) {
    longestBranches <-
        which(nestLevels == max(nestLevels)) #deepest clones
    f <- table(cloneFam)[cloneFam[longestBranches]]
    ##choose the family with less dependent branched clones (more linear)
    if (length(unique(f)) == 1) {
        longestFamily <- as.numeric(names(f))[1]
        #deepest clone in the selected fam
        lastClone <-
            longestBranches[which(cloneFam[longestBranches] == longestFamily)][1]
    } else{
        longestFamily <- as.numeric(names(f[f == min(f)]))
        #deepest clone in the selected fam
        lastClone <-
            longestBranches[which(cloneFam[longestBranches] == longestFamily)]
    }


    subBranches <- .getAllBranches(parents)

    clonesOrder <- subBranches[[lastClone]]
    parentClones <- which(parents == 0)
    if (length(parentClones) > 1) {
        clonesOrder <-
            clonesOrder %>% insert(parentClones[-which(parentClones %in% clonesOrder)], ats = 3)
    }

    for (i in seq_len(length(parents))) {
        p <- clonesOrder[i]
        if (p != 0) {
            children <- which(parents == p)
            clonesOrder <- c(clonesOrder, children)
            if (length(children) == 0) {
                clonesOrder <- c(clonesOrder, parentClones)
            }
        }
        clonesOrder <- clonesOrder[!duplicated(clonesOrder)]

    }
    return(list(clonesOrder = clonesOrder, deepestClone = lastClone))
}






#' Given a sea object generate key points of the layout that will be used for
#' plotting with top visualization
#'
#' @param seaObject A sea object with appropriate slots filled
#' (fracTable, parents, nestLevels)
#' @param separateIndependentClones Boolean - Should independently-arising
#' clones (with parent 0) be separated by blank space in the plot?
#'
#' @return A sea object with layout slots filled in
#'
#' @examples
#' \dontrun{
#' .topLayout(seaObject)
#'
#' .topLayout(seaObject, separateIndependentClones = TRUE)
#' }
.topLayout <-
    function(seaObject, separateIndependentClones = FALSE) {
        cloneFam <- seaObject@cloneFamily ##vector with each clones family
        cloneBranches <- .getAllBranches(seaObject@parents)
        clonesOrder <- .getClonesOrder(
            nestLevels = seaObject@nestLevels,
            parents = seaObject@parents,
            cloneFam = cloneFam
        )$clonesOrder
        deepestClone <- .getClonesOrder(
            nestLevels = seaObject@nestLevels,
            parents = seaObject@parents,
            cloneFam = cloneFam
        )$deepestClone

        innerSpace <- lapply(rownames(seaObject@fracTable), .getInnerSpace,
                                    seaObject)
        outerSpace <- .getOuterSpace(seaObject)

        ytop.vec <- c()
        ybtm.vec <- c()
        xpos.vec <- c()

        ##for each timepoint
        for (timepos in seq_len(length(seaObject@timepoints))) {
            timepoint <- seaObject@timepoints[timepos]

            ytop <- rep(NA, length(seaObject@parents))
            ybtm <- rep(NA, length(seaObject@parents))
            xpos <- rep(timepoint, length(seaObject@parents))

            for (parent in clonesOrder) {
                if (parent == 0) {
                    parentClones <- which(seaObject@parents == 0)
                    children <-
                        c(clonesOrder[2],
                        parentClones[-which(parentClones == clonesOrder[2])])
                } else{
                    children <-
                        clonesOrder[sort(match(which(seaObject@parents == parent),
                                        clonesOrder))]
                }
                spacing <- 0
                ##start at the bottom
                y <- 0

                if (parent != 0) {
                    ##consider inner spacing if this is a subclone
                    y <- ybtm[parent]
                    spacing <- innerSpace[[parent]][timepos] /
                                            (.getNumChildren(seaObject, parent,
                                                            timepos) + 1)
                }

                ##for each clone that has this parent, get coords
                for (clone in children) {
                    ##clone absent, don't need to add positions
                    if (seaObject@fracTable[clone, timepos] == 0) {
                        xpos[clone] <- NA
                        ##smooth ending to dying clones
                        if (timepos > 1) {
                            if (seaObject@fracTable[clone, timepos - 1] > 0) {
                                if (clone %in% cloneBranches[[deepestClone]]) {
                                    ybtm[clone] <- y
                                    ytop[clone] <- y - spacing / 2
                                    xpos[clone] <- timepoint - 0.25
                                } else{
                                    ybtm[clone] <- y - spacing / 2
                                    ytop[clone] <- y - spacing / 2
                                    xpos[clone] <- timepoint - 0.25
                                }
                            }
                        }
                    } else {
                        #clone is still here, deal with it
                        #Bottom family --> bottom visualization
                        if (separateIndependentClones & parent == 0) {
                            spacing <- outerSpace[timepos]
                            ybtm[clone] <- y
                            y <- y - seaObject@fracTable[clone, timepos]
                            ytop[clone] <- y
                            y <- y - spacing * 0.05
                        } else{
                            if (clone %in% cloneBranches[[deepestClone]]) {
                                ybtm[clone] <- y
                                y <- y - seaObject@fracTable[clone, timepos]
                                ytop[clone] <- y
                                y <- y - spacing
                            } else{
                                ##other families --> normal viz
                                ybtm[clone] <- y - spacing
                                y <- y - seaObject@fracTable[clone, timepos]
                                ytop[clone] <- y - spacing
                                y <- y - spacing
                            }
                        }


                    }
                }
            }
            ybtm.vec <- c(ybtm.vec, ybtm)
            ytop.vec <- c(ytop.vec, ytop)
            xpos.vec <- c(xpos.vec, xpos)
        }


        ##turn coords into a matrix so that we go by clone instead of by
        ##timepoint
        ybtm <- matrix(ybtm.vec, ncol = ncol(seaObject@fracTable))
        ytop <- matrix(ytop.vec, ncol = ncol(seaObject@fracTable))
        xpos <- matrix(xpos.vec, ncol = ncol(seaObject@fracTable))


        ybtm.list <- list()
        ytop.list <- list()
        xpos.list <- list()

        ##now, split into lists per clone
        for (i in clonesOrder[2:length(clonesOrder)]) {
            ybtm.list[[i]] <- ybtm[i, !is.na(ybtm[i, ])]
            ytop.list[[i]] <- ytop[i, !is.na(ytop[i, ])]
            xpos.list[[i]] <- xpos[i, !is.na(xpos[i, ])]
        }

        seaObject@ybtm <- ybtm.list
        seaObject@ytop <- ytop.list
        seaObject@xpos <- xpos.list

        return(seaObject)
    }






##Get colors for the allele visualization (bottom half)
#' Given a sea object and an integer vector with which clone's color to fill
#' each clone return a vector with each clones colors for the bottom half of
#' plaice plot
#'
#' @param seaObject A sea object with appropriate slots filled
#' (fracTable, parents, nestLevels)
#' @param clonesToFill An integer vector with the index of the clone's color to
#' fill each clone. For example:
#' clonesToFill <- c(0,0,0,2,0,0) clone 4 (and its children) will be filled
#' with clone's 2 color
#'
#' @return A vector with the clones colors
#'
.getBottomCloneCols <- function(seaObject, clonesToFill) {
    btmCloneColors <- rep('white', nrow(seaObject@fracTable))
    branches <- .getAllBranches(seaObject@parents)
    for (clone in seq_len(length(clonesToFill))) {
        if (clonesToFill[clone] != 0) {
            children <- c()
            for (branch in branches) {
                if (clone %in% branch) {
                    children <- c(children, branch[2:length(branch)])
                    children <-
                        children[which(seaObject@nestLevels[children] > seaObject@nestLevels[clone])]
                }
            }
            children <-
                unique(children) #paint clone desired and "children"
            btmCloneColors[clone] <- seaObject@col[clonesToFill[clone]]
            for (child in children) {
                if (clonesToFill[child] == 0) {
                    btmCloneColors[child] <- seaObject@col[clonesToFill[clone]]
                }
            }
        }
    }
    return(btmCloneColors)
}
