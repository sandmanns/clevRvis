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

methods::setGeneric("ytop", function(x) standardGeneric("ytop"))
methods::setGeneric("ytop<-", 
                    function(x, value) standardGeneric("ytop<-"))

methods::setMethod("ytop", "seaObject", function(x) x@ytop)
methods::setMethod("ytop<-", "seaObject", function(x, value) {
    x@ytop <- value
    x
})

methods::setGeneric("ybtm", function(x) standardGeneric("ybtm"))
methods::setGeneric("ybtm<-", 
                    function(x, value) standardGeneric("ybtm<-"))

methods::setMethod("ybtm", "seaObject", function(x) x@ybtm)
methods::setMethod("ybtm<-", "seaObject", function(x, value) {
    x@ybtm <- value
    x
})

methods::setGeneric("xpos", function(x) standardGeneric("xpos"))
methods::setGeneric("xpos<-", 
                    function(x, value) standardGeneric("xpos<-"))

methods::setMethod("xpos", "seaObject", function(x) x@xpos)
methods::setMethod("xpos<-", "seaObject", function(x, value) {
    x@xpos <- value
    x
})

methods::setGeneric("col", function(x) standardGeneric("col"))
methods::setGeneric("col<-", 
                    function(x, value) standardGeneric("col<-"))

methods::setMethod("col", "seaObject", function(x) x@col)
methods::setMethod("col<-", "seaObject", function(x, value) {
    x@col <- value
    x
})

methods::setGeneric("timepoints", function(x) standardGeneric("timepoints"))
methods::setGeneric("timepoints<-", 
                    function(x, value) standardGeneric("timepoints<-"))

methods::setMethod("timepoints", "seaObject", function(x) x@timepoints)
methods::setMethod("timepoints<-", "seaObject", function(x, value) {
    x@timepoints <- value
    x
})

methods::setGeneric("fracTable", function(x) standardGeneric("fracTable"))
methods::setGeneric("fracTable<-", 
                    function(x, value) standardGeneric("fracTable<-"))

methods::setMethod("fracTable", "seaObject", function(x) x@fracTable)
methods::setMethod("fracTable<-", "seaObject", function(x, value) {
    x@fracTable <- value
    x
})

methods::setGeneric("parents", function(x) standardGeneric("parents"))
methods::setGeneric("parents<-", 
                    function(x, value) standardGeneric("parents<-"))

methods::setMethod("parents", "seaObject", function(x) x@parents)
methods::setMethod("parents<-", "seaObject", function(x, value) {
    x@parents <- value
    x
})

methods::setGeneric("nestLevels", function(x) standardGeneric("nestLevels"))
methods::setGeneric("nestLevels<-", 
                    function(x, value) standardGeneric("nestLevels<-"))

methods::setMethod("nestLevels", "seaObject", function(x) x@nestLevels)
methods::setMethod("nestLevels<-", "seaObject", function(x, value) {
    x@nestLevels <- value
    x
})

methods::setGeneric("cloneFamily", function(x) standardGeneric("cloneFamily"))
methods::setGeneric("cloneFamily<-", 
                    function(x, value) standardGeneric("cloneFamily<-"))

methods::setMethod("cloneFamily", "seaObject", function(x) x@cloneFamily)
methods::setMethod("cloneFamily<-", "seaObject", function(x, value) {
    x@cloneFamily <- value
    x
})

methods::setGeneric("cloneLabels", function(x) standardGeneric("cloneLabels"))
methods::setGeneric("cloneLabels<-", 
                    function(x, value) standardGeneric("cloneLabels<-"))

methods::setMethod("cloneLabels", "seaObject", function(x) x@cloneLabels)
methods::setMethod("cloneLabels<-", "seaObject", function(x, value) {
    x@cloneLabels <- value
    x
})

methods::setGeneric("defaultLabels", function(x) standardGeneric("defaultLabels"))
methods::setGeneric("defaultLabels<-", 
                    function(x, value) standardGeneric("defaultLabels<-"))

methods::setMethod("defaultLabels", "seaObject", function(x) x@defaultLabels)
methods::setMethod("defaultLabels<-", "seaObject", function(x, value) {
    x@defaultLabels <- value
    x
})

methods::setGeneric("originTimepoint", function(x) standardGeneric("originTimepoint"))
methods::setGeneric("originTimepoint<-", 
                    function(x, value) standardGeneric("originTimepoint<-"))

methods::setMethod("originTimepoint", "seaObject", function(x) x@originTimepoint)
methods::setMethod("originTimepoint<-", "seaObject", function(x, value) {
    x@originTimepoint <- value
    x
})

methods::setMethod("show","seaObject",
                    function(object)print(list(ytop = object@ytop,
                                                ybtm = object@ybtm,
                                                xpos = object@xpos,
                                                col = object@col,
                                                timepoints = object@timepoints,
                                                fracTable = object@fracTable,
                                                parents = object@parents,
                                                nestLevels = object@nestLevels,
                                                cloneFamily = object@cloneFamily,
                                                cloneLabels = object@cloneLabels,
                                                defaultLabels = object@defaultLabels,
                                                originTimepoint = object@originTimepoint)))

