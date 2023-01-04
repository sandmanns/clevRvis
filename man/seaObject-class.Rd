\name{seaObject-class}
\alias{seaObject-class}
\title{
Class seaObject
}
\description{
Represents a seaObject class, containing all necessary input to generate shark
plots, dolphin plots and plaice plots
}
\details{
In the code snippets below, x is a seaObject.

show(x) : summary of the seaObject

ytop(x) : get value of ytop

ytop(x)<- : assign value to ytop

ybtm(x) : get value of ybtm

ybtm(x)<- : assign value to ybtm

xpos(x) : get value of xpos

xpos(x)<- : assign value to xpos

col(x) : get value of col

col(x)<- : assign value to col

timepoints(x) : get value of timepoints

timepoints(x)<- : assign value to timepoints

fracTable(x) : get value of fracTable

fracTable(x)<- : assign value to fracTable

parents(x) : get value of parents

parents(x)<- : assign value to parents

nestLevels(x) : get value of nestLevels

nestLevels(x)<- : assign value to nestLevels

cloneFamily(x) : get value of cloneFamily

cloneFamily(x)<- : assign value to cloneFamily

cloneLabels(x) : get value of cloneLabels

cloneLabels(x)<- : assign value to cloneLabels

defaultLabels(x) : get value of defaultLabels

defaultLabels(x)<- : assign value to defaultLabels

originTimepoint(x) : get value of originTimepoint

originTimepoint(x)<- : assign value to originTimepoint

}

\author{
Sarah Sandmann <sarah.sandmann@uni-muenster.de>
}
\examples{
timepoints <- c(0,30,75,150)
fracTable <- matrix(
            c( 100, 45, 00, 00,
                02, 00, 00, 00,
                03, 00, 02, 01,
                98, 00, 95, 40),
            ncol=length(timepoints))
parents <- c(0,1,1,3)
seaObject <- createSeaObject(fracTable = fracTable,
                            parents = parents,
                            timepoints = timepoints)
timepoints(seaObject)
timepoints(seaObject) <- c(0,20,75,150)
}

