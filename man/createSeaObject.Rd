\name{createSeaObject}
\alias{createSeaObject}
\title{
Create a seaObject
}
\description{
clevRvis needs a seaObject for the visualization of clonal evolution by means
of any available plot. When generating the seaObject, extra time points may be
interpolated, therapy effect may be estimated.
}
\usage{
createSeaObject(fracTable, 
                parents, 
                timepoints, 
                col = NULL, 
                cloneLabels = NULL,
                originTimepoint = NULL, 
                timepointInterpolation = TRUE, 
                therapyEffect = NULL)
}
\arguments{
\item{fracTable}{
A numeric matrix containing tumor fraction estimates for all clones at all
time points.
}
\item{parents}{
An integer vector specifying parental relationships between clones.
}
\item{timepoints}{
A numeric vector specifying the time points for each column of the matrix.
}
\item{col}{
A vector of colors to use when plotting each clone (default: NULL).
}
\item{cloneLabels}{
A character vector of names to assign to each clone when plotting a legend
(default: NULL).
}
\item{originTimepoint}{
Time point when the first clone emerges (must be before the first measured time
point) (default: NULL).
}
\item{timepointInterpolation}{
When set to true extra time points will be estimated between measured time
points and before the first measure time point to improve the visualization
(default: TRUE).
}
\item{therapyEffect}{
A single numeric value indicating the time point when to estimate the effect
of therapy or a numeric vector containing two consecutive measured time points,
therapy effect time point will be in the middle (default: NULL).
}
}
\details{
The basis for all plotting functions included in clevRvis is a seaObject. It
contains information on the CCFs for all clones, at all measured time points.
Additionally, parental information on the clones is included.

Additional time points may be interpolated, therapy effect may be estimated when
generating seaObjects.

Time point interpolation is generally recommended to improve visualization of
clonal evolution. When having less time points than clones, or many new clones
emerging in one single measured time point, the extra time point estimation is
strongly recommended to visualize the clonal evolution properly. If there is
only one measured time point, time point interpolation is required and the time
point of origin has to be specified manually, as there is no way of calculating
it.

To visualize the effect of therapy on the clones' CCFs in case of missing
measured data, a fully automatic approach for therapy effect estimation is
available. When creating the seaObject, a specific time point can be defined
(between two measured time points) or two measured time points can be selected
(new therapy effect time point will be in the middle) for the estimation of the
therapy effect.

}
\value{
A sea Object with all relevant slots filled.
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

}
