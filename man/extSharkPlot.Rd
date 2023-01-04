\name{extSharkPlot}
\alias{extSharkPlot}
\title{
Generate an extended graph visualization of clonal evolution
}
\description{
An extended shark plot shows the basic graph visualization of clonal evolution
with nodes representing clones and edges indicating their evolutionary relations
and additional visualization of CCFs.
}
\usage{
extSharkPlot(seaObject, 
            showLegend = FALSE, 
            main = NULL, 
            timepoints = NULL, 
            width = 10, 
            interactivePlot = TRUE)
}
\arguments{
\item{seaObject}{
A seaObject.
}
\item{showLegend}{
A boolean indicating whether to show the legend or not (default: FALSE).
}
\item{main}{
A string corresponding to the plot's main title (default: NULL).
}
\item{timepoints}{
By default, all time points available in the seaObject are visualized.
Optionally, a selected set of available time points can be chosen
(default: NULL).
}
\item{width}{
An integer value indicating the width of the widget plot (default: 10).
}
\item{interactivePlot}{
A boolean defining whether the plot should be interactive (default: TRUE; if
using this function to export the extended shark plot, e.g. by png(), define
interactivePlot = FALSE).
}
}
\details{
An extended shark plots consists of two elements:

1) A basic shark plot: common trees with nodes representing clones and edges
indicating their evolutionary relation. Phylogeny can be directly deduced from
these plots.

2) Additionally, CCFs of each clone (rows) at each time point (columns) are
shown as points next to the basic shark plot. The size of each point correlates
with the CCF at the corresponding clone and time point.

Both plots are linked in an interactive widget.
}
\value{
None
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

extSharkPlot(seaObject, showLegend = TRUE, main = 'Example Shark Plot')
}
