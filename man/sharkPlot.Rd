\name{sharkPlot}
\alias{sharkPlot}
\title{
Generate a basic graph visualization of clonal evolution
}
\description{
A shark plot shows the basic graph visualization of clonal evolution with nodes
representing clones and edges indicating their evolutionary relations.
}
\usage{
sharkPlot(seaObject, showLegend, main)
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
}
\details{
A shark plot is the basic approach for visualization: common trees, with nodes
representing clones and edges indicating their evolutionary relation. Phylogeny
can be directly deduced from these plots.

Shark plots also offer an extension to visualize the changes in CCF along time
for each clone. CCFs of each clone (rows) at each time point (columns) are shown
as points next to the basic shark plot (see \code{\link{extSharkPlot}}).
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

sharkPlot(seaObject, showLegend = TRUE, main = 'Example Shark Plot')
}
