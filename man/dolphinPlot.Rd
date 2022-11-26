\name{dolphinPlot}
\alias{dolphinPlot}
\title{
Generate a detailed visualization of clonal evolution
}
\description{
Dolphin plots provide a detailed visualization of clonal evolution. Plots show
the development of all clones over time (x axis) and their clonal prevalences
(y axis).
}
\usage{
dolphinPlot(seaObject, shape, borderCol, pos, vlines, vlineCol, vlab, vlabSize,
            separateIndependentClones, showLegend, markMeasuredTimepoints,
            main, mainPos, mainSize, xlab, ylab, pad.left, annotations,
            annotSize)
}
\arguments{
\item{seaObject}{
A seaObject.
}
\item{shape}{
The type of shape to construct the plot out of. The options are "spline" and
"polygon" (default: "spline").
}
\item{borderCol}{
A color for the border line. If "NULL" then no border will be drawn (default:
NULL).
}
\item{pos}{
Plotting position of the clones. Options are "center" or "bottom" (default:
"center").
}
\item{vlines}{
A vector of positions at which to draw vertical lines (default: NULL).
}
\item{vlineCol}{
A color value for the vertical lines (default: "#6E6E66").
}
\item{vlab}{
A character vector containing labels for each of the vertical lines (default:
NULL).
}
\item{vlabSize}{
An integer value for the vertical labels size (default: 3).
}
\item{separateIndependentClones}{
Boolean defining whether independently-arising clones (with parent 0) should be
separated by blank space in the plot (default: FALSE).
}
\item{showLegend}{
A boolean indicating whether to show a legend at the left side of the plot
(default: FALSE).
}
\item{markMeasuredTimepoints}{
A vector of x positions at which to draw triangles on the bottom of the plot
(default: NULL).
}
\item{main}{
A string corresponding to the plot's main title (default: NULL).
}
\item{mainPos}{
A string defining the title's position. Options are 'left', 'middle' or 'right',
always above the plot (default: "middle").
}
\item{mainSize}{
An integer value defining the size of the title (default: 5).
}
\item{xlab}{
A string defining the label of the x axis (default: NULL).
}
\item{ylab}{
A string defining the label of the y axis. Automatically, a vertical line
showing 100\% will be plotted (default: NULL).
}
\item{pad.left}{
The amount of "ramp-up" to the left of the first time point. Given as a fraction
of the total plot width (default: 0.005).
}
\item{annotations}{
A data.frame with: columns "x" (x position), "y" (y position), "lab" (annotation
text) and "col" (color of the text either black or white) (default: NULL).
}
\item{annotSize}{
An integer value defining the size of the annotations (default: 3).
}

}
\details{
Dolphin plots displays detailed information on clonal evolution, showing the
development of all clones over time (x axis) and their clonal prevalence
(y axis). Information on phylogeny, CCFs and time course characterizing a clonal
evolution are jointly visualized in this single plot.

Several basic options for customizing dolphin plots are available, e.g.
switching between spline and polygon shape, bottom or central visualization,
annotations, separating independent clones, adding vertical lines and labels,
changing border and vertical lines colors, etc.

Dolphin plots may be chosen to be plotted along with basic shark plots (see
\code{\link{plotWidget}}).
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
                            timepoints = timepoints,
                            timepointInterpolation = TRUE)

dolphinPlot(seaObject, main = 'Example Dolphin Plot', pos = 'center')
}
