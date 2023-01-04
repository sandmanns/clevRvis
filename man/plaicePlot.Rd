\name{plaicePlot}
\alias{plaicePlot}
\title{
Generate an allele-aware visualization of clonal evolution
}
\description{
Plaice plots provide an allele-aware visualization of clonal evolution. Plots
show the development of all clones over time (x axis) and their clonal
prevalences (y axis), and the ratio of remaining healthy alleles (lower plaice).
}
\usage{
plaicePlot(seaObject, 
            shape = "spline", 
            borderCol = "black", 
            vlines = NULL, 
            vlineCol = "#6E6E66", 
            vlab = NULL, 
            vlabSize = 3,
            separateIndependentClones = FALSE, 
            clonesToFill = NULL, 
            showLegend = FALSE,
            markMeasuredTimepoints = NULL, 
            main = NULL, 
            mainPos = "middle", 
            mainSize = 5, 
            xlab = NULL, 
            ylab = FALSE,
            pad.left = 0.005, 
            annotations = NULL, 
            annotationsSize = 3, 
            interactivePlot = TRUE)
}
\arguments{
\item{seaObject}{
A seaObject.
}
\item{shape}{
The type of shape to construct the plot out of. The options are "spline" and
"polygon" (default: "spline"").
}
\item{borderCol}{
A color for the border line. If "NULL" then no border will be drawn (default:
"black").
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
\item{clonesToFill}{
An integer vector with the index of the clone's color to fill each clone. For
example: clonesToFill <- c(0,0,0,2,0,0) clone 4 (and its children) will be
filled with clone 2 color (default: NULL).
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
A boolean defining whether or not to show the default y axis labels (default:
FALSE).
}
\item{pad.left}{
The amount of "ramp-up" to the left of the first time point. Given as a fraction
of the total plot width (default: 0.005).
}
\item{annotations}{
A data.frame with: columns "x" (x position), "y" (y position), "lab" (annotation
text) and "col" (color of the text either black or white) (default: NULL).
}
\item{annotationsSize}{
An integer value defining the size of the annotations (default: 3).
}
\item{interactivePlot}{
A boolean defining whether the plot should be interactive (default: TRUE; if
using this function to export the plaice plot, e.g. by png(), define
interactivePlot = FALSE).
}

}
\details{
Plaice plots are based on the bottom visualization of dolphin plots ("flatfish"
= plaice), mirrored above and below the x-axis . They have been developed to
improve the visualization of biallelic events.

Clonal evolution can be visualized in the upper half of the plot. Several
options similar to the ones for dolphin plot are available as well for plaice
plot: switching between spline and polygon shape, annotations, separating
independent clones (recommended), adding vertical lines and labels, etc.

On the bottom half of the plot, a not-colored mirrored representation of clonal
evolution is plotted. The user may choose the clones and their color, in order
to show which clones caused biallelic events. When a clone is not colored, it
indicates that a healthy allele remains.

Variants affecting the only available X- or Y-chromosome in male subjects can
also be visualized using plaice plots.
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

plaicePlot(seaObject, shape = "spline",
            vlines = c(0,150), vlab = c("day 0","day 150"),
            main = 'Example plot', clonesToFill = c(0,1,0))
}
