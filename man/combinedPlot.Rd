\name{combinedPlot}
\alias{combinedPlot}
\title{
Generate a combined basic graph and detailed visualization of clonal evolution
}
\description{
Given a sea object containing layout information, a shark and dolphin plot can
be plotted together - linked and interactive.
}
\usage{
combinedPlot(seaObject, 
            shark = TRUE, 
            dolphin = TRUE, 
            shape = "spline", 
            borderCol = NULL, 
            vlines = NULL, 
            vlineCol = "#6E6E66", 
            vlab = NULL,
            vlabSize = 3, 
            pos = "center", 
            separateIndependentClones = FALSE, 
            showLegend = FALSE,
            markMeasuredTimepoints = NULL, 
            downloadWidget = NULL, 
            mainDph = NULL, 
            mainPosDph = "middle",
            mainSizeDph = 5, 
            mainShk = NULL, 
            xlab = NULL, 
            ylab = NULL, 
            pad.left = 0.005, 
            annotations = NULL, 
            width = 12,
            height = 9)
}
\arguments{
\item{seaObject}{
A seaObject.
}
\item{shark}{
A boolean defining whether or not to draw a shark plot (default: TRUE).
}
\item{dolphin}{
A boolean defining whether or not to draw a dolphin plot (default: TRUE).
}
\item{shape}{
The type of shape to construct the plot out of. The options are "spline" and
"polygon" (default: "spline"").
}
\item{borderCol}{
A color for the border line. If "NULL" then no border will be drawn (default:
NULL).
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
\item{pos}{
Plotting position of the clones. Options are "center", "bottom" or "top"
(default: "center").
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
\item{downloadWidget}{
File to safe HTML to (default: NULL).
}
\item{mainDph}{
A string corresponding to the dolphin plot's main title (default: NULL).
}
\item{mainPosDph}{
A string defining the dolphin plot's title position. Options are 'left',
'middle' or 'right', always above the plot (default: "middle").
}
\item{mainSizeDph}{
An integer value defining the size of the dolphin plot's title (default: 5).
}
\item{mainShk}{
A string corresponding to the shark plot's main title (default: NULL).
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
\item{width}{
An integer value indicating the with of the output widget (default: 12).
}
\item{height}{
An integer value indicating the height of the output widget (default: 9).
}
}
\details{
Dolphin plots may be chosen to be plotted along with basic shark plots (for
details see \code{\link{dolphinPlot}} and \code{\link{sharkPlot}}). Both plots
are internally connected. By hovering on one of the clones, it is automatically
highlighted in both, shark and dolphin plot.

Important note: extended shark plots and dolphin plots can NOT be visualized
together.
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

combinedPlot(seaObject, borderCol = 'white', showLegend = TRUE)
}
