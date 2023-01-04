\name{clevRvis_package}
\alias{clevRvis_package}
\alias{clevRvis}
\docType{package}
\title{
\packageTitle{clevRvis}
}
\description{
clevRvis is a tool that allows you to visualize changes in the subclonal
architecture of tumors. Simple tree visualization (shark plot) a more detailed
visualization (dolphin plot) and an allele-aware visualization (plaice plot)
are available.
Moreover, this tool provides fully automatic algorithms for interpolating time 
points and estimating therapy effect in the presence of lacking input data.
}
\details{
The package contains a function performing the whole analysis using a shiny user
interface - \code{\link{clevRvisShiny}}.

Additionally, all functions for classical use in R are available:

1) \code{createSeaObject}: create the seaObject needed for further visualization
options

2) \code{sharkPlot}: basic graph visualization (shark plot) of clonal evolution.

3) \code{extSharkplot}: basic graph visualization with extension showing Cancer
Cell Fractions (CCFs) as point size for each clone at each selected time point.

4) \code{dolphinPlot}: detailed visualization (dolphin plot) of clonal evolution
showing the development of all clones over time and their clonal prevalences.

5) \code{combinedPlot}: interactive linked visualization of shark and dolphin plot
together.

6) \code{plaicePlot}: detailed visualization on the allelic level (plaice plot)
of clonal evolution. Clonal prevalences on the top half and percentage of
remaining healthy alleles on the bottom half.

7) \code{exploreTrees}: Generate alternative parental relations to explore
    alternative trees
}
\author{
Sarah Sandmann <sarah.sandmann@uni-muenster.de>
}

\seealso{
\code{\link{clevRvisShiny}}, \code{\link{createSeaObject}},
\code{\link{sharkPlot}}, \code{\link{extSharkPlot}}, \code{\link{dolphinPlot}},
\code{\link{combinedPlot}}, \code{\link{plaicePlot}}, \code{\link{exploreTrees}}
}
\examples{
timepoints <- c(0,50,100)
parents <- c(0,1,1,3,0,5,6)
fracTable <- matrix(
                    c(  20, 10,  0,  0,  0,  0,  0,
                        40, 20, 15,  0, 30, 10,  0,
                        50, 25, 15, 10, 40, 20, 15),
                    ncol = length(timepoints))

#Generating a seaObject with timepoint interpolation
seaObject <- createSeaObject(fracTable, parents, timepoints)


#Basic shark plot showing legend and title
#png('basicShark.png',height = 800, width=600)
sharkPlot(seaObject, showLegend = TRUE, main = 'Example Shark plot')
#dev.off()

#extended shark plot, showing CCF as point size only for measured timepoints,
#legend and title
#png('extendedShark.png',height = 700, width = 1500)
extSharkPlot(seaObject, timepoints = timepoints, showLegend = TRUE,
            main = 'Example Extended Shark plot')
#dev.off()

#Default dolphin plot, with vertical lines showing all time points, custom
#y axis label and triangles indicating the measured time points
#png('dolphinPlot.png',height = 800, width=1750)
dolphinPlot(seaObject, showLegend = TRUE, vlines = slot(seaObject,"timepoints"),
            vlab = slot(seaObject,"timepoints"), vlabSize = 2,
            ylab = 'Cancer cell fractions (CCFs)',
            markMeasuredTimepoints = timepoints)
#dev.off()

#Basic shark plot linked to dolphin plot
combinedPlot(seaObject, showLegend = TRUE, vlines = timepoints,
            vlab = timepoints, vlabSize = 2, ylab = 'Cancer cell fraction',
            separateIndependentClones = TRUE)

#static plaice plot showing biallelic events + annotations
annotsTable <- data.frame(x = c(24,55), y = c(-40,-5),
                        col = c('black', 'white'), lab = c('TP53', 'UBA1'))
#png('plaicePlot.png',height = 800, width = 1750)
plaicePlot(seaObject, showLegend = TRUE,  vlines = timepoints,
            vlab = timepoints, vlabSize = 4, ylab = TRUE,
            separateIndependentClones = TRUE, clonesToFill = c(0,0,1,0,0,6,0),
            annotations = annotsTable, interactivePlot = FALSE)
#dev.off()

#seaObject with enabled timepoint interpolation and therapy effect estimation
#between timepoint 50 and 100
seaObject <- createSeaObject(fracTable, parents, timepoints,
                            timepointInterpolation = TRUE,
                            therapyEffect = c(50,100))

#Default dolphin plot showing estimated therapy effect, with vertical
#lines showing all time points, custom y axis label and triangles indicating
#the measured time points
#png('dolphinPlotTherapy.png',height = 800, width=1750)
dolphinPlot(seaObject, showLegend = TRUE, vlines = slot(seaObject,"timepoints"),
            vlab = slot(seaObject,"timepoints"), vlabSize = 2,
            ylab = 'Cancer cell fractions (CCFs)',
            markMeasuredTimepoints = timepoints)
#dev.off()

#Explore alternative valid trees
timepoints <- c(0,30,75,150)
fracTable <- matrix(
            c( 100, 45,  0,  0,
                20,  0,  0,  0,
                30,  0, 20,  5,
                98,  0, 55, 40),
            ncol=length(timepoints))
trees <- exploreTrees(fracTable, timepoints)

}
